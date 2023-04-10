{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Common (FragmentationIndicator (..), consumeAll)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Binary (Binary, Get, Word16, Word32, Word64, get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Internal (ByteString (Empty))
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Lib
import Message (ControlMessage, ControlMessages (..))
import Streamly.Data.Stream (Stream, mapMaybe, unfoldrM)
import qualified Streamly.Data.Stream as S (foldr, foldrM, mapM, take, toList)
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Stream (Step (..), Stream (Stream))
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Show.Pretty (pPrint)

data ParseState = ParseState ByteString Int64

data BSState = NotStarted | Skipped | Fragment ByteString
  deriving (Show)

type CMState = Map.Map (Word16, Word16) (Word32, BSState)

reassembleControlMessages :: Stream IO (Word16, Word16, Word32, ControlMessages) -> Stream IO ControlMessage
reassembleControlMessages (Stream step state) = do
  Stream step' ([], Map.empty, state)
  where
    step' _ (x : xs, m, st) = do
      cm <- consumeAll get x
      return $ Yield cm (xs, m, st)
    step' gst ([], m, st) = do
      r <- step (adaptState gst) st
      case r of
        Yield a s -> do
          res <- consume m a
          case res of
            Pipe.Yield a' s' -> return $ Skip (a', s', s)
            Pipe.Continue s' -> return $ Skip ([], s', s)
        Skip s -> return $ Skip ([], m, s)
        Stop -> do
          let m' = Map.filter (\(_, s) -> case s of Fragment _ -> True; _ -> False) m
          unless (Map.null m') $ fail $ "Unfinished fragmented messages" ++ show m'
          return Stop

    consume :: CMState -> (Word16, Word16, Word32, ControlMessages) -> IO (Pipe.Step CMState [ByteString])
    consume m (cid, pid, pseq, ControlMessages ind _ _ msgs) = case ind of
      -- state machine
      -- null -> [Fragment (DividedHead), Skipped (DividedBody), NotStarted (Undivided/DividedEnd)]
      -- NotStarted -> [Fragment (DividedHead), Skipped (DividedBody), NotStarted (Undivided/DevidedEnd)]
      -- Skipped -> [Skipped (DividedBody), NotStarted (Undivided/DevidedEnd)]
      -- Fragment -> [Fragment (DividedBody | seq follow), Skipped (DividedBody | seq lost), NotStarted (Undivided/DividedEnd)]
      FragmentationIndicatorUndivided -> do
        handleHead
        let m' = Map.insert (cid, pid) (pseq, NotStarted) m
        case msgs of
          Left bs -> return $ Pipe.Yield [bs] m'
          Right bs -> return $ Pipe.Yield bs m'
      FragmentationIndicatorDividedHead -> do
        handleHead
        t <- extractOne msgs
        return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Fragment t) m
      FragmentationIndicatorDividedBody -> do
        prev <- handleEnd
        case prev of
          Nothing -> return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Skipped) m
          Just bs -> do
            t <- extractOne msgs
            return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Fragment $ bs <> t) m
      FragmentationIndicatorDividedEnd -> do
        prev <- handleEnd
        let m' = Map.insert (cid, pid) (pseq, NotStarted) m
        case prev of
          Nothing -> return $ Pipe.Continue m'
          Just bs -> do
            t <- extractOne msgs
            return $ Pipe.Yield [bs <> t] m'
      where
        extractOne :: MonadFail m => Either ByteString [ByteString] -> m ByteString
        extractOne (Left bs) = return bs
        extractOne (Right _) = fail "more than one message"

        handleHead :: IO ()
        handleHead =
          case Map.lookup (cid, pid) m of
            Nothing -> return ()
            Just (s, b) | s + 1 /= pseq -> do
              regressionCheck s
              miss s
              case b of
                Fragment _ -> putStrLn "\tDiscarding previous fragmented messages"
                _ -> pure ()
            Just (_, Fragment _) -> fail "Incorrect fragmentation indicator"
            _ -> return ()

        handleEnd :: IO (Maybe ByteString)
        handleEnd =
          case Map.lookup (cid, pid) m of
            Nothing -> do
              putStrLn $ printf "Sequence started mid-stream for Context ID 0x%x, Packet ID 0x%X" cid pid
              discard
              return Nothing
            Just (s, b) | s + 1 /= pseq -> do
              regressionCheck s
              miss s
              case b of
                Fragment _ -> putStrLn "\tDiscarding previous fragmented messages"
                _ -> pure ()
              discard
              return Nothing
            Just (_, Fragment b) -> return $ Just b
            Just (_, Skipped) -> Nothing <$ discard
            Just (_, NotStarted) -> fail "Incorrect fragmentation indicator"

        regressionCheck :: Word32 -> IO ()
        regressionCheck s =
          when (s >= pseq) $ fail $ printf "Sequence number regression for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq

        discard :: IO ()
        discard = putStrLn $ "\tDiscarding sequence " ++ show pseq ++ ", indicator " ++ show ind

        miss :: Word32 -> IO ()
        miss s = putStrLn $ printf "Sequence number missing for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq

parseTLVPackets :: ParseState -> IO (Maybe (TLVPacket, ParseState))
parseTLVPackets (ParseState Empty _) = return Nothing
parseTLVPackets (ParseState bs o) = do
  case runGetOrFail get bs of
    Left (_, off, err) -> fail $ "error parse at " ++ show (o + off) ++ ": " ++ err
    Right (r, off, p) -> return $ Just (p, ParseState r (o + off))

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
    [] -> fail "No file specified"
    x : _ -> return x
  file <- L.readFile filename
  let packets = unfoldrM parseTLVPackets (ParseState file 0)
      headers =
        mapMaybe
          ( \case
              TLVPacketHeaderCompressedIP
                ( CompressedIPPacket
                    { contextId = c,
                      contextIdentificationHeader = h
                    }
                  ) -> Just (c, h)
              _ -> Nothing
          )
          packets
      payloads =
        mapMaybe
          (\case (c, ContextIdentificationNoCompressedHeader h) -> Just (c, h); _ -> Nothing)
          headers
      fragments =
        mapMaybe
          ( \case
              ( cid,
                MMTPPacket
                  { packetId = pid,
                    packetSequenceNumber = pseq,
                    mmtpPayload = MMTPPayloadControlMessages m
                  }
                ) -> Just (cid, pid, pseq, m)
              _ -> Nothing
          )
          payloads
      messages = reassembleControlMessages fragments
  -- (r, o) <- S.foldrM reassembleControlMessages (pure (mempty, [])) fragments

  -- _ <- S.toList $ S.mapM pPrint $ (\(pid, pseq, m) -> (pid, pseq, Message.fragmentationIndicator m)) <$> fragments

  len <- S.foldr (\_ c -> c + 1) (0 :: Word64) messages
  print len

-- list <- toList $ parseTLVPackets (runGetIncremental get) file
-- print list

-- mapM_ print $ runGet (parseLimitPacket 2) input
