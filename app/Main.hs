{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Common (FragmentationIndicator (..), consumeAll)
import Control.Monad (unless, when)
import Data.Binary (Binary, Get, Word16, Word32, Word64, get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Internal (ByteString (Empty))
import Data.Int (Int64)
import qualified Data.Map as Map
import Lib
import Message (ControlMessage, ControlMessages (..))
import Streamly.Data.Stream (Stream, mapMaybe, unfoldrM)
import qualified Streamly.Data.Stream as S (foldr, foldrM, mapM, take, toList)
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Stream (Step (..), Stream (Stream))
import Text.Show.Pretty (pPrint)

data ParseState = ParseState ByteString Int64

type CMState = Map.Map Word16 (Word32, ByteString)

reassembleControlMessages :: MonadFail m => Stream m (Word16, Word32, ControlMessages) -> Stream m ControlMessage
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
          unless (Map.null m) $ fail "Unfinished fragmented messages"
          return Stop

    consume :: MonadFail m => CMState -> (Word16, Word32, ControlMessages) -> m (Pipe.Step CMState [ByteString])
    consume m (pid, pseq, ControlMessages ind _ _ msgs) =
      case ind of
        FragmentationIndicatorUndivided -> do
          when (Map.member pid m) $ fail $ "Packet ID " ++ show pid ++ " already started fragmented message"
          case msgs of
            Left bs -> return $ Pipe.Yield [bs] m
            Right bs -> return $ Pipe.Yield bs m
        FragmentationIndicatorDividedHead -> do
          when (Map.member pid m) $ fail $ "Packet ID " ++ show pid ++ " already started fragmented message"
          t <- extractOne msgs
          return $ Pipe.Continue $ Map.insert pid (pseq, t) m
        FragmentationIndicatorDividedBody -> do
          case Map.lookup pid m of
            Nothing -> fail $ "Packet ID " ++ show pid ++ " not started fragmented message"
            Just (s, bs) -> do
              when (s >= pseq) $ fail "Sequence number reversed"
              t <- extractOne msgs
              return $ Pipe.Continue $ Map.insert pid (pseq, bs <> t) m
        FragmentationIndicatorDividedEnd -> do
          case Map.lookup pid m of
            Nothing -> fail $ "Packet ID " ++ show pid ++ " not started fragmented message"
            Just (s, bs) -> do
              when (s >= pseq) $ fail "Sequence number reversed"
              t <- extractOne msgs
              return $ Pipe.Yield [bs <> t] $ Map.delete pid m
      where
        extractOne :: MonadFail m => Either ByteString [ByteString] -> m ByteString
        extractOne (Left bs) = return bs
        extractOne (Right _) = fail "more than one message"

parseTLVPackets :: MonadFail m => ParseState -> m (Maybe (TLVPacket, ParseState))
parseTLVPackets (ParseState Empty _) = return Nothing
parseTLVPackets (ParseState bs o) = do
  case runGetOrFail get bs of
    Left (_, off, err) -> fail $ "error parse at " ++ show (o + off) ++ ": " ++ err
    Right (r, off, p) -> return $ Just (p, ParseState r (o + off))

main :: IO ()
main = do
  file <- L.readFile "F:\\29999.mmts"
  let packets = unfoldrM parseTLVPackets (ParseState file 0)
      headers =
        mapMaybe
          (\case TLVPacketHeaderCompressedIP c -> Just $ contextIdentificationHeader c; _ -> Nothing)
          packets
      payloads =
        mapMaybe
          (\case ContextIdentificationNoCompressedHeader h -> Just h; _ -> Nothing)
          headers
      fragments =
        mapMaybe
          ( \case
              MMTPPacket
                { packetId = pid,
                  packetSequenceNumber = pseq,
                  mmtpPayload = MMTPPayloadControlMessages m
                } -> Just (pid, pseq, m)
              _ -> Nothing
          )
          payloads
      messages = reassembleControlMessages fragments
  -- (r, o) <- S.foldrM reassembleControlMessages (pure (mempty, [])) fragments

  _ <- S.toList $ S.mapM pPrint messages
  return ()

-- len <- S.foldr (\_ c -> c + 1) (0 :: Word64) messages
-- print len

-- list <- toList $ parseTLVPackets (runGetIncremental get) file
-- print list

-- mapM_ print $ runGet (parseLimitPacket 2) input
