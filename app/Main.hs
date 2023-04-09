{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Common (FragmentationIndicator (..), consumeAll)
import Control.Monad (when)
import Data.Binary (Binary, Get, Word16, Word32, Word64, get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Internal (ByteString (Empty))
import Data.Int (Int64)
import qualified Data.Map as Map
import Lib
import Message (ControlMessage, ControlMessages (..))
import qualified Streamly.Data.Fold as Fold
import Streamly.Data.Stream (Stream, mapMaybe, postscan, unfoldrM)
import qualified Streamly.Data.Stream as S (foldr, foldrM, mapM, take, toList)
import Streamly.Internal.Data.Pipe.Type (Pipe (Pipe), PipeState (..), Step (..))
import Streamly.Internal.Data.Stream.StreamD.Transform (transform)
import Text.Show.Pretty (pPrint)

data ParseState = ParseState ByteString Int64

type CMState = Map.Map Word16 (Word32, ByteString)

type CMPipeState = PipeState CMState (CMState, [ByteString])

reassembleControlMessages :: Pipe IO (Word16, Word32, ControlMessages) ControlMessage
reassembleControlMessages = Pipe consume produce Map.empty
  where
    produce :: (CMState, [ByteString]) -> IO (Step CMPipeState ControlMessage)
    produce (m, []) = return $ Continue $ Consume m
    produce (m, x : xs) = do
      p <- consumeAll get x
      return $ Yield p $ Produce (m, xs)

    consume :: CMState -> (Word16, Word32, ControlMessages) -> IO (Step CMPipeState ControlMessage)
    consume m (pid, pseq, ControlMessages ind _ _ msgs) =
      case ind of
        FragmentationIndicatorUndivided -> case msgs of
          Left bs -> return $ Continue $ Produce (m, [bs])
          Right bs -> return $ Continue $ Produce (m, bs)
        FragmentationIndicatorDividedHead -> do
          when (Map.member pid m) $ fail $ "Packet ID " ++ show pid ++ " already started fragmented message"
          t <- extractOne msgs
          return $ Continue $ Consume $ Map.insert pid (pseq, t) m
        FragmentationIndicatorDividedBody -> do
          case Map.lookup pid m of
            Nothing -> fail $ "Packet ID " ++ show pid ++ " not started fragmented message"
            Just (s, bs) -> do
              when (s >= pseq) $ fail "Sequence number reversed"
              t <- extractOne msgs
              return $ Continue $ Consume $ Map.insert pid (pseq, bs <> t) m
        FragmentationIndicatorDividedEnd -> do
          case Map.lookup pid m of
            Nothing -> fail $ "Packet ID " ++ show pid ++ " not started fragmented message"
            Just (s, bs) -> do
              when (s >= pseq) $ fail "Sequence number reversed"
              t <- extractOne msgs
              return $ Continue $ Produce (Map.delete pid m, [bs <> t])
      where
        extractOne :: MonadFail m => Either ByteString [ByteString] -> m ByteString
        extractOne (Left bs) = return bs
        extractOne (Right _) = fail "more than one message"

parseTLVPackets :: ParseState -> IO (Maybe (TLVPacket, ParseState))
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
      messages = transform reassembleControlMessages fragments
  -- (r, o) <- S.foldrM reassembleControlMessages (pure (mempty, [])) fragments

  _ <- S.toList $ S.mapM pPrint messages
  return ()

-- len <- S.foldr (\_ c -> c + 1) (0 :: Word64) messages
-- print len

-- list <- toList $ parseTLVPackets (runGetIncremental get) file
-- print list

-- mapM_ print $ runGet (parseLimitPacket 2) input
