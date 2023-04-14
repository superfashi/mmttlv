{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (when)
import Data.Binary (Word16, Word32, get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString (Empty))
import Data.Int (Int64)
import Data.MMTTLV
  ( CompressedIPPacket (..),
    ContextIdentificationHeader (..),
    MMTPPacket (..),
    MMTPPayload (..),
    TLVPacket (..),
  )
import Data.MMTTLV.Internal (FragmentationIndicator (..), consumeAll)
import Data.MMTTLV.Message
  ( ControlMessage (ControlMessagePA),
    ControlMessages (ControlMessages),
    PAMessage (PAMessage, tables),
  )
import Data.MMTTLV.Table (MMTPackageTable, Table (MPT))
import qualified Data.Map as Map
import Streamly.Data.Stream (Stream, mapMaybe, uncons, unfoldrM)
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Stream (Step (..), Stream (Stream))
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import Text.Printf (hPrintf, printf)
import Text.Show.Pretty (pPrint)

data ParseState = ParseState ByteString Int64

parseTLVPackets :: ParseState -> IO (Maybe (TLVPacket, ParseState))
parseTLVPackets (ParseState Empty _) = return Nothing
parseTLVPackets (ParseState bs o) = do
  case runGetOrFail get bs of
    Left (_, off, err) -> fail $ "error parse at " ++ show (o + off) ++ ": " ++ err
    Right (r, off, p) -> return $ Just (p, ParseState r (o + off))

reassemble ::
  MonadFail m =>
  Stream m ((Word16, Word16), Word32, (FragmentationIndicator, [ByteString])) ->
  Stream m ((Word16, Word16), ByteString)
reassemble (Stream step state) = do
  Stream step' ([], Map.empty, state)
  where
    step' _ ((cpid, x) : xs, m, st) = do
      return $ Yield (cpid, x) (xs, m, st)
    step' gst ([], m, st) = do
      r <- step (adaptState gst) st
      case r of
        Yield a s -> do
          res <- consume m a
          case res of
            Pipe.Yield a' s' -> return $ Skip (a', s', s)
            Pipe.Continue s' -> return $ Skip ([], s', s)
        Skip s -> return $ Skip ([], m, s)
        Stop -> return Stop

    consume m ((cid, pid), pseq, (ind, frag)) = case ind of
      FragmentationIndicatorUndivided -> do
        handleHead
        return $ Pipe.Yield (((cid, pid),) <$> frag) $ Map.insert (cid, pid) (pseq, Nothing) m
      FragmentationIndicatorDividedHead -> do
        handleHead
        t <- extractOne frag
        return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Just t) m
      FragmentationIndicatorDividedBody -> do
        prev <- handleEnd
        case prev of
          Nothing -> return $ Pipe.Continue m
          Just bs -> do
            t <- extractOne frag
            return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Just $ bs <> t) m
      FragmentationIndicatorDividedEnd -> do
        prev <- handleEnd
        case prev of
          Nothing -> return $ Pipe.Continue m
          Just bs -> do
            t <- extractOne frag
            return $ Pipe.Yield [((cid, pid), bs <> t)] $ Map.insert (cid, pid) (pseq, Nothing) m
      where
        extractOne :: MonadFail m => [ByteString] -> m ByteString
        extractOne [] = fail "Empty extraction"
        extractOne [x] = return x
        extractOne _ = fail "Too many fragments"

        handleHead :: MonadFail m => m ()
        handleHead =
          case Map.lookup (cid, pid) m of
            Nothing -> return ()
            Just (s, _) | s + 1 /= pseq -> miss s
            Just (_, Just _) -> fail "Incorrect fragmentation indicator"
            _ -> return ()

        handleEnd :: MonadFail m => m (Maybe ByteString)
        handleEnd =
          case Map.lookup (cid, pid) m of
            Nothing -> return Nothing
            Just (s, _) | s + 1 /= pseq -> miss s
            Just (_, Just b) -> return $ Just b
            Just (_, Nothing) -> fail "Incorrect fragmentation indicator"

        miss :: MonadFail m => Word32 -> m a
        miss s = do
          when (s >= pseq) $ fail $ printf "Sequence number regression for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq
          fail $ printf "Sequence number missing for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq

parseControlMessages :: MonadFail m => ((Word16, Word16), ByteString) -> m ControlMessage
parseControlMessages (_, bs) = consumeAll get bs

extractControlMessages :: TLVPacket -> Maybe ((Word16, Word16), Word32, (FragmentationIndicator, [ByteString]))
extractControlMessages = comp (\case MMTPPayloadControlMessages m -> Just $ conv m; _ -> Nothing)
  where
    conv :: ControlMessages -> (FragmentationIndicator, [ByteString])
    conv (ControlMessages ind _ _ (Left bs)) = (ind, [bs])
    conv (ControlMessages ind _ _ (Right bs)) = (ind, bs)

    comp :: (MMTPPayload -> Maybe a) -> TLVPacket -> Maybe ((Word16, Word16), Word32, a)
    comp
      e
      ( TLVPacketHeaderCompressedIP
          ( CompressedIPPacket
              { contextId = cid,
                contextIdentificationHeader = h
              }
            )
        ) =
        case h of
          ContextIdentificationHeaderPartialIPv6UDP _ _ m -> extractMMTP m
          ContextIdentificationNoCompressedHeader m -> extractMMTP m
          _ -> Nothing
        where
          extractMMTP
            ( MMTPPacket
                { Data.MMTTLV.packetId = pid,
                  packetSequenceNumber = pseq,
                  mmtpPayload = p
                }
              ) = case e p of Just a -> Just ((cid, pid), pseq, a); _ -> Nothing
    comp _ _ = Nothing

extractMPT :: ControlMessage -> Maybe MMTPackageTable
extractMPT (ControlMessagePA (PAMessage {tables = tbls})) = extract tbls
  where
    extract :: [Table] -> Maybe MMTPackageTable
    extract [] = Nothing
    extract (MPT x : _) = Just x
    extract (_ : xs) = extract xs
extractMPT _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
    [] -> do
      hPrintf stderr "Usage: %s <filename>" =<< getProgName
      exitFailure
    x : _ -> return x
  file <- L.readFile filename

  let packets = unfoldrM parseTLVPackets (ParseState file 0)
      msgs = S.mapM parseControlMessages $ reassemble (mapMaybe extractControlMessages packets)
      mpts = mapMaybe extractMPT msgs

  mpt <- uncons mpts
  case mpt of
    Nothing -> fail "No MPT found"
    Just (m, _) -> pPrint m
