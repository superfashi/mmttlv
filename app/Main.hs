{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Common (FragmentationIndicator (..), consumeAll)
import Control.Monad (unless, when)
import Data.Binary (Word16, Word32, Word64, get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString.Lazy as L (appendFile, drop, empty, hPut, length, pack, putStr, readFile, writeFile)
import Data.ByteString.Lazy.Internal (ByteString (Empty))
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Lib hiding (MFUNonTimed, MFUTimed)
import qualified Lib as MFUNonTimed (MFUNonTimed (mfuData))
import qualified Lib as MFUTimed (MFUTimed (mfuData))
import Message
import Net (NetworkTimeProtocolData)
import Streamly.Data.Stream (Stream, mapMaybe, unfoldrM)
import qualified Streamly.Data.Stream as S (foldr, foldrM, mapM, take, toList)
import qualified Streamly.Internal.Data.Pipe.Type as Pipe
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Stream (Step (..), Stream (Stream))
import qualified Streamly.Internal.Data.Stream as IS (mapM_)
import qualified Streamly.Internal.Data.Stream.StreamD.Container as D (nub)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), hPutStrLn, withBinaryFile)
import Table
import Text.Printf (printf)
import Text.Show.Pretty (pPrint)

data ParseState = ParseState ByteString Int64

data BSState = NotStarted | Skipped | Fragment ByteString
  deriving (Show)

type CMState = Map.Map (Word16, Word16) (Word32, BSState)

reassembleControlMessages :: Stream IO ((Word16, Word16), Word32, ControlMessages) -> Stream IO ControlMessage
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

    consume :: CMState -> ((Word16, Word16), Word32, ControlMessages) -> IO (Pipe.Step CMState [ByteString])
    consume m ((cid, pid), pseq, ControlMessages ind _ _ msgs) = case ind of
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

type MFUState = Map.Map (Word16, Word16) (Word32, Maybe ByteString)

reassembleMFU :: Stream IO ((Word16, Word16), Word32, MPU) -> Stream IO ((Word16, Word16), ByteString)
reassembleMFU (Stream step state) = do
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
        Stop -> do
          let m' = Map.filter (isJust . snd) m
          unless (Map.null m') $ print $ "Unfinished fragmented messages " ++ show (Map.keys m')
          return Stop

    consume :: MFUState -> ((Word16, Word16), Word32, MPU) -> IO (Pipe.Step MFUState [((Word16, Word16), ByteString)])
    consume m ((cid, pid), pseq, MPU ind _ _ frag) = case ind of
      FragmentationIndicatorUndivided -> do
        handleHead
        let t = extractMulti frag
        return $ Pipe.Yield (((cid, pid),) <$> t) $ Map.insert (cid, pid) (pseq, Nothing) m
      FragmentationIndicatorDividedHead -> do
        handleHead
        let t = extractOne frag
        return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Just t) m
      FragmentationIndicatorDividedBody -> do
        prev <- handleEnd
        case prev of
          Nothing -> return $ Pipe.Continue m
          Just bs -> do
            let t = extractOne frag
            return $ Pipe.Continue $ Map.insert (cid, pid) (pseq, Just $ bs <> t) m
      FragmentationIndicatorDividedEnd -> do
        prev <- handleEnd
        case prev of
          Nothing -> return $ Pipe.Continue m
          Just bs -> do
            let t = extractOne frag
            return $ Pipe.Yield [((cid, pid), bs <> t)] $ Map.insert (cid, pid) (pseq, Nothing) m
      where
        extractOne :: MPUFragment -> ByteString
        extractOne (MPUFragmentMFU (MFUNonTimedType d)) = MFUNonTimed.mfuData d
        extractOne (MPUFragmentMFU (MFUTimedType d)) = MFUTimed.mfuData d
        extractOne c = error $ show (cid, pid, pseq, c)

        extractMulti :: MPUFragment -> [ByteString]
        extractMulti (MPUFragmentMFU (MFUNonTimedType d)) = [MFUNonTimed.mfuData d]
        extractMulti (MPUFragmentMFU (MFUTimedType d)) = [MFUTimed.mfuData d]
        extractMulti (MPUFragmentMFU (MFUNonTimedAggregatedType d)) = MFUNonTimed.mfuData <$> d
        extractMulti (MPUFragmentMFU (MFUTimedAggregatedType d)) = MFUTimed.mfuData <$> d
        extractMulti c = error $ show (cid, pid, pseq, c)

        handleHead :: IO ()
        handleHead =
          case Map.lookup (cid, pid) m of
            Nothing -> return ()
            Just (s, _) | s + 1 /= pseq -> miss s
            Just (_, Just _) -> fail "Incorrect fragmentation indicator"
            _ -> return ()

        handleEnd :: IO (Maybe ByteString)
        handleEnd =
          case Map.lookup (cid, pid) m of
            Nothing -> do
              discard
              return Nothing
            Just (s, _) | s + 1 /= pseq -> miss s
            Just (_, Just b) -> return $ Just b
            Just (_, Nothing) -> fail "Incorrect fragmentation indicator"

        discard :: IO ()
        discard = putStrLn $ "\tDiscarding sequence " ++ show pseq ++ ", indicator " ++ show ind

        miss :: Word32 -> IO a
        miss s = do
          when (s >= pseq) $ fail $ printf "Sequence number regression for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq
          fail $ printf "Sequence number missing for Context ID 0x%X, Packet ID 0x%X, %d -> %d" cid pid s pseq

parseTLVPackets :: ParseState -> IO (Maybe (TLVPacket, ParseState))
parseTLVPackets (ParseState Empty _) = return Nothing
parseTLVPackets (ParseState bs o) = do
  case runGetOrFail get bs of
    Left (_, off, err) -> fail $ "error parse at " ++ show (o + off) ++ ": " ++ err
    Right (r, off, p) -> return $ Just (p, ParseState r (o + off))

extractComponent :: (MMTPPayload -> Maybe a) -> TLVPacket -> Maybe ((Word16, Word16), Word32, a)
extractComponent
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
            { Lib.packetId = pid,
              packetSequenceNumber = pseq,
              mmtpPayload = p
            }
          ) = case e p of Just a -> Just ((cid, pid), pseq, a); _ -> Nothing
extractComponent _ _ = Nothing

extractControlMessages :: TLVPacket -> Maybe ((Word16, Word16), Word32, ControlMessages)
extractControlMessages = extractComponent (\case MMTPPayloadControlMessages m -> Just m; _ -> Nothing)

extractMPU :: TLVPacket -> Maybe ((Word16, Word16), Word32, MPU)
extractMPU = extractComponent (\case MMTPPayloadMPU m -> Just m; _ -> Nothing)

extractNTP :: TLVPacket -> Maybe NetworkTimeProtocolData
extractNTP (TLVPacketIPv6 (IPv6Packet {payload = p})) = case p of
  Right d -> Just d
  _ -> Nothing
extractNTP _ = Nothing

extractMPT :: ControlMessage -> Maybe MMTPackageTable
extractMPT (ControlMessagePA (PAMessage {tables = tbls})) = extract tbls
  where
    extract :: [Table] -> Maybe MMTPackageTable
    extract [] = Nothing
    extract (MPT x : _) = Just x
    extract (_ : xs) = extract xs
extractMPT _ = Nothing

extractPLT :: ControlMessage -> Maybe PackageListTable
extractPLT (ControlMessagePA (PAMessage {tables = tbls})) = extract tbls
  where
    extract :: [Table] -> Maybe PackageListTable
    extract [] = Nothing
    extract (PLT x : _) = Just x
    extract (_ : xs) = extract xs
extractPLT _ = Nothing

extractMFU :: MPU -> Maybe MFU
extractMFU (MPU {mpuFragment = MPUFragmentMFU m}) = do
  case m of
    MFUNonTimedType m' -> Just $ MFUNonTimedType $ m' {MFUNonTimed.mfuData = L.empty}
    MFUTimedType m' -> Just $ MFUTimedType $ m' {MFUTimed.mfuData = L.empty}
    _ -> Nothing
extractMFU _ = Nothing

type CollectMap = Map.Map (Word16, Word16) ByteString

collectMFU :: ((Word16, Word16), ByteString) -> CollectMap -> CollectMap
collectMFU (cpid, bs) = Map.insertWith ((<>) . fix) cpid bs
  where
    fix :: ByteString -> ByteString
    fix bs = if L.length bs < 4 then error "Invalid MFU" else L.pack [0, 0, 0, 1] <> L.drop 4 bs

main :: IO ()
main = do
  args <- getArgs
  filename <- case args of
    [] -> fail "No file specified"
    x : _ -> return x
  file <- L.readFile filename
  let packets = unfoldrM parseTLVPackets (ParseState file 0)
  let messages = reassembleControlMessages $ mapMaybe extractControlMessages packets
  -- (r, o) <- S.foldrM reassembleControlMessages (pure (mempty, [])) fragments

  -- _ <- S.toList $ S.mapM pPrint $ (\(pid, pseq, m) -> (pid, pseq, Message.fragmentationIndicator m)) <$> fragments
  -- len <- S.foldr (\_ c -> c + 1) (0 :: Word64) $ mapMaybe extractNTP packets
  -- print len
  -- len <- S.foldr (\_ c -> c + 1) (0 :: Word64) messages
  -- print len

  let mpuPackets = mapMaybe extractMPU packets
      reasembledMFU = reassembleMFU (mapMaybe (\case w@((1, 0xF100), _, _) -> Just w; _ -> Nothing) mpuPackets)
  -- len <- S.foldr (\_ c -> c + 1) (0 :: Word64) mpuPackets
  -- print len
  -- S.toList $ S.mapM pPrint $ S.take 3 mpuPackets
  -- return ()
  collected <- S.foldr collectMFU Map.empty reasembledMFU
  L.writeFile "dump" $ collected Map.! (1, 0xF100)

-- IS.mapM_ (\(c, b) -> pPrint (c, b)) reasembledMFU

-- withBinaryFile "dump" WriteMode $ \h ->
--   IS.mapM_ (hPutStrLn h) $
--     mapMaybe
--       ( \(s, p) ->
--           case extractMFU p of
--             Just c -> Just $ show (Lib.fragmentationIndicator p, s, mpuSequenceNumber p, c)
--             _ -> Nothing
--       )
--       (mapMaybe (\case ((1, 0xF100), s, h) -> Just (s, h); _ -> Nothing) mpuPackets)

-- list <- toList $ parseTLVPackets (runGetIncremental get) file
-- print list

-- mapM_ print $ runGet (parseLimitPacket 2) input
