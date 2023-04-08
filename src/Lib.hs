{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}

module Lib (TLVPacket) where

import Common (FragmentationIndicator, consumeAll, repeatRead)
import Data.Binary (Binary (..), Word16, Word32, Word8)
import Data.Binary.Get
  ( Get,
    getLazyByteString,
    getRemainingLazyByteString,
    getWord16be,
    getWord32be,
    getWord8,
    lookAhead,
  )
import Data.Bits (shiftR, testBit, (.&.))
import Data.ByteString.Lazy (ByteString)
import Message (ControlMessages)

data MPUFragment
  = MPUFragmentMPUMetadata
  | MPUFragmentMovieFragmentMetadata
  | MPUFragmentMFU MFU
  deriving (Show)

data MFU
  = NonTimed MFUNonTimed
  | NonTimedAggregated [MFUNonTimed]
  | Timed MFUTimed
  | TimedAggregated [MFUTimed]
  deriving (Show)

data MFUTimed = MFUTimed
  { movieFragmentSequenceNumber :: Word32,
    sampleNumber :: Word32,
    offset :: Word32,
    priority :: Word8,
    depedenencyCounter :: Word8,
    mfuData :: ByteString
  }
  deriving (Show)

instance Binary MFUTimed where
  get =
    MFUTimed <$> getWord32be
      <*> getWord32be
      <*> getWord32be
      <*> getWord8
      <*> getWord8
      <*> getRemainingLazyByteString
  put _ = undefined

data MFUNonTimed = MFUNonTimed
  { itemId :: Word32,
    mfuData :: ByteString
  }
  deriving (Show)

instance Binary MFUNonTimed where
  get = MFUNonTimed <$> getWord32be <*> getRemainingLazyByteString
  put _ = undefined

data MPU = MPU
  { fragmentationIndicator :: FragmentationIndicator,
    divisionNumberCounter :: Word8,
    mpuSequenceNumber :: Word32,
    mpuFragment :: MPUFragment
  }
  deriving (Show)

instance Binary MPU where
  get = do
    len <- getWord16be
    byte <- getWord8
    dnc <- getWord8
    mpuSeqNum <- getWord32be
    payload <- getLazyByteString $ fromIntegral $ len - 6

    fragment <- case shiftR byte 4 of
      0x00 -> return MPUFragmentMPUMetadata
      0x01 -> return MPUFragmentMovieFragmentMetadata
      0x02 -> MPUFragmentMFU <$> consumeAll (parseMFU (testBit byte 3) (testBit byte 0)) payload
      _ -> fail "Invalid MPU fragmentation indicator"

    return $
      MPU
        { fragmentationIndicator = toEnum $ fromIntegral $ shiftR byte 1 .&. 0b11,
          divisionNumberCounter = dnc,
          mpuSequenceNumber = mpuSeqNum,
          mpuFragment = fragment
        }
    where
      parseMFU :: Bool -> Bool -> Get MFU -- timed flag, aggregated flag
      parseMFU False False = NonTimed <$> get
      parseMFU False True = NonTimedAggregated <$> parseAggregatedNonTimedMFU
      parseMFU True False = Timed <$> get
      parseMFU True True = TimedAggregated <$> parseAggregatedTimedMFU

      parseAggregatedNonTimedMFU :: Get [MFUNonTimed]
      parseAggregatedNonTimedMFU =
        repeatRead $
          getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll get

      parseAggregatedTimedMFU :: Get [MFUTimed]
      parseAggregatedTimedMFU =
        repeatRead $
          getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll get

  put = undefined

data MMTPMultiExtensionHeader
  = MMTPMultiExtensionHeaderScrambleInformation
  | MMTPMultiExtensionHeaderDownloadID Word32
  | MMTPMultiExtensionHeaderFileDivisionTransmissionInformation
  | MMTPMultiExtensionHeaderReserved Word16 ByteString
  deriving (Show)

instance Binary MMTPMultiExtensionHeader where
  get = do
    typ <- (0b0111_1111_1111_1111 .&.) <$> getWord16be
    l <- getWord16be
    dat <- getLazyByteString $ fromIntegral l
    case typ of
      0x0001 -> return MMTPMultiExtensionHeaderScrambleInformation
      0x0002 -> MMTPMultiExtensionHeaderDownloadID <$> consumeAll get dat
      0x0003 -> return MMTPMultiExtensionHeaderFileDivisionTransmissionInformation
      _ -> return $ MMTPMultiExtensionHeaderReserved typ dat

  put = undefined

data MMTPExtensionHeader
  = MMTPExtensionHeaderMultiTypeHeaderExtension [MMTPMultiExtensionHeader]
  | MMTPExtensionHeaderPrivate Word16 ByteString
  deriving (Show)

instance Binary MMTPExtensionHeader where
  get = do
    typ <- getWord16be
    l <- getWord16be
    dat <- getLazyByteString $ fromIntegral l
    case typ of
      0x0000 -> MMTPExtensionHeaderMultiTypeHeaderExtension <$> consumeAll parseMulti dat
      _ -> return $ MMTPExtensionHeaderPrivate typ dat
    where
      parseMulti :: Get [MMTPMultiExtensionHeader]
      parseMulti = do
        end <- (`testBit` 7) <$> lookAhead getWord8
        t <- get
        if end
          then return [t]
          else (t :) <$> parseMulti

  put = undefined

data MMTPPayload
  = MMTPPayloadMPU MPU
  | MMTPPayloadGenericObject ByteString
  | MMTPPayloadControlMessages ControlMessages
  | MMTPPayloadRepairSymbol ByteString
  | MMTPPayloadReserved Word8 ByteString
  | MMTPPayloadPrivate Word8 ByteString
  deriving (Show)

data MMTPPacket = MMTPPacket
  { version :: Word8,
    fecType :: Word8,
    rapFlag :: Bool,
    packetId :: Word16,
    deliveryTimestamp :: Word32,
    packetSequenceNumber :: Word32,
    packetCounter :: Maybe Word32,
    extensionHeader :: Maybe MMTPExtensionHeader,
    mmtpPayload :: MMTPPayload
  }
  deriving (Show)

instance Binary MMTPPacket where
  get = do
    firstByte <- getWord8
    secondByte <- getWord8
    pid <- getWord16be
    dts <- getWord32be
    psn <- getWord32be

    pktct <- if testBit firstByte 5 then Just <$> getWord32be else return Nothing
    extHdr <- if testBit firstByte 1 then Just <$> get else return Nothing

    payload <- case secondByte .&. 0b11_1111 of
      0x00 -> MMTPPayloadMPU <$> get
      0x01 -> MMTPPayloadGenericObject <$> getRemainingLazyByteString
      0x02 -> MMTPPayloadControlMessages <$> get
      0x03 -> MMTPPayloadRepairSymbol <$> getRemainingLazyByteString
      typ | typ >= 0x04 && typ <= 0x1F -> MMTPPayloadReserved typ <$> getRemainingLazyByteString
      typ | typ >= 0x20 && typ <= 0x3F -> MMTPPayloadPrivate typ <$> getRemainingLazyByteString
      _ -> undefined -- unreachable
    return $
      MMTPPacket
        { version = shiftR firstByte 6,
          fecType = shiftR firstByte 3 .&. 0b11,
          rapFlag = testBit firstByte 0,
          packetId = pid,
          deliveryTimestamp = dts,
          packetSequenceNumber = psn,
          packetCounter = pktct,
          extensionHeader = extHdr,
          mmtpPayload = payload
        }

  put = undefined

data ContextIdentificationHeader
  = ContextIdentificationHeaderPartialIPv4UDP
  | ContextIdentificationHeaderIPv4Identifier
  | ContextIdentificationHeaderPartialIPv6UDP
  | ContextIdentificationNoCompressedHeader MMTPPacket
  deriving (Show)

data CompressedIPPacket = CompressedIPPacket
  { contextId :: Word16,
    sequenceNumber :: Word8,
    contextIdentificationHeader :: ContextIdentificationHeader
  }
  deriving (Show)

instance Binary CompressedIPPacket where
  get = do
    firstTwo <- getWord16be
    header <- getWord8 >>= extractHeader
    return $ CompressedIPPacket (shiftR firstTwo 4) (fromIntegral (firstTwo .&. 0b1111)) header
    where
      extractHeader :: Word8 -> Get ContextIdentificationHeader
      extractHeader 0x20 = return ContextIdentificationHeaderPartialIPv4UDP
      extractHeader 0x21 = return ContextIdentificationHeaderIPv4Identifier
      extractHeader 0x60 = return ContextIdentificationHeaderPartialIPv6UDP
      extractHeader 0x61 = ContextIdentificationNoCompressedHeader <$> get
      extractHeader _ = fail "unknown header"

  put = undefined

data TLVPacket
  = TLVPacketIPv4 ByteString
  | TLVPacketIPv6 ByteString
  | TLVPacketHeaderCompressedIP CompressedIPPacket
  | TLVPacketTransmissionControlSignal ByteString
  | TLVPacketNull ByteString
  deriving (Show)

instance Binary TLVPacket where
  get = do
    tlvHeader <- getWord8
    case tlvHeader of
      0b01_111111 -> do
        t <- getWord8
        l <- getWord16be
        d <- getLazyByteString $ fromIntegral l
        case t of
          0x01 -> return $ TLVPacketIPv4 d
          0x02 -> return $ TLVPacketIPv6 d
          0x03 -> TLVPacketHeaderCompressedIP <$> consumeAll get d
          0xFE -> return $ TLVPacketTransmissionControlSignal d
          0xFF -> return $ TLVPacketNull d
          _ -> fail "Invalid TLV packet type"
      _ -> fail "Invalid TLV packet header"

  put = undefined