module Lib where

import Common (FragmentationIndicator, consumeAll, repeatRead)
import Control.Monad (when)
import Control.Monad.Extra (whenMaybe)
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
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.ByteString.Lazy as L
import Debug.Trace (traceShow)
import Hexdump (prettyHex)
import Message (ControlMessages)
import Net (IPv6Addr, IPv6Header (..), NetworkTimeProtocolData, UDPHeader (..))
import Numeric (showHex)

data MPUFragment
  = MPUFragmentMPUMetadata
  | MPUFragmentMovieFragmentMetadata
  | MPUFragmentMFU MFU
  deriving (Show)

data MFU
  = MFUNonTimedType MFUNonTimed
  | MFUNonTimedAggregatedType [MFUNonTimed]
  | MFUTimedType MFUTimed
  | MFUTimedAggregatedType [MFUTimed]
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

    MPU (toEnum $ fromIntegral $ shiftR byte 1 .&. 0b11)
      <$> getWord8 <*> getWord32be <*> do
        payload <- getLazyByteString $ fromIntegral $ len - 6
        case shiftR byte 4 of
          0x00 -> return MPUFragmentMPUMetadata
          0x01 -> return MPUFragmentMovieFragmentMetadata
          0x02 -> MPUFragmentMFU <$> consumeAll (parseMFU (testBit byte 3) (testBit byte 0)) payload
          _ -> fail "Invalid MPU fragmentation indicator"
    where
      parseMFU :: Bool -> Bool -> Get MFU -- timed flag, aggregated flag
      parseMFU False False = MFUNonTimedType <$> get
      parseMFU False True = MFUNonTimedAggregatedType <$> parseAggregatedNonTimedMFU
      parseMFU True False = MFUTimedType <$> get
      parseMFU True True = MFUTimedAggregatedType <$> parseAggregatedTimedMFU

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
    MMTPPacket
      (shiftR firstByte 6)
      (shiftR firstByte 3 .&. 0b11)
      (testBit firstByte 0)
      <$> getWord16be
      <*> getWord32be
      <*> getWord32be
      <*> whenMaybe (testBit firstByte 5) getWord32be
      <*> whenMaybe (testBit firstByte 1) get
      <*> ( case secondByte .&. 0b11_1111 of
              0x00 -> MMTPPayloadMPU <$> get
              0x01 -> MMTPPayloadGenericObject <$> getRemainingLazyByteString
              0x02 -> MMTPPayloadControlMessages <$> get
              0x03 -> MMTPPayloadRepairSymbol <$> getRemainingLazyByteString
              typ | typ >= 0x04 && typ <= 0x1F -> MMTPPayloadReserved typ <$> getRemainingLazyByteString
              typ | typ >= 0x20 && typ <= 0x3F -> MMTPPayloadPrivate typ <$> getRemainingLazyByteString
              _ -> undefined -- unreachable
          )

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
    CompressedIPPacket (shiftR firstTwo 4) (fromIntegral (firstTwo .&. 0b1111))
      <$> (getWord8 >>= extractHeader)
    where
      extractHeader :: Word8 -> Get ContextIdentificationHeader
      extractHeader 0x20 = ContextIdentificationHeaderPartialIPv4UDP <$ getRemainingLazyByteString
      extractHeader 0x21 = ContextIdentificationHeaderIPv4Identifier <$ getRemainingLazyByteString
      extractHeader 0x60 = ContextIdentificationHeaderPartialIPv6UDP <$ getRemainingLazyByteString
      extractHeader 0x61 = ContextIdentificationNoCompressedHeader <$> get
      extractHeader _ = fail "unknown header"

  put = undefined

data IPv6Packet = IPv6Packet
  { -- IPv6 part
    version :: Word8,
    trafficClass :: Word8,
    flowLabel :: Word32,
    hopLimit :: Word8,
    sourceAddress :: IPv6Addr,
    destinationAddress :: IPv6Addr,
    -- UDP part
    sourcePort :: Word16,
    destinationPort :: Word16,
    -- Payload part
    payload :: Either ByteString NetworkTimeProtocolData
  }
  deriving (Show)

instance Binary IPv6Packet where
  get = do
    ih <- get
    when (Net.nextHeader ih /= 0x11) $ fail "IPv6Packet nextHeader is not UDP"
    getLazyByteString (fromIntegral $ Net.payloadLength ih) >>= consumeAll (parseUdp ih)
    where
      parseUdp :: IPv6Header -> Get IPv6Packet
      parseUdp
        IPv6Header
          { Net.version = v,
            Net.trafficClass = tc,
            Net.flowLabel = fl,
            Net.hopLimit = hl,
            Net.sourceAddress = sa,
            Net.destinationAddress = da
          } = do
          UDPHeader {Net.sourcePort = sp, Net.destinationPort = dp} <- get
          -- TODO: checksum
          IPv6Packet v tc fl hl sa da sp dp
            <$> (getRemainingLazyByteString >>= consumeAll (parsePayload da dp))

      parsePayload :: IPv6Addr -> Word16 -> Get (Either ByteString NetworkTimeProtocolData)
      parsePayload (_, _, _, _, _, _, _, 0x101) 123 = Right <$> get
      parsePayload _ _ = error "somthing else"

  put = undefined

data TLVPacket
  = TLVPacketIPv4 ByteString
  | TLVPacketIPv6 IPv6Packet
  | TLVPacketHeaderCompressedIP CompressedIPPacket
  | TLVPacketTransmissionControlSignal ByteString
  | TLVPacketNull Word16
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
          0x01 -> error "TODO: IPv4"
          0x02 -> TLVPacketIPv6 <$> consumeAll get d
          0x03 -> TLVPacketHeaderCompressedIP <$> consumeAll get d
          0xFE -> error "TODO: TCS"
          0xFF ->
            TLVPacketNull l
              <$ when (L.any (/= 0xFF) d) (fail "TLV Null packet is not null")
          _ -> fail "Invalid TLV packet type"
      _ -> fail "Invalid TLV packet header"

  put = undefined
