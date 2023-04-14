module Net where

import Data.Binary (Binary (..), Word16, Word32, Word64, Word8)
import Data.Binary.Get (getWord16be, getWord32be, getWord64be, getWord8)
import Data.Bits (Bits (shiftR), (.&.))

type IPv4Addr = (Word8, Word8, Word8, Word8)

type IPv6Addr = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)

data IPv6Header = IPv6Header
  { version :: Word8,
    trafficClass :: Word8,
    flowLabel :: Word32,
    payloadLength :: Word16,
    nextHeader :: Word8,
    hopLimit :: Word8,
    sourceAddress :: IPv6Addr,
    destinationAddress :: IPv6Addr
  }
  deriving (Show)

instance Binary IPv6Header where
  get = do
    ver_tc_fl <- getWord32be
    IPv6Header
      (fromIntegral $ shiftR ver_tc_fl 28)
      (fromIntegral $ shiftR ver_tc_fl 20 .&. 0b1111_1111)
      (ver_tc_fl .&. 0b1111_1111_1111_1111_1111)
      <$> getWord16be
        <*> getWord8
        <*> getWord8
        <*> get
        <*> get

  put = undefined

data PartialIPv6Header = PartialIPv6Header
  { version :: Word8,
    trafficClass :: Word8,
    flowLabel :: Word32,
    nextHeader :: Word8,
    hopLimit :: Word8,
    sourceAddress :: IPv6Addr,
    destinationAddress :: IPv6Addr
  }
  deriving (Show)

instance Binary PartialIPv6Header where
  get = do
    ver_tc_fl <- getWord32be
    PartialIPv6Header
      (fromIntegral $ shiftR ver_tc_fl 28)
      (fromIntegral $ shiftR ver_tc_fl 20 .&. 0b1111_1111)
      (ver_tc_fl .&. 0b1111_1111_1111_1111_1111)
      <$> getWord8
      <*> getWord8
      <*> get
      <*> get

  put = undefined

data UDPHeader = UDPHeader
  { sourcePort :: Word16,
    destinationPort :: Word16,
    length :: Word16,
    checksum :: Word16
  }
  deriving (Show)

instance Binary UDPHeader where
  get =
    UDPHeader
      <$> getWord16be
        <*> getWord16be
        <*> getWord16be
        <*> getWord16be

  put = undefined

data PartialUDPHeader = PartialUDPHeader
  { sourcePort :: Word16,
    destinationPort :: Word16
  }
  deriving (Show)

instance Binary PartialUDPHeader where
  get =
    PartialUDPHeader
      <$> getWord16be
      <*> getWord16be

  put = undefined

data NetworkTimeProtocolData = NetworkTimeProtocolData
  { leapIndicator :: Word8,
    version :: Word8,
    mode :: Word8,
    stratum :: Word8,
    poll :: Word8,
    precision :: Word8,
    rootDelay :: Word32,
    rootDispersion :: Word32,
    referenceIdentifier :: Word32,
    referenceTimestamp :: Word64,
    originateTimestamp :: Word64,
    receiveTimestamp :: Word64,
    transmitTimestamp :: Word64
  }
  deriving (Show)

instance Binary NetworkTimeProtocolData where
  get = do
    li_vn_mode <- getWord8
    NetworkTimeProtocolData
      (shiftR li_vn_mode 6)
      (shiftR li_vn_mode 3 .&. 0b111)
      (li_vn_mode .&. 0b111)
      <$> getWord8
        <*> getWord8
        <*> getWord8
        <*> getWord32be
        <*> getWord32be
        <*> getWord32be
        <*> getWord64be
        <*> getWord64be
        <*> getWord64be
        <*> getWord64be

  put = undefined
