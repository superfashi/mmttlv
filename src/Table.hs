module Table where

import Common (consumeAll, readN, repeatRead)
import Control.Monad (when)
import Data.Binary (Binary (..), Word16, Word32, Word8)
import Data.Binary.Get (Get, getLazyByteString, getRemainingLazyByteString, getWord16be, getWord32be, getWord8, lookAhead)
import Data.Bits (Bits (testBit), (.&.))
import Data.ByteString.Lazy (ByteString)
import Descriptor (Descriptor)
import Net (IPv4Addr, IPv6Addr)

data Table
  = MPT MMTPackageTable
  | PLT PackageListTable
  | Unknown Word8 ByteString
  deriving (Show)

instance Binary Table where
  get = do
    tableId <- lookAhead getWord8
    case tableId of
      0x20 -> MPT <$> get
      0x80 -> PLT <$> get
      _ -> Unknown tableId <$> getRemainingLazyByteString
  put = undefined

data MMTGeneralLocationInfo
  = MMTLocationType0
      { packetId :: Word16
      }
  | MMTLocationType1
      { ipv4Src :: IPv4Addr,
        ipv4Dst :: IPv4Addr,
        dstPort :: Word16,
        packetId :: Word16
      }
  | MMTLocationType2
      { ipv6Src :: IPv6Addr,
        ipv6Dst :: IPv6Addr,
        dstPort :: Word16,
        packetId :: Word16
      }
  | MMTLocationType3
      { networkId :: Word16,
        mpeg2TransportStreamId :: Word16,
        mpeg2Pid :: Word16
      }
  | MMTLocationType4
      { ipv6Src :: IPv6Addr,
        ipv6Dst :: IPv6Addr,
        dstPort :: Word16,
        mpeg2Pid :: Word16
      }
  | MMTLocationType5
      { url :: ByteString
      }
  deriving (Show)

instance Binary MMTGeneralLocationInfo where
  get = do
    typ <- getWord8
    case typ of
      0x00 ->
        MMTLocationType0 <$> getWord16be
      0x01 ->
        MMTLocationType1 <$> get <*> get <*> getWord16be <*> getWord16be
      0x02 ->
        MMTLocationType2 <$> get <*> get <*> getWord16be <*> getWord16be
      0x03 ->
        MMTLocationType3 <$> getWord16be <*> getWord16be <*> ((0b0001_1111_1111_1111 .&.) <$> getWord16be)
      0x04 ->
        MMTLocationType4 <$> get <*> get <*> getWord16be <*> ((0b0001_1111_1111_1111 .&.) <$> getWord16be)
      0x05 ->
        MMTLocationType5 <$> (getWord8 >>= getLazyByteString . fromIntegral)
      _ -> fail "Invalid MMT General Location Info type"

  put = undefined

data MPTAsset = MPTAsset
  { idType :: Word8,
    assetIdScheme :: Word32,
    assetId :: ByteString,
    assetType :: ByteString,
    assetClockRelationFlag :: Bool,
    generalLocationInfo :: [MMTGeneralLocationInfo],
    descriptors :: [Descriptor]
  }
  deriving (Show)

instance Binary MPTAsset where
  get =
    MPTAsset
      <$> getWord8
      <*> getWord32be
      <*> (getWord8 >>= getLazyByteString . fromIntegral)
      <*> getLazyByteString 4
      <*> ((`testBit` 0) <$> getWord8)
      <*> (getWord8 >>= readN)
      <*> (getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll (repeatRead get))

  put = undefined

data MMTPackageTable = MMTPackageTable
  { version :: Word8,
    mptMode :: Word8,
    packageId :: ByteString,
    descriptors :: ByteString,
    assets :: [MPTAsset]
  }
  deriving (Show)

instance Binary MMTPackageTable where
  get = verifyTableHeader 0x20 parseTable
    where
      parseTable :: Word8 -> Get MMTPackageTable
      parseTable ver =
        MMTPackageTable ver
          <$> ((0b11 .&.) <$> getWord8)
          <*> (getWord8 >>= getLazyByteString . fromIntegral)
          <*> (getWord16be >>= getLazyByteString . fromIntegral)
          <*> (getWord8 >>= readN)

  put = undefined

data PLTIPDeliveryLocation
  = PLTIPDeliveryLocationType1
      { ipv4SrcAddr :: IPv4Addr,
        ipv4DstAddr :: IPv4Addr,
        dstPort :: Word16
      }
  | PLTIPDeliveryLocationType2
      { ipv6SrcAddr :: IPv6Addr,
        ipv6DstAddr :: IPv6Addr,
        dstPort :: Word16
      }
  | PLTIPDeliveryLocationType5
      { url :: ByteString
      }
  deriving (Show)

data PLTIPDelivery = PLTIPDelivery
  { transportFileId :: Word32,
    location :: PLTIPDeliveryLocation,
    descriptors :: [Descriptor]
  }
  deriving (Show)

instance Binary PLTIPDelivery where
  get =
    PLTIPDelivery
      <$> getWord32be
      <*> parseLocation
      <*> (getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll (repeatRead get))
    where
      parseLocation :: Get PLTIPDeliveryLocation
      parseLocation = do
        typ <- getWord8
        case typ of
          0x01 ->
            PLTIPDeliveryLocationType1 <$> get <*> get <*> getWord16be
          0x02 ->
            PLTIPDeliveryLocationType2 <$> get <*> get <*> getWord16be
          0x05 ->
            PLTIPDeliveryLocationType5 <$> (getWord8 >>= getLazyByteString . fromIntegral)
          _ -> fail "Invalid PLT IP delivery location type"

  put = undefined

data PLTMMTPackage = PLTMMTPackage
  { packageId :: ByteString,
    generalLocationInfo :: MMTGeneralLocationInfo
  }
  deriving (Show)

instance Binary PLTMMTPackage where
  get =
    PLTMMTPackage
      <$> (getWord8 >>= getLazyByteString . fromIntegral)
      <*> get

  put = undefined

data PackageListTable = PackageListTable
  { version :: Word8,
    packages :: [PLTMMTPackage],
    ipDeliveries :: [PLTIPDelivery]
  }
  deriving (Show)

instance Binary PackageListTable where
  get = verifyTableHeader 0x80 parseTable
    where
      parseTable :: Word8 -> Get PackageListTable
      parseTable ver = do
        PackageListTable ver
          <$> (getWord8 >>= readN)
          <*> (getWord8 >>= getLazyByteString . fromIntegral >>= consumeAll (repeatRead get))

  put = undefined

verifyTableHeader :: Word8 -> (Word8 -> Get a) -> Get a
verifyTableHeader t parse = do
  tid <- getWord8
  when (tid /= t) $ fail "Unexpected table id"
  ver <- getWord8
  getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll (parse ver)