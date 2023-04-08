{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}

module Table where

import Common (consumeAll, readN, repeatRead)
import Control.Monad (when)
import Data.Binary (Binary (..), Word16, Word32, Word8)
import Data.Binary.Get (Get, getLazyByteString, getWord16be, getWord32be, getWord8, lookAhead)
import Data.Bits (Bits (testBit), (.&.))
import Data.ByteString.Lazy (ByteString)
import Descriptor (Descriptor)
import Net (IPv4Addr, IPv6Addr)

data Table
  = MPT MMTPackageTable
  | PLT
  deriving (Show)

instance Binary Table where
  get = do
    tableId <- lookAhead getWord8
    case tableId of
      0x20 -> MPT <$> get
      _ -> undefined
  put = undefined

data MMTGeneralLocationInfo
  = -- packet_id
    MMTLocationType0 Word16
  | -- ipv4_src, ipv4_dst, dst_port, packet_id
    MMTLocationType1 IPv4Addr IPv4Addr Word16 Word16
  | -- ipv6_src, ipv6_dst, dst_port, packet_id
    MMTLocationType2 IPv6Addr IPv6Addr Word16 Word16
  | -- netword_id, MPEG_2_transport_stream_id, MPEG_2_PID
    MMTLocationType3 Word16 Word16 Word16
  | -- ipv6_src, ipv6_dst, dst_port, MPEG_2_PID
    MMTLocationType4 IPv6Addr IPv6Addr Word16 Word16
  | -- url
    MMTLocationType5 ByteString
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
    assetType :: Word32,
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
      <*> getWord32be
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
  get = do
    tableId <- getWord8
    when (tableId /= 0x20) $ fail "Invalid MPT table ID"
    ver <- getWord8
    getWord16be >>= getLazyByteString . fromIntegral >>= consumeAll (parseTable ver)
    where
      parseTable :: Word8 -> Get MMTPackageTable
      parseTable ver =
        MMTPackageTable ver
          <$> ((0b11 .&.) <$> getWord8)
          <*> (getWord8 >>= getLazyByteString . fromIntegral)
          <*> (getWord16be >>= getLazyByteString . fromIntegral)
          <*> (getWord8 >>= readN)

  put = undefined
