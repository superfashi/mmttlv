module Message where

import Common (FragmentationIndicator, consumeAll, readN, repeatRead)
import Control.Monad (when)
import Data.Binary (Binary (..), Word16, Word8)
import Data.Binary.Get
  ( Get,
    getLazyByteString,
    getRemainingLazyByteString,
    getWord16be,
    getWord32be,
    getWord8,
    lookAhead,
  )
import Data.Bits (Bits (testBit), shiftR)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (traceShow)
import Table (Table)

data PAMessageTableInfo = PAMessageTableInfo
  { tableId :: Word8,
    tableVersion :: Word8,
    tableLength :: Word16
  }
  deriving (Show)

instance Binary PAMessageTableInfo where
  get = PAMessageTableInfo <$> getWord8 <*> getWord8 <*> getWord16be
  put = undefined

data PAMessage = PAMessage
  { version :: Word8,
    tableInfos :: [PAMessageTableInfo],
    tables :: [Table]
  }
  deriving (Show)

instance Binary PAMessage where
  get = do
    msgid <- getWord16be
    when (msgid /= 0x0000) $ fail "Invalid PA Message message id"
    ver <- getWord8
    getWord32be >>= getLazyByteString . fromIntegral >>= consumeAll (parseMsg ver)
    where
      parseMsg :: Word8 -> Get PAMessage
      parseMsg ver = PAMessage ver <$> (getWord8 >>= readN) <*> repeatRead get

  put = undefined

data ControlMessage
  = ControlMessagePA PAMessage
  | ControlMessageCA ByteString
  deriving (Show)

instance Binary ControlMessage where
  get = do
    msgid <- lookAhead getWord16be
    case traceShow msgid msgid of
      0x0000 -> ControlMessagePA <$> get
      -- 0x0001 -> return $ ControlMessageCA dat
      _ -> undefined -- TODO

  put = undefined

data ControlMessages = ControlMessages
  { fragmentationIndicator :: FragmentationIndicator,
    lengthInformationExtensionFlag :: Bool,
    divisionNumberCounter :: Word8,
    messages :: Either ControlMessage [ControlMessage]
  }
  deriving (Show)

instance Binary ControlMessages where
  get = do
    byte <- getWord8
    dnc <- getWord8
    let lef = testBit byte 1
    msgs <-
      if testBit byte 0
        then Right <$> readAggregate lef
        else Left <$> get
    return $
      ControlMessages
        { fragmentationIndicator = toEnum $ fromIntegral $ shiftR byte 6,
          lengthInformationExtensionFlag = lef,
          divisionNumberCounter = dnc,
          messages = msgs
        }
    where
      readAggregate :: Bool -> Get [ControlMessage]
      readAggregate lef =
        repeatRead $
          ( if lef
              then fromIntegral <$> getWord32be
              else fromIntegral <$> getWord16be
          )
            >>= getLazyByteString
            >>= consumeAll get

  put = undefined
