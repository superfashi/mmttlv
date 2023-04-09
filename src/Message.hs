module Message where

import Common (FragmentationIndicator, consumeAll, readN, repeatRead)
import Control.Monad (when)
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
import Data.Bits (Bits (testBit), shiftR, (.&.))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L (length, splitAt)
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
  get = verifyMessageHeader 0x0000 getWord32be parseMsg
    where
      parseMsg :: Word8 -> Get PAMessage
      parseMsg ver = PAMessage ver <$> (getWord8 >>= readN) <*> repeatRead get

  put = undefined

data ControlMessage
  = ControlMessagePA PAMessage
  | ControlMessageM2Section M2SectionMessage
  | ControlMessageReserved16 Word16 Word8 ByteString
  deriving (Show)

instance Binary ControlMessage where
  get = do
    msgid <- lookAhead getWord16be
    case traceShow ("msgid", msgid) msgid of
      0x0000 -> ControlMessagePA <$> get
      0x8000 -> ControlMessageM2Section <$> get
      _
        | msgid >= 0x8004 && msgid <= 0xDFFF ->
          ControlMessageReserved16
            <$> getWord16be
            <*> getWord8
            <*> (getWord16be >>= getLazyByteString . fromIntegral)
      _ -> fail $ "Unsupported Control Message message id: " ++ show msgid

  put = undefined

data ControlMessages = ControlMessages
  { fragmentationIndicator :: FragmentationIndicator,
    lengthInformationExtensionFlag :: Bool,
    fragmentCounter :: Word8,
    messages :: Either ByteString [ByteString]
  }
  deriving (Show)

instance Binary ControlMessages where
  get = do
    fi_lef_ag <- getWord8
    fc <- getWord8
    let lef = testBit fi_lef_ag 1
    ControlMessages
      (toEnum $ fromIntegral $ shiftR fi_lef_ag 6)
      lef
      fc
      <$> if testBit fi_lef_ag 0
        then Right <$> readAggregate lef
        else Left <$> getRemainingLazyByteString
    where
      readAggregate :: Bool -> Get [ByteString]
      readAggregate lef =
        repeatRead $
          ( if lef
              then fromIntegral <$> getWord32be
              else fromIntegral <$> getWord16be
          )
            >>= getLazyByteString

  put = undefined

data M2SectionMessage = M2SectionMessage
  { version :: Word8,
    tableId :: Word8,
    sectionSyntaxIndicator :: Bool,
    tableIdExtension :: Word16,
    versionNumber :: Word8,
    currentNextIndicator :: Bool,
    sectionNumber :: Word8,
    lastSectionNumber :: Word8,
    signalingData :: Table,
    crc32 :: Word32
  }
  deriving (Show)

instance Binary M2SectionMessage where
  get = do
    verifyMessageHeader 0x8000 getWord16be parseMsg
    where
      parseMsg :: Word8 -> Get M2SectionMessage
      parseMsg ver = do
        tid <- getWord8
        ssi_sl <- getWord16be
        when ((shiftR ssi_sl 12 .&. 0b111) /= 0b111) $
          fail "incorrect internal fixed bits"
        (getLazyByteString . fromIntegral) (ssi_sl .&. 0b0000_1111_1111_1111)
          >>= consumeAll (parseSection ver tid (testBit ssi_sl 15))

      parseSection :: Word8 -> Word8 -> Bool -> Get M2SectionMessage
      parseSection ver tid ssi = do
        tide <- getWord16be
        vn_cni <- getWord8
        when (shiftR vn_cni 6 /= 0b11) $
          fail "incorrect internal fixed bits"
        sn <- getWord8
        lsn <- getWord8
        sd_crc <- getRemainingLazyByteString
        let (sd, crc) = L.splitAt (L.length sd_crc - 4) sd_crc
        M2SectionMessage
          ver
          tid
          ssi
          tide
          (shiftR vn_cni 1 .&. 0b11111)
          (testBit vn_cni 0)
          sn
          lsn
          <$> consumeAll get sd
          <*> consumeAll get crc

  put = undefined

verifyMessageHeader :: Integral b => Word16 -> Get b -> (Word8 -> Get a) -> Get a
verifyMessageHeader mid lp parse = do
  msgid <- getWord16be
  when (msgid /= mid) $ fail "Unexpected message id"
  ver <- getWord8
  lp >>= getLazyByteString . fromIntegral >>= consumeAll (parse ver)