module Data.MMTTLV.Descriptor (module Data.MMTTLV.Descriptor) where

import Control.Monad (when)
import Control.Monad.Extra (whenMaybe)
import Data.Binary (Binary (..), Word16, Word32, Word64, Word8)
import Data.Binary.Get
  ( Get,
    getLazyByteString,
    getRemainingLazyByteString,
    getWord16be,
    getWord32be,
    getWord64be,
    getWord8,
    lookAhead,
  )
import Data.Bits (Bits (testBit), shiftR, (.&.))
import Data.ByteString.Lazy (ByteString)
import Data.MMTTLV.Internal (ISO639LanguageCode, consumeAll, readN, repeatRead)

data Descriptor
  = MPUTimestamp MPUTimestampDescriptor
  | VideoComponent VideoComponentDescriptor
  | MHStreamID MHStreamIDDescriptor
  | MHAudioComponent MHAudioComponentDescriptor
  | MHDataComponent MHDataComponentDescriptor
  | MPUExtendedTimestamp MPUExtendedTimestampDescriptor
  deriving (Show)

instance Binary Descriptor where
  get = do
    tag <- lookAhead getWord16be
    case tag of
      0x0001 -> MPUTimestamp <$> get
      0x8010 -> VideoComponent <$> get
      0x8011 -> MHStreamID <$> get
      0x8014 -> MHAudioComponent <$> get
      0x8020 -> MHDataComponent <$> get
      0x8026 -> MPUExtendedTimestamp <$> get
      _ -> fail $ "Unknown descriptor tag " ++ show tag

  put = undefined

newtype MPUTimestampDescriptor = MPUTimestampDescriptor [(Word32, Word64)]
  deriving (Show)

instance Binary MPUTimestampDescriptor where
  get =
    verifyDescriptorHeader 0x0001 $
      MPUTimestampDescriptor <$> repeatRead ((,) <$> getWord32be <*> getWord64be)

  put = undefined

data VideoComponentDescriptor = VideoComponentDescriptor
  { videoResolution :: Word8,
    videoAspectRatio :: Word8,
    videoScanFlag :: Bool,
    videoFrameRate :: Word8,
    componentTag :: Word16,
    videoTransferCharacteristics :: Word8,
    iso639LanguageCode :: ISO639LanguageCode,
    textChar :: ByteString
  }
  deriving (Show)

instance Binary VideoComponentDescriptor where
  get =
    verifyDescriptorHeader 0x8010 $ do
      res_asp <- getWord8
      flag_rate <- getWord8
      VideoComponentDescriptor
        (shiftR res_asp 4)
        (res_asp .&. 0b1111)
        (testBit flag_rate 7)
        (flag_rate .&. 0b11111)
        <$> getWord16be
          <*> ((`shiftR` 4) <$> getWord8)
          <*> get
          <*> getRemainingLazyByteString

  put = undefined

newtype MHStreamIDDescriptor = MHStreamIDDescriptor {componentTag :: Word16}
  deriving (Show)

instance Binary MHStreamIDDescriptor where
  get =
    verifyDescriptorHeader 0x8011 $
      MHStreamIDDescriptor <$> getWord16be
  put _ = undefined

data MHAudioComponentDescriptor = MHAudioComponentDescriptor
  { streamContent :: Word8,
    componentType :: Word8,
    componentTag :: Word16,
    streamType :: Word8,
    simulcastGroupTag :: Word8,
    mainComponentFlag :: Bool,
    qualityIndicator :: Word8,
    samplingRate :: Word8,
    iso639LanguageCode :: ISO639LanguageCode,
    iso639LanguageCode2 :: Maybe ISO639LanguageCode,
    textChar :: ByteString
  }
  deriving (Show)

instance Binary MHAudioComponentDescriptor where
  get =
    verifyDescriptorHeader 0x8014 $ do
      sc <- (0b1111 .&.) <$> getWord8
      ctyp <- getWord8
      ctag <- getWord16be
      styp <- getWord8
      sgt <- getWord8
      emlf_mcf_qi_sr <- getWord8
      MHAudioComponentDescriptor
        sc
        ctyp
        ctag
        styp
        sgt
        (testBit emlf_mcf_qi_sr 6)
        (shiftR emlf_mcf_qi_sr 4 .&. 0b11)
        (shiftR emlf_mcf_qi_sr 1 .&. 0b111)
        <$> get
        <*> whenMaybe (testBit emlf_mcf_qi_sr 7) get
        <*> getRemainingLazyByteString

  put = undefined

data MPUExtendedTimestampDescriptorSequence = MPUExtendedTimestampDescriptorSequence
  { mpuSequenceNumber :: Word32,
    mpuPresentationTimeLeapIndicator :: Word8,
    mpuDecodingTimeOffset :: Word16,
    au :: [Word16]
  }
  deriving (Show)

instance Binary MPUExtendedTimestampDescriptorSequence where
  get =
    MPUExtendedTimestampDescriptorSequence
      <$> getWord32be
      <*> ((`shiftR` 6) <$> getWord8)
      <*> getWord16be
      <*> (getWord8 >>= readN)

  put = undefined

data MPUExtendedTimestampDescriptorSequenceWithOffset = MPUExtendedTimestampDescriptorSequenceWithOffset
  { mpuSequenceNumber :: Word32,
    mpuPresentationTimeLeapIndicator :: Word8,
    mpuDecodingTimeOffset :: Word16,
    au :: [(Word16, Word16)]
  }
  deriving (Show)

instance Binary MPUExtendedTimestampDescriptorSequenceWithOffset where
  get =
    MPUExtendedTimestampDescriptorSequenceWithOffset
      <$> getWord32be
      <*> ((`shiftR` 6) <$> getWord8)
      <*> getWord16be
      <*> (getWord8 >>= readN)

  put = undefined

data MPUExtendedTimestampDescriptor
  = MPUExtendedTimestampDescriptorType0
      { timescale :: Maybe Word32,
        sequences :: [MPUExtendedTimestampDescriptorSequence]
      }
  | MPUExtendedTimestampDescriptorType1
      { timescale :: Maybe Word32,
        defaultPtsOffset :: Word16,
        sequences :: [MPUExtendedTimestampDescriptorSequence]
      }
  | MPUExtendedTimestampDescriptorType2
      { timescale :: Maybe Word32,
        sequencesWithOffset :: [MPUExtendedTimestampDescriptorSequenceWithOffset]
      }
  deriving (Show)

instance Binary MPUExtendedTimestampDescriptor where
  get =
    verifyDescriptorHeader 0x8026 $ do
      type_flag <- getWord8
      ts <- if testBit type_flag 0 then Just <$> getWord32be else return Nothing
      case shiftR type_flag 1 .&. 0b11 of
        0 -> do
          MPUExtendedTimestampDescriptorType0 ts <$> repeatRead get
        1 -> do
          dpo <- getWord16be
          MPUExtendedTimestampDescriptorType1 ts dpo <$> repeatRead get
        2 -> do
          MPUExtendedTimestampDescriptorType2 ts <$> repeatRead get
        _ -> fail "Invalid PTS offset type"

  put = undefined

data TimeControlMode
  = TimeControlModeTTMLDescription -- 0000
  | TimeControlModeTTMLDescriptionEHEIT -- 0001
  | TimeControlModeTTMLDescriptionReference
      { referenceStartTime :: Word64,
        referenceStartTimeLeapIndicator :: Word8
      } -- 0010
  | TimeControlModeTTMLDescriptionMPU -- 0011
  | TimeControlModeTTMLDescriptionNPT -- 0100
  | TimeControlModeMPUTimestamp -- 1000
  | TimeControlModeWithout -- 1111
  deriving (Show)

data AdditionalAribSubtitleInfo = AdditionalAribSubtitleInfo
  { subtitleTag :: Word8,
    subtitleInfoVersion :: Word8,
    iso639LanguageCode :: ISO639LanguageCode,
    subtitleType :: Word8,
    subtitleFormat :: Word8,
    opm :: Word8,
    dmf :: Word8,
    resolution :: Word8,
    compressionType :: Word8,
    startMPUSequenceNumber :: Maybe Word32,
    tmd :: TimeControlMode
  }
  deriving (Show)

instance Binary AdditionalAribSubtitleInfo where
  get = do
    tag <- getWord8
    siv_smsnflag <- getWord8

    lc <- get
    typ_sf_opm <- getWord8
    tmd_dmf <- getWord8
    res_ctype <- getWord8

    AdditionalAribSubtitleInfo
      tag
      (shiftR siv_smsnflag 4)
      lc
      (shiftR typ_sf_opm 6)
      (shiftR typ_sf_opm 2 .&. 0b1111)
      (typ_sf_opm .&. 0b11)
      -- skip tmd
      (tmd_dmf .&. 0b1111)
      (shiftR res_ctype 4)
      (res_ctype .&. 0b1111)
      <$> whenMaybe (testBit siv_smsnflag 3) get
      <*> parseTMD (shiftR tmd_dmf 4)
    where
      parseTMD :: Word8 -> Get TimeControlMode
      parseTMD 0b0000 = return TimeControlModeTTMLDescription
      parseTMD 0b0001 = return TimeControlModeTTMLDescriptionEHEIT
      parseTMD 0b0010 =
        TimeControlModeTTMLDescriptionReference
          <$> getWord64be <*> ((`shiftR` 6) <$> getWord8)
      parseTMD 0b0011 = return TimeControlModeTTMLDescriptionMPU
      parseTMD 0b0100 = return TimeControlModeTTMLDescriptionNPT
      parseTMD 0b1000 = return TimeControlModeMPUTimestamp
      parseTMD 0b1111 = return TimeControlModeWithout
      parseTMD _ = fail "Invalid time control mode"

  put = undefined

data MHDataComponentDescriptor
  = MHDataComponentClosedCaption AdditionalAribSubtitleInfo -- 0x0020
  | MHDataComponentMultimedia ByteString -- 0x0021 -- TODO
  deriving (Show)

instance Binary MHDataComponentDescriptor where
  get =
    verifyDescriptorHeader 0x8020 $ do
      typ <- getWord16be
      case typ of
        0x0020 -> MHDataComponentClosedCaption <$> get
        0x0021 -> MHDataComponentMultimedia <$> getRemainingLazyByteString
        _ -> fail "Unknown MH-data component descriptor tag"

  put = undefined

verifyDescriptorHeader :: Word16 -> Get a -> Get a
verifyDescriptorHeader tag parse = do
  t <- getWord16be
  when (t /= tag) $ fail "Unexpected descriptor tag"
  getWord8 >>= getLazyByteString . fromIntegral >>= consumeAll parse