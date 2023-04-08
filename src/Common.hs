module Common where

import Data.Binary (Binary (..), Get, Word8)
import Data.Binary.Get
  ( Decoder (..),
    isEmpty,
    pushChunks,
    pushEndOfInput,
    runGetIncremental,
  )
import qualified Data.ByteString as B
import Data.ByteString.Lazy (ByteString)

data FragmentationIndicator
  = FragmentationIndicatorUndivided
  | FragmentationIndicatorDividedHead
  | FragmentationIndicatorDividedBody
  | FragmentationIndicatorDividedEnd
  deriving (Show, Enum)

data ISO639LanguageCode = ISO639LanguageCode Word8 Word8 Word8

instance Show ISO639LanguageCode where
  show (ISO639LanguageCode a b c) =
    [ toEnum $ fromIntegral a,
      toEnum $ fromIntegral b,
      toEnum $ fromIntegral c
    ]

instance Binary ISO639LanguageCode where
  get = ISO639LanguageCode <$> get <*> get <*> get
  put (ISO639LanguageCode a b c) = put a >> put b >> put c

consumeAll :: Get a -> ByteString -> Get a
consumeAll g bs = do
  case pushEndOfInput $ runGetIncremental g `pushChunks` bs of
    Fail _ loc err -> fail $ "error at " ++ show loc ++ ": " ++ err
    Partial _ -> fail "not enough bytes"
    -- Done _ _ a -> return a
    Done r _ a -> if B.null r then return a else fail "unconsumed input"

repeatRead :: Get a -> Get [a]
repeatRead g = do
  end <- isEmpty
  if end
    then return []
    else do
      a <- g
      as <- repeatRead g
      return $ a : as

readN :: Binary a => Word8 -> Get [a]
readN 0 = return []
readN n = (:) <$> get <*> readN (n - 1)
