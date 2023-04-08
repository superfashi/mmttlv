module Main (main) where

import Control.Monad.Trans.Class (lift)
import Data.Binary (get)
import Data.Binary.Get (Decoder (..), runGetIncremental)
import qualified Data.ByteString as B (null)
import qualified Data.ByteString.Lazy as L (readFile)
import qualified Data.ByteString.Lazy.Internal as L (ByteString (Chunk, Empty), chunk)
import Data.Int (Int64)
import Lib (TLVPacket)
import ListT (ListT, cons)
import qualified ListT (head)
import Text.Show.Pretty (pPrint)

parseTLVPackets :: MonadFail m => Decoder TLVPacket -> (Int64, L.ByteString) -> ListT m TLVPacket
parseTLVPackets (Fail _ loc e) (br, _) = lift $ fail $ "error parse at " ++ show (br + loc) ++ ": " ++ e
parseTLVPackets (Done r _ p) (_, L.Empty) | B.null r = return p
parseTLVPackets (Done r o p) (br, input) = cons p $ parseTLVPackets (runGetIncremental get) (o + br, L.chunk r input)
parseTLVPackets (Partial k) (br, L.Empty) = parseTLVPackets (k Nothing) (br, L.Empty)
parseTLVPackets (Partial k) (br, L.Chunk bs input) = parseTLVPackets (k (Just bs)) (br, input)

main :: IO ()
main = do
  file <- L.readFile "F:\\29999.mmts"
  let packets = parseTLVPackets (runGetIncremental get) (0, file)
  len <- ListT.head packets
  pPrint len

-- list <- toList $ parseTLVPackets (runGetIncremental get) file
-- print list

-- mapM_ print $ runGet (parseLimitPacket 2) input
