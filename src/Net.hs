module Net where

import Data.Binary (Word32, Word64)

type IPv4Addr = Word32

type IPv6Addr = (Word64, Word64)