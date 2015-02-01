module Network.BitTorrent.Types (
    textToInfoHash
  , infoHashToString
  , InfoHash
  , PortNum
) where

import Data.Text as T
import Data.Text.Encoding as T
import Data.ByteString.Base16 as Base16
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Binary as Bin
import Data.LargeWord

type InfoHash = Word160
type PortNum = Word16

textToInfoHash :: Text -> Maybe ByteString
textToInfoHash text
    | hashLen == 40 = if BS.length inv == 0 then Just ihStr else Nothing
    | otherwise = Nothing
  where
    hashLen = BS.length hashStr
    hashStr = T.encodeUtf8 text
    (ihStr, inv) = Base16.decode hashStr

infoHashToString :: InfoHash -> String
infoHashToString = BSC.unpack . Base16.encode . BSL.toStrict . Bin.encode
