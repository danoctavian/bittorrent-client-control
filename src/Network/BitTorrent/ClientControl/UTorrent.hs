{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.ClientControl.UTorrent (
    makeUTorrentConn 
  )where
 
import Network.HTTP.Conduit
import Network.URL
import Data.Maybe
import Data.Text as DT
import Data.Text.Encoding
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 as BSLC
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Prelude as P
import Data.ByteString.Char8 as BSC
import Data.Aeson as JSON
import Data.HashMap.Strict
import Data.Text as DT
import Data.Vector as DV (toList, (!))
import Foreign.Marshal.Utils as FMU
import Control.Monad
import Control.Exception
import Data.ByteString as BS
import Network.HTTP.Client.MultipartFormData
import Data.Binary as Bin
import Data.List
import Data.Scientific

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Data.Word

import Network.BitTorrent.ClientControl
import Network.BitTorrent.Types


actionParam = "action"
hashParam = "hash"

logger = "utorrentapi"

data UTorrentConn = UTorrentConn {
                    baseURL :: URL
                  , user :: String
                  , pass :: String
                  , cookies :: CookieJar}
  deriving Show


makeUTorrentConn hostName portNum (user, pass) = do
  debugM logger $ "attempting connection to " ++ (show $ utServerURL  hostName portNum)
  conn <- uTorentConn (utServerURL hostName portNum) user pass
  return $ TorrentClientConn { addMagnetLink = addUrl conn
                             , listTorrents = list conn
                             , startTorrent = start conn
                             , forceStartTorrent = forceStart conn
                             , stopTorrent = stop conn
                             , pauseTorrent = pause conn
                             , unpauseTorrent = unpause conn
                             , setSettings = settings conn
                             , connectToPeer = Nothing
                             , addTorrentFile = addFile conn
                             , removeTorrent = rmTorrent conn
                             , removeTorrentWithData = rmTorrentWithData conn
                             , setJobProperties = setJobProps conn
                           }

uTorentConn baseUrl user pass = do
  let url = (fromJust . importURL $ baseUrl) {url_path = "gui/"}
  let conn = UTorrentConn url user pass (createCookieJar [])
  res <- makeRequest  conn (\url -> url {url_path = "gui/token.html"}) return
  debugM logger $ ("response  from utserver is " ++) $  (show res)
  return $ UTorrentConn (add_param url ("token", getToken (BSLC.unpack $ responseBody res)))
                        user pass (responseCookieJar res)

makeRequest conn urlChange reqChange = do
  request <-parseUrl $ exportURL $ urlChange $ baseURL conn
  completeReq <- reqChange $ (applyBasicAuth (BSC.pack $ user conn)
            (BSC.pack $ pass conn) (request { cookieJar = Just $ cookies conn }))
  debugM logger (show completeReq)
  withManager $ \manager -> httpLbs completeReq manager
 

requestWithParams conn params reqChange = fmap responseBody $ makeRequest conn
                  (\url -> P.foldl (\u p -> add_param u p) url params) reqChange

-- ACTION IMPLEMENTATIONS

addUrl conn url = requestWithParams conn [("s", url), (actionParam, "add-url")] return
                  >> return () 

-- action on a certain infohash
hashAction action conn hash = requestWithParams conn
                              [ (actionParam, action)
                              , (hashParam, infoHashToString hash)] return
                              >> return ()

pause = hashAction "pause"
unpause = hashAction "unpause"
start =  hashAction "start"
forceStart =  hashAction "forcestart"
stop =  hashAction "stop"
rmTorrent = hashAction "removetorrent"
rmTorrentWithData = hashAction "removedatatorrent"

addFile conn filePath = requestWithParams conn [(actionParam, "add-file")]
                        (formDataBody [partFile "torrent_file" filePath])
                        >> return ()

list conn
  = fmap ((P.map (\(Array a) -> torrentFromArray a))
      . (\(Array a) -> DV.toList a) . fromJust . (Data.HashMap.Strict.lookup "torrents")
      . fromJust . (\s -> JSON.decode s :: Maybe Object))
      $ requestWithParams conn [("list", "1")] return


torrentFromArray a = Torrent {
    torrentID = Bin.decode $ toLazy $ fromJust $ textToInfoHash $ fromAesonStr $ a DV.! 0
  , torrentName = DT.unpack $ fromAesonStr $ a DV.! 2
  , progress =  numToInt $ a DV.! 4
  , upSpeed = numToInt $ a DV.! 8
  , downSpeed = numToInt $ a DV.! 9
  , peersConnected = numToInt $ a DV.! 12
  , seedsConnected = numToInt $ a DV.! 14
}

numToInt = fromJust . toBoundedInteger . fromAesonNum 

settings conn settings = setProps conn "setsetting" [] settings settingToParam

setJobProps conn infoHash props
  = setProps conn "setprops" [(hashParam, infoHashToString infoHash)] props jobPropToParam 

settingToParam (ProxySetType proxyType) = ("proxy.type", show . fromEnum $ proxyType)
settingToParam (ProxyIP ip) =  ("proxy.proxy", ip)
settingToParam (ProxyP2P isP2P) = ("proxy.p2p", boolSetting isP2P)
settingToParam (ProxyPort n) = ("proxy.port", show n) 
settingToParam (DHTNetwork b) = ("dht", boolSetting b) 
settingToParam (TransportDisposition outTCP outUTP inTCP inUTP)
  = ("bt.transp_disposition"
    , show $ (P.sum $ P.map (\(i,b) -> 2 ^ i * (fromBool b :: Int))
                     $ P.zip [0..]  [outTCP, outUTP, inTCP, inUTP])) 
settingToParam (PeerExchange b) = ("pex", boolSetting b) 
settingToParam (LocalPeerDiscovery b) = ("lsd", boolSetting b) 
settingToParam (LimitLocalPeerBandwidth b) = ("rate_limit_local_peers", boolSetting b) 
settingToParam (UploadSlotsPerTorrent n) = ("ul_slots_per_torrent", show n) 
settingToParam (DHTForNewTorrents b) = ("dht_per_torrent", boolSetting b) 
settingToParam (UPnP b) = ("upnp", boolSetting b) 
settingToParam (NATPMP b) = ("natpmp", boolSetting b) 
settingToParam (RandomizePort b) = ("rand_port_on_start", boolSetting b)
settingToParam (BindPort n) = ("bind_port", show n)


setProps conn action others props propToParam = if (props == [])  then (return ()) else do
  requestWithParams conn (P.reverse $ ((actionParam, action) : others) P.++
       (join $ P.map ((\(s, v) -> [("s", s), ("v", v)]) . propToParam) props)) return
  return ()


jobPropToParam (DownloadRate r) = ("dlrate", show r)
jobPropToParam (UploadRate r) = ("ulrate", show r)

utServerURL :: String -> Word16 -> String
utServerURL hostName p = "http://" P.++ hostName P.++ ":" P.++ (show p)

dummyHash :: InfoHash
dummyHash = Bin.decode $ BSLC.replicate 20 '0'

-- utils
getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

boolSetting b = show $ (fromBool $ b :: Int) 
fromAesonStr (String s) = s
fromAesonNum (Number n) = n
toLazy bs = BSL.fromChunks [bs]

