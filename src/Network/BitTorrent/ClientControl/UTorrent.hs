{-# LANGUAGE OverloadedStrings #-}

module Network.BitTorrent.ClientControl.UTorrent where
 
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

import System.Log.Logger
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import Data.Word

import Network.BitTorrent.ClientControl


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
  return $ TorrentClientConn {addMagnetLink = addUrl conn, listTorrents = list conn,
                              pauseTorrent = pause conn, setSettings = settings conn,
                              connectToPeer = Nothing,
                              addTorrentFile = addFile conn}

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

-- implementation for operations
addUrl conn url = requestWithParams conn [("s", url), (actionParam, "add-url")] return
                  >> return () 

pause conn hash
  = requestWithParams conn [(hashParam, show hash), (actionParam, "pause")] return
    >> return ()

addFile conn filePath = requestWithParams conn [(actionParam, "add-file")]
                        (formDataBody [partFile "torrent_file" filePath])
                        >> return ()

list conn
  = fmap ((P.map (\(Array a) ->
      Torrent (Bin.decode $ toLazy $ encodeUtf8 $ fromAesonStr $ a DV.! 0)
              (DT.unpack $ fromAesonStr $ a DV.! 2)) )
      . (\(Array a) -> DV.toList a) . fromJust . (Data.HashMap.Strict.lookup "torrents")
      . fromJust . (\s -> JSON.decode s :: Maybe Object))
      $ requestWithParams conn [("list", "1")] return

settings conn settings =if (settings == []) then (return ()) else do
  requestWithParams conn (P.reverse $ (actionParam, "setsetting") :
       (join $ P.map ((\(s, v) -> [("s", s), ("v", v)]) . settingToParam) settings)) return
  return ()

settingToParam (ProxySetType proxyType) = ("proxy.type", show . fromEnum $ proxyType)
settingToParam (ProxyIP ip) =  ("proxy.proxy", ip)
settingToParam (ProxyP2P isP2P) = ("proxy.p2p", boolSetting isP2P)
settingToParam (ProxyPort n) = ("proxy.port", show n) 
settingToParam (DHTNetwork b) = ("dht", boolSetting b) 
settingToParam (UTP b) = ("bt.transp_disposition", boolSetting b) 
settingToParam (PeerExchange b) = ("pex", boolSetting b) 
settingToParam (LocalPeerDiscovery b) = ("lsd", boolSetting b) 
settingToParam (DHTForNewTorrents b) = ("dht_per_torrent", boolSetting b) 
settingToParam (UPnP b) = ("upnp", boolSetting b) 
settingToParam (NATPMP b) = ("natpmp", boolSetting b) 
settingToParam (RandomizePort b) = ("rand_port_on_start", boolSetting b)
settingToParam (BindPort n) = ("bind_port", show n)


utServerURL :: String -> Word16 -> String
utServerURL hostName p = "http://" P.++ hostName P.++ ":" P.++ (show p)

dummyHash :: InfoHash
dummyHash = Bin.decode $ BSLC.replicate 20 '0'

-- utils
getToken :: String -> String 
getToken = (\(TagText t) -> t) . (!! 2) . parseTags 

boolSetting b = show $ (fromBool $ b :: Int) 
fromAesonStr (String s) = s
toLazy bs = BSL.fromChunks [bs]

runTorrentClientScript = do
  updateGlobalLogger logger (setLevel DEBUG)
  conn <- makeUTorrentConn "127.0.0.1" 9000  ("admin", "")
  debugM logger "made first connection"
  r3 <- setSettings conn [UPnP False, NATPMP False, RandomizePort False, DHTForNewTorrents False, UTP True, LocalPeerDiscovery False, ProxySetType Socks4, ProxyIP "127.0.0.1", ProxyPort 1080, ProxyP2P True]
  debugM logger $ show $  r3
  torrentFile <-return "/home/dan/testdata/sample100.torrent"
  addMagnetLink conn archMagnet
  addTorrentFile conn torrentFile 
  r <- listTorrents conn
  debugM logger $ "list of torrents  is  " ++  (show r)
  return ()

archMagnet = "magnet:?xt=urn:btih:67f4bcecdca3e046c4dc759c9e5bfb2c48d277b0&dn=archlinux-2014.03.01-dual.iso&tr=udp://tracker.archlinux.org:6969&tr=http://tracker.archlinux.org:6969/announce"

{-
  HTTP calls notes:

set settings calls

set proxy to socks5
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.type&v=2

set port
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.port&v=9998


set address
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.proxy&v=127.0.1.1

set p2p proxy 
http://localhost:8080/gui/?token=6pvMv-pFjjA6IcGkotXcW8hMfNwu5hPeBMLksFaQo_ACDFD8_N4yiHf0JFM=&action=setsetting&s=proxy.p2p&v=0
-}
