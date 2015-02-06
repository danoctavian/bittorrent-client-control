{-# LANGUAGE DeriveDataTypeable #-}

module Network.BitTorrent.ClientControl where

{-
  interface for bittorrent client controllers
-}

import Data.Word
import Control.Exception
import Data.Typeable

import Network.BitTorrent.Types

data Torrent = Torrent {torrentID :: InfoHash, torrentName :: String}
  deriving Show



data TorrentClientException = ClientException | ServerException
  deriving (Show, Typeable)

instance Exception TorrentClientException 

data TorrentClientConn =  TorrentClientConn {

  -- basic functionality
  addMagnetLink :: String -> IO (),
  addTorrentFile :: FilePath -> IO (),
  removeTorrent :: InfoHash -> IO (),
  removeTorrentWithData :: InfoHash -> IO (),
  listTorrents :: IO [Torrent],
  pauseTorrent :: InfoHash -> IO (),
  setSettings :: [Setting] -> IO (),
  setJobProperties :: InfoHash -> [JobProperty] -> IO (),
  -- optional functionality
  connectToPeer :: Maybe (InfoHash -> String -> PortNum -> IO ())
}

data Setting = ProxySetType ProxyType | ProxyIP String | ProxyP2P Bool | ProxyPort PortNum 
               | DHTNetwork Bool
               | TransportDisposition { outgoingTCP :: Bool
                                      , outgoingUTP :: Bool 
                                      , incomingTCP :: Bool
                                      , incomingUTP :: Bool
                                    }

               | PeerExchange Bool | LocalPeerDiscovery Bool
               | LimitLocalPeerBandwidth Bool -- limits the bandwith for local peers as well
               | UploadSlotsPerTorrent Int 
               | DHTForNewTorrents Bool | UPnP Bool | NATPMP Bool | RandomizePort Bool
               | BindPort Word16
  deriving (Show, Eq)

data ProxyType = None | Socks4 | Socks5 | HTTPS | HTTP deriving (Enum, Show, Eq)

data JobProperty = DownloadRate Int | UploadRate Int -- bytes per second
  deriving (Show, Eq)
                


