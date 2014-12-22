{-# LANGUAGE DeriveDataTypeable #-}

module Network.BitTorrent.ClientControl where

{-
  interface for bittorrent client controllers
-}

import Data.Word
import Data.LargeWord
import Control.Exception
import Data.Typeable

type InfoHash = Word160
data Torrent = Torrent {torrentID :: InfoHash, torrentName :: String}
  deriving Show

type PortNum = Word16


data TorrentClientException = ClientException | ServerException
  deriving (Show, Typeable)

instance Exception TorrentClientException 

data TorrentClientConn =  TorrentClientConn {

  -- basic functionality
  addMagnetLink :: String -> IO (),
  addTorrentFile :: FilePath -> IO (),
  removeTorrent :: InfoHash -> IO (),
  listTorrents :: IO [Torrent],
  pauseTorrent :: InfoHash -> IO (),
  setSettings :: [Setting] -> IO (),

  -- optional functionality
  connectToPeer :: Maybe (InfoHash -> String -> PortNum -> IO ())
}

data Setting = ProxySetType ProxyType | ProxyIP String | ProxyP2P Bool | ProxyPort PortNum 
               | DHTNetwork Bool | UTP Bool | PeerExchange Bool | LocalPeerDiscovery Bool
               | DHTForNewTorrents Bool | UPnP Bool | NATPMP Bool | RandomizePort Bool
               | BindPort Word16
  deriving (Show, Eq)

data ProxyType = None | Socks4 | Socks5 | HTTPS | HTTP deriving (Enum, Show, Eq)

