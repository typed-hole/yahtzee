{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Yahtzee.Server
  ( runYahtzeeServer
  ) where

import Prelude hiding (getContents)
import Network.Socket
    ( defaultHints,
      getAddrInfo,
      openSocket,
      withSocketsDo,
      gracefulClose,
      accept,
      bind,
      listen,
      close,
      AddrInfo(addrAddress),
      Socket, SockAddr )
import System.Exit (exitFailure)
import Control.Exception (bracket, bracketOnError, Exception (displayException), SomeException)
import Network.Socket.ByteString.Lazy ( recv, sendAll, getContents )
import Control.Monad (forever, unless)
import qualified Data.ByteString.Lazy as BS
import Control.Concurrent (forkFinally, ThreadId)
import Data.Void (absurd)

runYahtzeeServer :: IO ()
runYahtzeeServer = withSocketsDo $ do
  addrInfo <- getAddrInfo (Just defaultHints) (Just "localhost") (Just "3000") >>= \case
    [] -> do
      putStrLn "Failed to resolve addrInfo"
      exitFailure
    addr:_whatever -> pure addr
  bracket (openSocket addrInfo) exit $ \socket -> do
    bind socket (addrAddress addrInfo)
    listen socket 1_024
    waitForConnections socket
  where
    exit socket = do
      putStrLn "Goodnight"
      close socket

waitForConnections :: Socket -> IO void
waitForConnections socket =
    forever
    . bracketOnError (accept socket) (close . fst)
    $ serveClient

serveClient :: (Socket, SockAddr) -> IO ThreadId
serveClient (client, clientAddr) = do
  putStrLn . mconcat $
    [ "Serving new client: "
    , show clientAddr
    ]
  forkFinally messageHandler errorHandler
  where
    messageHandler :: IO ()
    messageHandler = do
      bytes <- recv client 1_024
      unless (BS.null bytes) $ do
        sendAll client bytes
        messageHandler

    errorHandler :: Either SomeException () -> IO ()
    errorHandler = \case
      Left err -> do
        putStrLn . mconcat $
          ["Failed to serve client: "
          , displayException err
          ]
        gracefulClose client 1_000
      Right () -> do
        putStrLn . mconcat $
          [ "Successfully served client: "
          , show clientAddr
          ]
        gracefulClose client 1_000
