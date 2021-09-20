{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import Network.Socket.ByteString.Lazy ( recv, sendAll )
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Concurrent (forkFinally, ThreadId)
import Yahtzee.Protocol (ClientMessage (HelloThere, SoUncivilized, YourMove), ServerMessage (YouFool, GeneralKenobi, AttackKenobi))
import Text.Read (readMaybe)
import System.Random.Stateful (Uniform(uniformM), newIOGenM, getStdGen, IOGenM, StdGen)

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
  gen <- getStdGen >>= newIOGenM
  forkFinally (messageHandler gen) errorHandler
  where
    messageHandler :: IOGenM StdGen -> IO ()
    messageHandler gen = do
      msg <- recv client 1024
      case readMaybe @ClientMessage . BS.unpack $ msg of
        Nothing -> do
          respond YouFool
        Just clientMsg -> case clientMsg of
          HelloThere -> do
            respond GeneralKenobi
            messageHandler gen
          SoUncivilized -> respond YouFool
          YourMove -> do
            dice <- uniformM gen
            respond $ AttackKenobi dice
            messageHandler gen

    respond :: ServerMessage -> IO ()
    respond = sendAll client . BS.pack . show

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
