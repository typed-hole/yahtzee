{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Control.Exception.Safe (bracket, bracketOnError, Exception (displayException), SomeException)
import Network.Socket.ByteString.Lazy ( recv, sendAll )
import Control.Monad (forever, (>=>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Yahtzee.Protocol (ClientMessage (HelloThere, SoUncivilized, YourMove), ServerMessage (YouFool, GeneralKenobi, AttackKenobi))
import Text.Read (readMaybe)
import System.Random.Stateful (Uniform(uniformM), newIOGenM, getStdGen, IOGenM, StdGen)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), asks)
import Control.Monad.IO.Class (MonadIO(liftIO))
import UnliftIO.Async ( waitCatch, withAsync )

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
    randomGen <- getStdGen >>= newIOGenM
    runReaderT (waitForConnections socket) ServerEnv
      { randomGen
      }
  where
    exit socket = do
      putStrLn "Goodnight"
      close socket

waitForConnections :: Socket -> ReaderT ServerEnv IO void
waitForConnections socket =
    forever
    . bracketOnError (liftIO $ accept socket) (liftIO . close . fst)
    $ serveClient

newtype ServerEnv = ServerEnv
  { randomGen :: IOGenM StdGen
  }

serveClient :: (Socket, SockAddr) -> ReaderT ServerEnv IO ()
serveClient (client, clientAddr) = do
  liftIO . putStrLn . mconcat $
    [ "Serving new client: "
    , show clientAddr
    ]
  withAsync messageHandler (waitCatch >=> errorHandler)
  where
    messageHandler :: ReaderT ServerEnv IO ()
    messageHandler = do
      msg <- liftIO $ recv client 1024
      case readMaybe @ClientMessage . BS.unpack $ msg of
        Nothing -> do
          respond YouFool
        Just clientMsg -> case clientMsg of
          HelloThere -> do
            respond GeneralKenobi
            messageHandler
          SoUncivilized -> respond YouFool
          YourMove -> do
            gen <- asks randomGen
            dice <- liftIO $ uniformM gen
            respond $ AttackKenobi dice
            messageHandler

    respond :: ServerMessage -> ReaderT ServerEnv IO ()
    respond = liftIO . sendAll client . BS.pack . show

    errorHandler :: Either SomeException () -> ReaderT ServerEnv IO ()
    errorHandler = liftIO . \case
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
