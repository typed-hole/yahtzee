{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingVia #-}

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
import Control.Exception.Safe (bracket, bracketOnError, Exception (displayException), SomeException, MonadCatch, MonadMask, MonadThrow)
import Network.Socket.ByteString.Lazy ( recv, sendAll )
import Control.Monad (forever, (>=>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Yahtzee.Protocol (ClientMessage (HelloThere, SoUncivilized, YourMove, You'reNotHelpingHere), ServerMessage (YouFool, GeneralKenobi, AttackKenobi), RerollDecision (..), KeepOrReroll (Keep, Reroll), Die)
import Text.Read (readMaybe)
import System.Random.Stateful (Uniform(uniformM), newIOGenM, getStdGen, IOGenM, StdGen)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import UnliftIO.Async ( waitCatch, withAsync )
import Control.Monad.Reader.Class (MonadReader, asks)
import UnliftIO (MonadUnliftIO)

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
    runServer (waitForConnections socket) ServerEnv
      { randomGen
      }
  where
    exit socket = do
      putStrLn "Goodnight"
      close socket

newtype Server a = Server { runServer :: ServerEnv -> IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadReader ServerEnv
    ) via ReaderT ServerEnv IO

waitForConnections :: Socket -> Server void
waitForConnections socket =
    forever
    . bracketOnError (liftIO $ accept socket) (liftIO . close . fst)
    $ serveClient

newtype ServerEnv = ServerEnv
  { randomGen :: IOGenM StdGen
  }

serveClient :: (Socket, SockAddr) -> Server ()
serveClient (client, clientAddr) = do
  liftIO . putStrLn . mconcat $
    [ "Serving new client: "
    , show clientAddr
    ]
  withAsync messageHandler (waitCatch >=> errorHandler)
  where
    messageHandler :: Server ()
    messageHandler = do
      msg <- liftIO $ recv client 1024
      liftIO $ BS.putStrLn msg
      case readMaybe @ClientMessage . BS.unpack $ msg of
        Nothing -> do
          respond YouFool
        Just clientMsg -> case clientMsg of
          HelloThere -> do
            respond GeneralKenobi
            messageHandler
          SoUncivilized -> respond YouFool
          You'reNotHelpingHere (fst, snd, trd, frt, fft) -> do
            fst' <- rerollMaybe (keepOrReroll fst) (die fst)
            snd' <- rerollMaybe (keepOrReroll snd) (die snd)
            trd' <- rerollMaybe (keepOrReroll trd) (die trd)
            frt' <- rerollMaybe (keepOrReroll frt) (die frt)
            fft' <- rerollMaybe (keepOrReroll fft) (die fft)
            respond $ AttackKenobi (fst', snd', trd', frt', fft')
            messageHandler
          YourMove -> do
            gen <- asks randomGen
            dice <- liftIO $ uniformM gen
            respond $ AttackKenobi dice
            messageHandler

    rerollMaybe :: KeepOrReroll -> Die -> Server Die
    rerollMaybe Keep = pure
    rerollMaybe Reroll = const $ do
      gen <- asks randomGen
      liftIO $ uniformM gen

    respond :: ServerMessage -> Server ()
    respond = liftIO . sendAll client . BS.pack . show

    errorHandler :: Either SomeException () -> Server ()
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
