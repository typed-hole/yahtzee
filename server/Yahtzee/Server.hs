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
import System.Random.Stateful (Uniform(uniformM), newIOGenM, getStdGen, IOGenM, StdGen)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import UnliftIO.Async ( waitCatch, withAsync )
import Control.Monad.Reader.Class (MonadReader, asks)
import UnliftIO (MonadUnliftIO, IORef, newIORef, modifyIORef', readIORef)
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Lens.Traversal (traverseOf)
import Control.Lens.Combinators (Each(each))
import Data.Function ((&))

runYahtzeeServer :: IO ()
runYahtzeeServer = withSocketsDo $ do
  addrInfo <- getAddrInfo (Just defaultHints) (Just "localhost") (Just "3000") >>= \case
    [] -> do
      putStrLn "Failed to resolve addrInfo"
      exitFailure
    addr:_whatever -> pure addr
  sessions <- newIORef Map.empty
  bracket (openSocket addrInfo) exit $ \socket -> do
    bind socket (addrAddress addrInfo)
    listen socket 1_024
    randomGen <- getStdGen >>= newIOGenM
    runServer (waitForConnections socket) ServerEnv
      { randomGen
      , sessions
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

data Session = Session

data ServerEnv = ServerEnv
  { randomGen :: IOGenM StdGen
  , sessions :: IORef (Map SockAddr Session)
  }

initSession :: SockAddr -> Server ()
initSession clientAddr = do
  sessionMap <- asks sessions
  modifyIORef' sessionMap $ Map.insert clientAddr Session

destroySession :: SockAddr -> Server ()
destroySession clientAddr = do
  sessionMap <- asks sessions
  modifyIORef' sessionMap $ Map.delete clientAddr

assertSession :: SockAddr -> Server ()
assertSession clientAddr = do
  sessionMap <- asks sessions >>= readIORef
  case Map.lookup clientAddr sessionMap of
    Just Session -> pure ()
    Nothing -> error "No session for client"

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
      case JSON.decode @ClientMessage $ msg of
        Nothing -> do
          respond YouFool
        Just clientMsg -> case clientMsg of
          HelloThere -> do
            initSession clientAddr
            respond GeneralKenobi
            messageHandler
          SoUncivilized -> do
            assertSession clientAddr
            respond YouFool
          You'reNotHelpingHere decisions -> do
            assertSession clientAddr
            decisions' <- decisions & traverseOf each rerollMaybe
            respond $ AttackKenobi decisions'
            messageHandler
          YourMove -> do
            assertSession clientAddr
            gen <- asks randomGen
            dice <- liftIO $ uniformM gen
            respond $ AttackKenobi dice
            messageHandler

    rerollMaybe :: RerollDecision -> Server Die
    rerollMaybe (RerollDecision {keepOrReroll, die}) = case keepOrReroll of
      Keep -> pure die
      Reroll -> do
        gen <- asks randomGen
        liftIO $ uniformM gen

    respond :: ServerMessage -> Server ()
    respond = liftIO . sendAll client . JSON.encode

    errorHandler :: Either SomeException () -> Server ()
    errorHandler = \case
      Left err -> do
        liftIO $ putStrLn . mconcat $
          ["Failed to serve client: "
          , displayException err
          ]
        destroySession clientAddr
        liftIO $ gracefulClose client 1_000
      Right () -> do
        liftIO . putStrLn . mconcat $
          [ "Successfully served client: "
          , show clientAddr
          ]
        liftIO $ gracefulClose client 1_000
