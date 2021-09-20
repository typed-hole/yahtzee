{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Yahtzee.Client
  ( runYahtzeeClient
  ) where

import Control.Exception (bracket, finally)
import Network.Socket (openSocket, close, getAddrInfo, defaultHints, connect, SockAddr (SockAddrInet), tupleToHostAddress, gracefulClose, Socket)
import System.Exit (exitFailure)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Char8 as BS
import Yahtzee.Protocol (ServerMessage (GeneralKenobi, YouFool, AttackKenobi))
import Text.Read (readMaybe)

runYahtzeeClient :: IO ()
runYahtzeeClient = do
  addrInfo <- getAddrInfo (Just defaultHints) (Just "localhost") Nothing >>= \case
    [] -> do
      putStrLn "Failed to resolve addrInfo"
      exitFailure
    addr:_whatever -> pure addr
  bracket (openSocket addrInfo) exit $ \socket -> do
    connect socket $ SockAddrInet 3_000 (tupleToHostAddress (127, 0, 0, 1))
    talkToServer socket `finally` gracefulClose socket 1_000
  where
    exit socket = do
      putStrLn "goodbye"
      close socket

talkToServer :: Socket -> IO ()
talkToServer server = go
  where
    go = do
      msg <- BS.getLine
      sendAll server msg
      response <- recv server 1024
      BS.putStrLn response
      case readMaybe @ServerMessage . BS.unpack $ response of
        Nothing -> error "Bad server message"
        Just serverMsg -> case serverMsg of
          GeneralKenobi -> go
          YouFool -> pure ()
          AttackKenobi _dice -> go
