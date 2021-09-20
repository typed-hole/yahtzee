{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
module Yahtzee.Client
  ( runYahtzeeClient
  ) where

import Control.Exception (bracket)
import Network.Socket (openSocket, close, getAddrInfo, defaultHints, connect, SockAddr (SockAddrInet), tupleToHostAddress, gracefulClose)
import System.Exit (exitFailure)
import Network.Socket.ByteString (sendAll, recv)
runYahtzeeClient :: IO ()
runYahtzeeClient = do
  addrInfo <- getAddrInfo (Just defaultHints) (Just "localhost") Nothing >>= \case
    [] -> do
      putStrLn "Failed to resolve addrInfo"
      exitFailure
    addr:_whatever -> pure addr
  bracket (openSocket addrInfo) close $ \socket -> do
    connect socket $ SockAddrInet 3_000 (tupleToHostAddress (127, 0, 0, 1))
    sendAll socket "HELLO_THERE"
    "GENERAL_KENOBI" <- recv socket (2^10)
    sendAll socket "YOUR_MOVE"
    "YOU_FOOL" <- recv socket (2^10)
    gracefulClose socket 1_000
