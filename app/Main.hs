module Main where

import Yahtzee.Server (runYahtzeeServer)

main :: IO ()
main = do
  runYahtzeeServer
