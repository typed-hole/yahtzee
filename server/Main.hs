module Main
  ( main
  ) where

import Yahtzee.Server (runYahtzeeServer)

main :: IO ()
main = do
  runYahtzeeServer
