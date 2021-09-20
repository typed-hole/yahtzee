{-# LANGUAGE DerivingStrategies #-}
module Yahtzee.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  ) where

data ClientMessage
  = HelloThere
  | SoUncivilized
  deriving stock (Eq, Show, Read)

data ServerMessage
  = GeneralKenobi
  | YouFool
  deriving stock (Eq, Show, Read)
