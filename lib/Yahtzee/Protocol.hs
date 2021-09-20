{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Yahtzee.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  , Die (..)
  ) where

import System.Random.Stateful (Uniform(uniformM), UniformRange (uniformRM))

data Die
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving stock (Eq, Show, Read)

instance Uniform Die where
  uniformM g = do
    d6 <- uniformRM (0 :: Int, 5) g
    go d6
    where
      go = \case
        0 -> pure One
        1 -> pure Two
        2 -> pure Three
        3 -> pure Four
        4 -> pure Five
        5 -> pure Six
        n -> go $ n `mod` 6

data ClientMessage
  = HelloThere
  | YourMove
  | SoUncivilized
  deriving stock (Eq, Show, Read)

data ServerMessage
  = GeneralKenobi
  | AttackKenobi (Die, Die, Die, Die, Die)
  | YouFool
  deriving stock (Eq, Show, Read)
