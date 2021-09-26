{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Yahtzee.Protocol
  ( ClientMessage (..)
  , ServerMessage (..)
  , Die (..)
  , Dice
  , RerollDecision (..)
  , RerollDecisions
  , KeepOrReroll (..)
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON )
import System.Random.Stateful (Uniform(uniformM), UniformRange (uniformRM))
import Data.Bool (bool)

data Die
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving stock (Eq, Show, Read)
$(deriveJSON defaultOptions ''Die)

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

data KeepOrReroll
  = Keep
  | Reroll
  deriving stock (Eq, Show, Read)
$(deriveJSON defaultOptions ''KeepOrReroll)

data RerollDecision = RerollDecision
  { die :: Die
  , keepOrReroll :: KeepOrReroll
  }
  deriving stock (Eq, Show, Read)
$(deriveJSON defaultOptions ''RerollDecision)

type RerollDecisions = (RerollDecision, RerollDecision, RerollDecision, RerollDecision, RerollDecision)
type Dice = (Die, Die, Die, Die, Die)

instance Uniform KeepOrReroll where
  uniformM = fmap (bool Reroll Keep) . uniformM

data ClientMessage
  = HelloThere
  | YourMove
  | You'reNotHelpingHere RerollDecisions
  | SoUncivilized
  deriving stock (Eq, Show, Read)
$(deriveJSON defaultOptions ''ClientMessage)

data ServerMessage
  = GeneralKenobi
  | AttackKenobi Dice
  | YouFool
  deriving stock (Eq, Show, Read)
$(deriveJSON defaultOptions ''ServerMessage)
