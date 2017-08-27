{-# LANGUAGE NoImplicitPrelude #-}

module Types
  ( module Types
  , module Game.Sequoia
  , module Linear.Vector
  , module BasePrelude
  ) where

import BasePrelude hiding (rotate, group)
import Game.Sequoia
import Linear.Vector hiding (E (..))
import GHC.TypeLits


data Piece = Wall Line Color

data Hook = Hook { hookPos :: V2 }
  deriving (Eq, Show)

data Level = Level
  { geometry     :: [Line]
  , forms        :: [Form]
  , playerSpawn  :: V2
  , deathZones   :: [Rect]
  , noBoostZones :: [Rect]
  , doors        :: [Door]
  , targets      :: [Hook]
  , objects      :: [Object]
  } deriving Show

data Zone = Death   Rect
          | NoBoost Rect
          | DoorZ   Door
          deriving Show
data Door = Door Rect String deriving Show

data PlayerAttachment
  = Unattached
  | StandingOn Line
  | Grasping Hook V2
  deriving (Eq, Show)


data Player = Player
  { pPos         :: !V2
  , jumpState    :: !JumpState
  , jumpsLeft    :: !Int
  , boostsLeft   :: !Int
  , recoveryTime :: !Time
  , attachment   :: PlayerAttachment
  } deriving (Show)



data JumpState
  = Stand
  | Jump Double
  | Boost V2 Double
  deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)

data Object where
  Object ::
    { obj       :: a
    , renderObj :: a -> Form
    , updateObj :: Time -> Level -> Player -> a -> a
    } -> Object

instance Show Object where
  show _ = "Object"

class KnownSymbol name => IsObject name where
  data InternalObj name
  spawn :: V2 -> [(String, String)] -> InternalObj name
  render :: InternalObj name -> Form
  update :: Time -> Level -> Player -> InternalObj name -> InternalObj name

data GameState = GameState
  { currentLevel :: !Level
  , player       :: !Player
  , camera       :: !V2
  }
