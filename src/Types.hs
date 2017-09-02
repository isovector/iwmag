{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Types
  ( module Types
  , module Game.Sequoia
  , module Linear.Vector
  , module BasePrelude
  , module Control.Lens
  ) where

import Control.Lens hiding (Level, levels)
import BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw)
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
  , _objects     :: [Object]
  , levelSize    :: V2
  } deriving Show


data Zone = Death   Rect
          | NoBoost Rect
          | DoorZ   Door
          deriving Show
data Door = Door Rect String deriving Show

data ActorAttachment
  = Unattached
  | StandingOn Line
  | Grasping Hook V2
  deriving (Eq, Show)

data GraspTarget
  = Unarmed
  | Holding
    { updateHeld :: Time -> Actor -> Level -> Level
    , onThrow    :: Actor -> V2 -> Level -> Level
    }

instance Eq GraspTarget where
  (==) _ _ = True

instance Show GraspTarget where
  show _ = "GraspTarget"

data Actor = Actor
  { _aPos        :: !V2
  , _aHealth     :: !Int
  , jumpState    :: !JumpState
  , jumpsLeft    :: !Int
  , boostsLeft   :: !Int
  , recoveryTime :: !Time
  , attachment   :: ActorAttachment
  , aGeom        :: BoxGeom
  , aColor       :: Color
  , graspTarget  :: GraspTarget
  } deriving (Show, Eq)

data BoxGeom = BoxGeom
  { leftX   :: Double
  , rightX  :: Double
  , topY    :: Double
  , bottomY :: Double
  } deriving (Show, Eq)



data JumpState
  = Stand
  | Jump Double
  | Boost V2 Double Bool
  deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)

data Object where
  Object ::
    { obj       :: a
    , renderObj :: a -> Form
    , updateObj :: Time -> Level -> Actor -> a -> (a, Actor -> Actor)
    , graspObj  :: ATraversal' Level Object -> Actor -> a -> Maybe (a, GraspTarget)
    } -> Object

instance Show Object where
  show _ = "Object"

class KnownSymbol name => IsObject name where
  type InternalObj name = r | r -> name
  spawn :: V2 -> [(String, String)] -> InternalObj name
  render :: InternalObj name -> Form
  update :: Time -> Level -> Actor -> InternalObj name -> (InternalObj name, Actor -> Actor)
  grasp  :: ATraversal' Level Object -> Actor -> InternalObj name -> Maybe (InternalObj name, GraspTarget)

data GameState = GameState
  { currentLevel :: !Level
  , levelName    :: !String
  , player       :: !Actor
  , camera       :: !V2
  }

data RawController = RawController
  { rctrlDir        :: !V2
  , rctrlJump       :: !Bool
  , rctrlWantsJump  :: !Bool
  , rctrlWantsGrasp :: !Bool
  }

data Controller = Controller
  { ctrlDir     :: !V2
  , ctrlLastDir :: !V2
  , timeIdle    :: !Time
  , ctrlJump    :: !Bool
  , wantsJump   :: !Bool
  , wantsBoost  :: !(Maybe V2)
  , wantsGrasp  :: !Bool
  } deriving (Show)

makeLenses ''Level
makeLenses ''Actor

