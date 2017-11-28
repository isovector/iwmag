{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Types
  ( module Types
  , module Game.Sequoia
  , module Linear.Vector
  , module BasePrelude
  , module Control.Lens
  , module Control.Monad.IO.Class
  , module Apecs
  , showTrace
  ) where

import           Apecs
import           BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw, Handler, runHandlers, Unique, cast, loop)
import           Control.Lens hiding (Level, levels, Context, rmap, set, set', without)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as M
import           Data.Tiled.Types (Object ())
import           Game.Sequoia hiding (render, step)
import           Game.Sequoia.Utils (showTrace)
import           Linear.Vector hiding (E (..))


data Piece = Wall
  { pieceLine     :: Line
  , pieceColor    :: Color
  , pieceGroup    :: String
  , pieceFriction :: Double
  }
  deriving (Eq, Show)

data Hook = Hook { hookPos :: V2 }
  deriving (Eq, Show)

data Level = Level
  { levelGeometry :: [Piece]
  , forms         :: [Form]
  , playerSpawn   :: V2
  , deathZones    :: [Rect]
  , noBoostZones  :: [Rect]
  , doors         :: [Door]
  , targets       :: [Hook]
  , levelSize     :: V2
  , _destructableGeometry :: M.Map String [Piece]
  , levelDudes :: [Object]
  }

geometry :: Level -> [Piece]
geometry level = mappend (levelGeometry level)
               . mconcat
               . fmap snd
               . M.toList
               $ _destructableGeometry level


data Zone = Death   Rect
          | NoBoost Rect
          | DoorZ   Door
          deriving Show
data Door = Door Rect String deriving Show

data GrabType
  = Ungrabbable
  | Carry
  | DoAction
  deriving (Eq, Show, Ord, Enum, Bounded)

-- data ActorAttachment
--   = Unattached
--   | StandingOn Piece
--   | Grasping Hook V2
--   deriving (Eq, Show)

data BoxGeom = BoxGeom
  { leftX   :: Double
  , rightX  :: Double
  , topY    :: Double
  , bottomY :: Double
  } deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)


data RawController = RawController
  { rctrlDir        :: !V2
  , rctrlJump       :: !Bool
  , rctrlWantsJump  :: !Bool
  , rctrlWantsGrasp :: !Bool
  , rctrlWantsDig   :: !Bool
  }

data Controller = Controller
  { ctrlDir     :: !V2
  , ctrlLastDir :: !V2
  , timeIdle    :: !Time
  , ctrlJump    :: !Bool
  , wantsJump   :: !Bool
  , wantsBoost  :: !(Maybe V2)
  , wantsGrasp  :: !Bool
  , wantsDig    :: !Bool
  } deriving (Show)


data Collision = Collision { getCollision :: BoxGeom }
instance Component Collision where
  type Storage Collision = Map Collision

data Geometry = Geometry { getPiece :: Piece }
instance Component Geometry where
  type Storage Geometry = Map Geometry

data Pos = Pos { getPos :: V2 } deriving Show
instance Component Pos where
  type Storage Pos = Map Pos

data Vel = Vel { getVel :: V2 } deriving Show
instance Component Vel where
  type Storage Vel = Map Vel

data Player = Player
instance Component Player where
  type Storage Player = Unique Player

data Gfx = Gfx { getGfx :: Form }
instance Component Gfx where
  type Storage Gfx = Map Gfx

data Jump = Jump
  { _jMaxJumps :: Int
  , _jCurJumps :: Int
  , _jJumping  :: Bool
  } deriving (Show)
instance Component Jump where
  type Storage Jump = Map Jump

data CanBoost = CanBoost
  { _cbMaxBoosts :: Int
  , _cbCurBoosts :: Int
  } deriving (Show)
instance Component CanBoost where
  type Storage CanBoost = Map CanBoost

data Boosting = Boosting
  { _bBoostVel  :: V2
  , _bBoostTime :: Time
  } deriving (Show)
instance Component Boosting where
  type Storage Boosting = Map Boosting

data Gravity = Gravity
instance Component Gravity where
  type Storage Gravity = Set Gravity
instance Flag Gravity where
  flag = Gravity

data WantsJump = WantsJump
instance Component WantsJump where
  type Storage WantsJump = Set WantsJump
instance Flag WantsJump where
  flag = WantsJump

data WantsBoost = WantsBoost { getBoostDir :: V2 } deriving Show
instance Component WantsBoost where
  type Storage WantsBoost = Map WantsBoost

data CurLevel = CurLevel Level
instance Component CurLevel where
  type Storage CurLevel = Global CurLevel
instance Monoid CurLevel where
  mempty = error "you gotta set the level dingus"
  mappend = error "this is a dumb interface"

data StandContext
  = StandingOn Piece
  deriving Show
  -- | Grasping Hook V2
instance Component StandContext where
  type Storage StandContext = Map StandContext


-- destructable flag

makeWorld "World"
  [ ''Pos
  , ''Gfx
  , ''Player
  , ''Gravity
  , ''Geometry
  , ''Collision
  , ''Vel
  , ''StandContext
  , ''CurLevel
  , ''Jump
  , ''WantsJump
  , ''CanBoost
  , ''WantsBoost
  , ''Boosting
  ]

type Sys = System World

makeLenses ''Level
makeLenses ''Jump
makeLenses ''CanBoost
makeLenses ''Boosting

