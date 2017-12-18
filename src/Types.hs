{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveGeneric          #-}
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
  , module Data.Ecstasy
  , showTrace
  ) where

import           BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw, Handler, runHandlers, Unique, cast, loop)
import           Control.Lens hiding (Level, levels, Context, rmap, set, set', without)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Ecstasy
import qualified Data.Map as M
import           Data.Tiled.Types (Object ())
import           Game.Sequoia hiding (render, step)
import           Game.Sequoia.Keyboard (Key (..))
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

allGeometry :: Level -> [Piece]
allGeometry level = mappend (levelGeometry level)
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

-- data Controller = Controller
--   { ctrlDir     :: !V2
--   , ctrlLastDir :: !V2
--   , timeIdle    :: !Time
--   , ctrlJump    :: !Bool
--   , wantsJump   :: !Bool
--   , wantsBoost  :: !(Maybe V2)
--   , wantsGrasp  :: !Bool
--   , wantsDig    :: !Bool
--   } deriving (Show)


data Player = Player
  { _pLastInput :: [Key]
  , _pLastDir   :: V2
  , _pIdleTime  :: Time
  }

data Jump = Jump
  { _jMaxJumps :: Int
  , _jCurJumps :: Int
  , _jJumping  :: Bool
  } deriving (Show)

data CanBoost = CanBoost
  { _cbMaxBoosts :: Int
  , _cbCurBoosts :: Int
  } deriving (Show)

data Boosting = Boosting
  { _bBoostVel  :: V2
  , _bBoostTime :: Time
  } deriving (Show)

data StandContext
  = StandingOn Piece
  deriving Show


data EntWorld f = Entity
  { pos          :: Component f 'Field V2
  , vel          :: Component f 'Field V2
  , gfx          :: Component f 'Field Form
  , player       :: Component f 'Unique Player
  , jump         :: Component f 'Field Jump
  , gravity      :: Component f 'Field ()
  , geometry     :: Component f 'Field Piece
  , collision    :: Component f 'Field BoxGeom
  , wantsJump    :: Component f 'Field ()
  , wantsBoost   :: Component f 'Field V2
  , standContext :: Component f 'Field StandContext
  , canBoost     :: Component f 'Field CanBoost
  , boosting     :: Component f 'Field Boosting
  } deriving Generic

data Globals = Globals
  { _currentLevel :: Level
  }

type Entity = EntWorld 'FieldOf
type Sys = SystemT Globals EntWorld IO

makeLenses ''Level
makeLenses ''Jump
makeLenses ''CanBoost
makeLenses ''Boosting
makeLenses ''Player
makeLenses ''Globals

