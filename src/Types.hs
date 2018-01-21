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
import           Control.Monad.Trans.State (StateT)
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

data RawInput = RawInput
  { _riArrows     :: V2
  , _riWantsJump  :: Bool
  }

data BoxGeom = BoxGeom
  { leftX   :: Double
  , rightX  :: Double
  , topY    :: Double
  , bottomY :: Double
  } deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)


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

data SwoopPhase = SwoopHover | SwoopSwing
  deriving Show
data Swoop = Swoop
  { _swOffset       :: V2
  , _swPhase        :: SwoopPhase
  , _swHoverTime    :: Time
  , _swMaxHoverTime :: Time
  }

data Hitpoints = Hitpoints
  { _hpCurrent :: Int
  , _hpMax     :: Int
  }

data ParryTimer = ParryTimer
  { _ptTime   :: Time
  , _ptBoxEnt :: Ent
  , _ptEnt    :: Ent
  , _ptAction :: Action
  }


type Field f t = Component f 'Field t

data Hitbox = Hitbox
  { _hbRadius :: Maybe Double
  , _hbAction :: Action
  }

data Action
  = ActionDoNothing
  | ActionCombine        Action Action
  | ActionImpartDamage   Int
  | ActionImpartVelocity V2
  | ActionCallback       ECSF
  | ActionParryable      Action
  | ActionResetJumps

instance Monoid Action where
  mempty  = ActionDoNothing
  mappend = ActionCombine

data EntWorld f = Entity
  { pos          :: Field f V2
  , vel          :: Field f V2
  , xvel         :: Field f V2  -- exertable velocity
  , termVel      :: Field f Double
  , acc          :: Field f V2
  , gfx          :: Field f Form
  , player       :: Component f 'Unique Player
  , jump         :: Field f Jump
  , gravity      :: Field f ()
  , geometry     :: Field f Piece
  , collision    :: Field f BoxGeom
  , wantsJump    :: Field f ()
  , wantsGrasp   :: Field f ()
  , wantsBoost   :: Field f V2
  , standContext :: Field f StandContext
  , canBoost     :: Field f CanBoost
  , boosting     :: Field f Boosting
  , swoop        :: Field f Swoop
  , dangerous    :: Field f ()
  , hitbox       :: Field f Hitbox
  , hitboxable   :: Field f ()
  , hitpoints    :: Field f Hitpoints
  , parryTimer   :: Field f ParryTimer
  , heldBy       :: Field f Ent
  , wantsThrow   :: Field f V2
  } deriving Generic

data Globals = Globals
  { _currentLevel :: Level
  }

type Entity = EntWorld 'FieldOf
type Sys = SystemT EntWorld (StateT Globals IO)
type ECSF = QueryT EntWorld (StateT Globals IO) (EntWorld 'SetterOf)

makeLenses ''Level
makeLenses ''Jump
makeLenses ''CanBoost
makeLenses ''Boosting
makeLenses ''Player
makeLenses ''Globals
makeLenses ''Swoop
makeLenses ''Hitbox
makeLenses ''Hitpoints
makeLenses ''RawInput
makeLenses ''ParryTimer

