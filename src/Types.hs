{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Types
  ( module Types
  , module Game.Sequoia
  , module Linear.Vector
  , module BasePrelude
  , module Control.Lens
  , asks
  ) where

import           BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw)
import           Control.Lens hiding (Level, levels, Context)
import           Control.Monad.Reader (Reader, asks)
import qualified Data.Map as M
import           GHC.TypeLits
import           Game.Sequoia
import           Linear.Vector hiding (E (..))


data Piece = Wall
  { pieceLine  :: Line
  , pieceColor :: Color
  , pieceGroup :: String
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
  , _objects      :: M.Map Int Object
  , levelSize     :: V2
  , _destructableGeometry :: M.Map String [Piece]
  } deriving Show

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

data ActorAttachment
  = Unattached
  | StandingOn Piece
  | Grasping Hook V2
  deriving (Eq, Show)

data GraspTarget
  = Unarmed
  | Holding
    { updateHeld :: Time -> Actor -> GameState -> GameState
    , onThrow    :: Actor -> V2 -> GameState -> GameState
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
  | Boost V2 Double Time Bool
  deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)

data Object where
  Object ::
    { obj       :: a
    , renderObj :: a -> Form
    , updateObj :: Time -> a -> Context (a, GameState -> GameState)
    , graspObj  :: a -> Context (Maybe (a, GraspTarget, GameState -> GameState))
    , objLens   :: ALens' Level (Maybe Object)
    , objProps  :: [(String, String)]
    } -> Object

instance Show Object where
  show _ = "Object"

data ObjectContext = ObjectContext
  { ctxLens   :: ALens' Level (Maybe Object)
  , ctxGameState :: GameState
  , ctxProps  :: [(String, String)]
  }

ctxPlayer :: ObjectContext -> Actor
ctxPlayer = _player . ctxGameState

ctxLevel :: ObjectContext -> Level
ctxLevel = _currentLevel . ctxGameState

type Context = Reader ObjectContext

class KnownSymbol name => IsObject name where
  type InternalObj name = r | r -> name
  spawn :: V2 -> Context (InternalObj name)
  render :: InternalObj name -> Form

  update :: Time -> InternalObj name -> Context (InternalObj name, GameState -> GameState)
  update _  o = pure (o, id)

  grasp  :: InternalObj name -> Context (Maybe (InternalObj name, GraspTarget, GameState -> GameState))
  grasp = const $ pure Nothing

data GameState = GameState
  { _currentLevel :: !Level
  , _levelName    :: !String
  , _player       :: !Actor
  , _camera       :: !V2
  , _geometryChanged :: !Bool
  , objectMap :: M.Map String
                       ( ALens' Level (Maybe Object)
                      -> V2
                      -> [(String, String)]
                      -> Object
                       )
  , _nextLevel    :: Maybe String
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
makeLenses ''GameState

