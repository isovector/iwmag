{-# LANGUAGE AllowAmbiguousTypes    #-}
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

import           BasePrelude hiding (rotate, group, (&), uncons, index, lazy, throw, Handler, runHandlers)
import           Control.Lens hiding (Level, levels, Context)
import           Control.Monad.Reader (asks, ReaderT)
import           Control.Monad.State (State)
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
  , _actors       :: M.Map Int Actor
  , levelSize     :: V2
  , _destructableGeometry :: M.Map String [Piece]
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

data HandlerContext = HContext
  { hctxTime       :: Time
  , hctxController :: Controller
  }


type Handler = ReaderT HandlerContext (State (GameState, Actor))

data Handlers = Handlers
  { _walkHandler       :: Handler ()
  , _standHandler      :: Handler ()
  , _startJumpHandler  :: Handler ()
  , _jumpHandler       :: Double -> Handler (Maybe Piece)
  , _startBoostHandler :: Handler ()
  , _boostHandler      :: V2 -> Double -> Time -> Bool -> Handler (Maybe Piece)
  , _collideHandler    :: Piece -> Handler ()
  , _hookHandler       :: Hook -> V2 -> Handler ()
  , _updateHandler     :: Handler ()
  }

instance Show Handlers where
  show _ = "Handlers"

instance Eq Handlers where
  (==) _ _ = True

data Internal where
  Internal :: a -> Internal

data Actor = Actor
  { _aPos        :: !V2
  , _aHealth     :: !Int
  , _jumpData    :: JumpData
  , _attachment  :: ActorAttachment
  , aGeom        :: BoxGeom
  , aColor       :: Color
  , graspTarget  :: GraspTarget
  , _handlers     :: Handlers
  , _self        :: ALens' Level (Maybe Actor)
  , _internal    :: Internal
  , aRender      :: Actor -> Form
  , aController  :: B Controller
  }

data BoxGeom = BoxGeom
  { leftX   :: Double
  , rightX  :: Double
  , topY    :: Double
  , bottomY :: Double
  } deriving (Show, Eq)

data JumpData = JumpData
  { _jumpState    :: !JumpState
  , _jumpsLeft    :: !Int
  , _boostsLeft   :: !Int
  , _recoveryTime :: !Time
  } deriving (Show, Eq)

data JumpState
  = Stand
  | Jump Double
  | Boost V2 Double Time Bool
  deriving (Show, Eq)

data Line = Line V2 V2 deriving (Show, Eq)
data Rect = Rect V2 V2 deriving (Show, Eq)


class KnownSymbol name => IsObject name where
  spawn :: V2 -> [(String, String)] -> Actor


type ObjectMap = M.Map String
                       ( V2
                      -> [(String, String)]
                      -> Actor
                       )


data GameState = GameState
  { _currentLevel :: Level
  , _levelName    :: String
  , _player       :: Actor
  , _camera       :: V2
  , _geometryChanged :: Bool
  , objectMap     :: ObjectMap
  , levels        :: [(String, Level)]
  }

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

makeLenses ''Level
makeLenses ''Actor
makeLenses ''GameState
makeLenses ''JumpData
makeLenses ''Handlers


hctxState :: Lens' (GameState, Actor) GameState
hctxState = _1

hctxPlayer :: Lens' (GameState, Actor) Actor
hctxPlayer = _2

hctxLevel :: Lens' (GameState, Actor) Level
hctxLevel = _1 . currentLevel

pack :: Iso' Internal a
pack = iso (\(Internal a) -> unsafeCoerce a)
           (\a -> Internal a)

