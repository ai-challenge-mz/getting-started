
module Types where

import Data.Monoid

data Point2D = Point2D {pX :: !Int, pY :: !Int}
    deriving Show

newtype PlayerId = PlayerId Int
    deriving (Eq, Ord, Show)

newtype PlanetId = PlanetId Int
    deriving (Eq, Ord, Show)

newtype ShipCount = ShipCount Int
    deriving (Eq, Ord, Show)

instance Monoid ShipCount where
    mempty = ShipCount 0
    mappend (ShipCount x) (ShipCount y) = ShipCount (x + y)

data Planet = Planet
    { plId :: !PlanetId
    , plCoords :: !Point2D
    , plProduction :: !ShipCount
    , plPopulation :: !ShipCount
    , plOwner :: !PlayerId
    } deriving Show

data MonitorMessage = MonitorMessage
    { mmPlanets :: ![Planet]
    , mmMe :: !PlayerId
    } deriving Show

data Launch = Launch
    { lSrc :: !PlanetId
    , lDst :: !PlanetId
    , lCount :: !ShipCount
    } deriving Show

data BotMessage = BotMessage
	{ bmLaunch :: ![Launch]
    , bmUpgrades :: ![PlanetId]
	} deriving Show

data BotError
    = BotTimeout
    | BotIllegal String
    deriving Show

data GameOverType
    = Elimination
    | TurnLimit
    | Disqualification BotError
    deriving Show

newtype Turn = Turn Int
    deriving (Eq, Ord, Show)

instance Enum Turn where
    succ (Turn x) = Turn (x + 1)
    fromEnum (Turn x) = x
    toEnum = Turn

instance Monoid Turn where
    mempty = Turn 0
    mappend (Turn x) (Turn y) = Turn (x + y)

data MatchResult = MatchResult
    { _mrEndTurn :: !Turn
    , _mrPlanets :: ![Planet]
    , _mrWinner :: !(Maybe FilePath)
    , _mrGameOverType :: !GameOverType
    }

instance Show MatchResult where
    show (MatchResult turn planets winner gameOver) = unlines
        ( show turn
        : case winner of
            Just name -> "Winner: " <> name
            Nothing -> "Draw"
        : "Game over: " <> show gameOver
        : map show planets
        )

data Fleet = Fleet
    { flDepartureTurn :: !Turn
    , flArrivalTurn :: !Turn
    , _flSource :: !PlanetId
    , _flDestination :: !PlanetId
    , _flSize :: !ShipCount
    , flOwner :: !PlayerId
    } deriving Show

data World = World Turn [Planet] [Fleet]

instance Show World where
    show (World turn planets fleets) =
        unlines (show turn : map show planets ++ map show fleets)

data Replay = Replay
    { repNames :: ![String]
    , repResult :: !MatchResult
    , repStates :: ![World]
    } deriving Show
