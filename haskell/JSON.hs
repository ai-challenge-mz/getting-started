{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import qualified Data.Vector as V

import Types

instance FromJSON Replay where
    parseJSON (Object v) = Replay
        <$> v .: "names"
        <*> v .: "result"
        <*> v .: "states"
    parseJSON _ = mzero

instance FromJSON World where
    parseJSON (Object v) = World
        <$> (Turn <$> v .: "turn")
        <*> v .: "planets"
        <*> v .: "fleets"
    parseJSON _ = mzero

instance FromJSON Planet where
    parseJSON (Array s) = 
        let [pid, x, y, production, population, owner] =
                map valueToInt (V.toList s)
        in pure (Planet
            (PlanetId pid)
            (Point2D x y)
            (ShipCount production)
            (ShipCount population)
            (PlayerId owner))
    parseJSON _ = mzero

instance FromJSON Fleet where
    parseJSON (Array s) = 
        let [launchTurn, arrivalTurn, src, dst, size, owner] =
                map valueToInt (V.toList s)
        in pure (Fleet
            (Turn launchTurn)
            (Turn arrivalTurn)
            (PlanetId src)
            (PlanetId dst)
            (ShipCount size)
            (PlayerId owner))
    parseJSON _ = mzero

instance FromJSON MatchResult where
    parseJSON (Object v) = MatchResult
        <$> v .: "turn"
        <*> pure []
        <*> v .:? "winner"
        <*> v .: "gameover"
    parseJSON _ = mzero

instance FromJSON Turn where
    parseJSON (Number v) = Turn <$> parseInt v
    parseJSON _ = mzero

instance FromJSON PlanetId where
    parseJSON (Number v) = PlanetId <$> parseInt v
    parseJSON _ = mzero

instance FromJSON PlayerId where
    parseJSON (Number v) = PlayerId <$> parseInt v
    parseJSON _ = mzero

instance FromJSON ShipCount where
    parseJSON (Number v) = ShipCount <$> parseInt v
    parseJSON _ = mzero

instance FromJSON GameOverType where
    parseJSON v@(String t) = case t of
        "Elimination" -> pure Elimination
        "TurnLimit" -> pure TurnLimit
        _ -> Disqualification <$> parseJSON v

instance FromJSON BotError where
    parseJSON (String t) = case t of
        "Timeout" -> pure BotTimeout
        _ -> pure (BotIllegal (T.unpack t))

valueToInt (Number v) =
    case toBoundedInteger v of
        Just x -> x
        _ -> error ("Expected integer but got " ++ show v)

parseInt v =
    case toBoundedInteger v of
        Just x -> pure x
        _ -> error ("Expected integer but got " ++ show v)

instance ToJSON Replay where
    toJSON (Replay names result states) = object
        [ "names" .= names
        , "result" .= result
        , "states" .= states
        ]

instance ToJSON World where
    toJSON (World turn planets fleets) = object
        [ "turn" .= turn
        , "planets" .= planets
        , "fleets" .= fleets
        ]

instance ToJSON Planet where
    toJSON (Planet (PlanetId pid) (Point2D x y)
        (ShipCount prod) (ShipCount pop) (PlayerId owner)) =
        toJSON [pid, x, y, prod, pop, owner]

instance ToJSON Fleet where
    toJSON (Fleet (Turn launchTurn) (Turn destinationTurn)
        (PlanetId src) (PlanetId dst)
        (ShipCount size) (PlayerId owner)) =
        toJSON [launchTurn, destinationTurn, src, dst, size, owner]

instance ToJSON MatchResult where
    toJSON (MatchResult turn planets winner gameover) = object
        [ "turn" .= turn
        , "planets" .= planets
        , "winner" .= winner
        , "gameover" .= gameover
        ]

instance ToJSON Turn where
    toJSON (Turn t) = toJSON t

instance ToJSON PlanetId where
    toJSON (PlanetId t) = toJSON t

instance ToJSON PlayerId where
    toJSON (PlayerId t) = toJSON t

instance ToJSON ShipCount where
    toJSON (ShipCount t) = toJSON t

instance ToJSON GameOverType where
    toJSON Elimination = "Elimination"
    toJSON TurnLimit = "TurnLimit"
    toJSON (Disqualification botError) = toJSON botError

instance ToJSON BotError where
    toJSON BotTimeout = "Timeout"
    toJSON (BotIllegal reason) = toJSON reason