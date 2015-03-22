{-# LANGUAGE OverloadedStrings #-}

module JSON where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Scientific

import Types

instance FromJSON Replay where
    parseJSON (Object v) = Replay
        <$> v .: "result"
        <*> v .: "states"
    parseJSON _ = mzero

instance FromJSON World where
    parseJSON (Object v) = World
        <$> (Turn <$> v .: "turn")
        <*> v .: "planets"
        <*> v .: "fleets"
    parseJSON _ = mzero

instance FromJSON Planet where
    parseJSON (Object v) = Planet
        <$> v .: "id"
        <*> (Point2D <$> v .: "x" <*> v .: "y")
        <*> v .: "production"
        <*> v .: "population"
        <*> v .: "owner"
    parseJSON _ = mzero

instance FromJSON Fleet where
    parseJSON (Object v) = Fleet
        <$> v .: "launchTurn"
        <*> v .: "arrivalTurn"
        <*> v .: "source"
        <*> v .: "destination"
        <*> v .: "size"
        <*> v .: "owner"
    parseJSON _ = mzero

instance FromJSON MatchResult where
    parseJSON (Object v) = MatchResult
        <$> v .: "turn"
        <*> pure []
        <*> v .:? "winner"
        <*> pure Elimination
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

parseInt v =
    case toBoundedInteger v of
        Just x -> pure x
        _ -> error ("Expected integer but got " ++ show v)