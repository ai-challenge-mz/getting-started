
module Util
    ( MessageLine (..)
    , chReadMessage
    , chReadUntilDot
    , classifyLine
    , distance
    , distinct
    , getPlanetById
    , isEliminated
    , isUpgradable
    , lexLine
    , nobody
    , readMessage
    , readUntilDot
    , upgradeCost
    , removeEmptyLaunches
    ) where

import Prelude hiding (log)

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe

import Channel
import Types

data MessageLine
    = PlanetLine !Planet
    | MyIdLine !PlayerId
    | UpgradeLine !PlanetId
    | LaunchLine !Launch

readMessage :: ([MessageLine] -> Maybe a) -> IO (Either String a)
readMessage = chReadMessage stdinChannel

chReadMessage :: InChannel -> ([MessageLine] -> Maybe a) -> IO (Either String a)
chReadMessage ch messageFromLines = do
    inputOrError <- chReadUntilDot ch
    case inputOrError of
        Right input -> do
            let inputLines = mapMaybe (classifyLine <=< lexLine) (lines input)
            return (maybe (Left input) Right (messageFromLines inputLines))
        Left exc -> return (Left (show exc))

lexLine :: String -> Maybe (String, [Int])
lexLine l = case words l of
    [] -> Nothing
    (c : rest) -> Just (c, map read rest)

classifyLine :: (String, [Int]) -> Maybe MessageLine
classifyLine ("P", [ident, x, y, prod, owner, pop])
    = Just $ PlanetLine $ Planet
        { plId = PlanetId ident
        , plCoords = Point2D x y
        , plProduction = ShipCount prod
        , plPopulation = ShipCount pop
        , plOwner = PlayerId owner
        }
classifyLine ("Y", [me]) = Just (MyIdLine (PlayerId me))
classifyLine ("B", [p]) = Just (UpgradeLine (PlanetId p))
classifyLine ("F", [src, dst, amount]) =
    Just (LaunchLine (Launch (PlanetId src) (PlanetId dst) (ShipCount amount)))
classifyLine _ = Nothing

readUntilDot :: IO (Either IOError String)
readUntilDot = chReadUntilDot stdinChannel

chReadUntilDot :: InChannel -> IO (Either IOError String)
chReadUntilDot h = try (fmap (intercalate "\n" . reverse) (go []))
    where
    go acc = do
        l <- receiveLine h
        case l of
          "." -> return acc
          _ -> go (l : acc)

distance :: Planet -> Planet -> Turn
distance p1 p2 = Turn (ceiling (sqrt distanceSquared))
    where
    distanceSquared :: Double
    distanceSquared = fromIntegral (dx * dx + dy * dy)
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
    Point2D x1 y1 = plCoords p1
    Point2D x2 y2 = plCoords p2

upgradeCost :: Planet -> ShipCount
upgradeCost p = ShipCount (2 ^ (abs prod))
    where
    (ShipCount prod) = plProduction p

isUpgradable :: Planet -> Bool
isUpgradable p = plPopulation p >= upgradeCost p

nobody :: PlayerId
nobody = PlayerId 0

getPlanetById :: PlanetId -> World -> Maybe Planet
getPlanetById pid (World _ planets _) =
    listToMaybe [p | p <- planets, plId p == pid]

distinct :: Ord a => [a] -> Bool
distinct [] = True
distinct xs = and (zipWith (/=) xs' (tail xs'))
    where xs' = sort xs

isEliminated :: PlayerId -> World -> Bool
isEliminated player (World _ planets fleets) =
    all ((/= player) . plOwner) planets
    &&
    all ((/= player) . flOwner) fleets

removeEmptyLaunches :: BotMessage -> BotMessage
removeEmptyLaunches (BotMessage launches upgrades) =
    BotMessage
        (filter ((/= ShipCount 0) . lCount) launches)
        upgrades