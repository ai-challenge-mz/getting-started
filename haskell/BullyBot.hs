
import Control.Monad
import Data.List (maximumBy, minimumBy, partition)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)

import Types
import BotLib

threshold :: Int
threshold = 18

react :: MonitorMessage -> BotMessage
react (MonitorMessage planets me@(PlayerId myId)) =
    BotMessage launches []
    where
    (myPlanets, enemyPlanets) = partition ((== me) . plOwner) planets
    launches = maybeToList $ do
        guard (not (null myPlanets))
        guard (not (null enemyPlanets))
        let (Planet srcId _ _ (ShipCount srcPop) _) =
                maximumBy (comparing plPopulation) myPlanets
            (Planet dstId _ _ _ _) =
                minimumBy (comparing plPopulation) enemyPlanets
        guard (srcPop > threshold)
        return (Launch srcId dstId (ShipCount (srcPop `div` 2)))

main :: IO ()
main = runStatelessBot react NoLog