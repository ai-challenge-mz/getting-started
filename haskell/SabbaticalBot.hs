
import BotLib
import Types
import Util

main :: IO ()
main = runStatefulBot react 1 NoLog

react :: MonitorMessage -> Int -> (BotMessage, Int)
react _ turn | turn `rem` 7 == 6 = (BotMessage [] [], turn + 1)
react (MonitorMessage planets me) turn
    = (BotMessage [] upgrades, turn + 1)
    where
    upgrades = [plId p | p <- planets, plOwner p == me, isUpgradable p]