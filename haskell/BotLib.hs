
module BotLib
    ( runStatefulBot
    , runStatelessBot
    , Log (..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.IO
import System.Exit

import Channel
import Types
import Util

data Log = Log | NoLog

runStatelessBot :: (MonitorMessage -> BotMessage) -> Log -> IO ()
runStatelessBot react = runStatefulBot (\input () -> (react input, ())) ()

runStatefulBot :: (MonitorMessage -> state -> (BotMessage, state)) -> state -> Log -> IO ()
runStatefulBot react initialState isLogEnabled = do
    let putLog a = case isLogEnabled of
                    Log -> hPutStrLn stderr ("bot> " ++ show a)
                    NoLog -> return ()
    hSetBuffering stderr LineBuffering
    let go oldState = do
            putLog "waiting"
            minput <- readMonitorMessage
            case minput of
                Right input -> do
                    putLog ("got", input)
                    let (output, newState) = react input oldState
                    putLog ("sending", output)
                    sendBotMessage output
                    go newState
                Left msg -> do
                    putStrLn ("Unrecognized message: " ++ msg)
                    exitWith (ExitFailure 1)
    go initialState

readMonitorMessage :: IO (Either String MonitorMessage)
readMonitorMessage = chReadMonitorMessage stdinChannel

chReadMonitorMessage :: InChannel -> IO (Either String MonitorMessage)
chReadMonitorMessage ch = chReadMessage ch
    (\inputLines ->
        MonitorMessage [p | PlanetLine p <- inputLines]
            <$> listToMaybe [me | MyIdLine me <- inputLines])

sendBotMessage :: BotMessage -> IO ()
sendBotMessage = chSendBotMessage stdoutChannel

chSendBotMessage :: OutChannel -> BotMessage -> IO ()
chSendBotMessage ch (BotMessage launches upgrades) = do
    forM_ launches $ \(Launch (PlanetId src) (PlanetId dst) (ShipCount amount)) ->
        sendLine ch (unwords ("F" : map show [src, dst, amount]))
    forM_ upgrades $ \(PlanetId p) ->
        sendLine ch ("B " ++ show p)
    sendLine ch "."