{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent
import Control.Exception (SomeException, catch)
import Control.Monad
import Data.Char
import Data.List (partition, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Ord (comparing)
import qualified Network.Simple.TCP as TCP
import qualified Network.Socket as Socket
import qualified System.Console.Docopt as Docopt
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Timeout
import Text.Printf
import Text.Read

import Channel
import Types
import Util

usage :: Docopt.Docopt
usage = [Docopt.docopt|
Usage:
  server [options] <bot> <bot>

Options:
  --mapSize=<number>     default is 5
  --neutralPlanetCount=<number>  default is 5
  --turnLimit=<number>   default is 200
  <bot>  these can be local filenames, port numbers or builtin 'idle'
|]

main :: IO ()
main = do
    args <- Docopt.parseArgsOrExit usage =<< getArgs
    let arg def name =
            fromMaybe def $ do
                raw <- Docopt.getArg args (Docopt.longOption name)
                readMaybe raw
        mapSize = arg 5 "mapSize"
        neutralPlanetCount = arg 5 "neutralPlanetCount"
        turnLimit = Turn (arg 200 "turnLimit")
        bots = Docopt.getAllArgs args (Docopt.argument "bot")
    players <- launchBots bots
    let world = createWorld mapSize neutralPlanetCount
    result <- simulateMatch players world turnLimit
    print result
    mapM_ botClose players

data Bot = Bot
    { botId :: !PlayerId
    , botName :: !String
    , botInput :: !OutChannel
    , botOutput :: !InChannel
    , botClose :: IO ()
    }

launchBots :: [FilePath] -> IO [Bot]
launchBots = mapM launch . zip [1..]
    where
    launch (index, "builtin-idle") =
        return
            (Bot
                (PlayerId index)
                "builtin-idle"
                whateverChannel
                (yesChannel ".")
                (return ()))
    launch (index, port) | all isDigit port = do
        handleVar <- newEmptyMVar
        finishVar <- newEmptyMVar
        _ <- TCP.listen TCP.HostAny port $ \(listeningSocket, listeningAddr) -> do
            putStrLn $ "Listening for incoming connections at " ++ show listeningAddr
            TCP.acceptFork listeningSocket $ \(connectionSocket, remoteAddr) -> do
                putStrLn $ "Connection established from " ++ show remoteAddr
                h <- Socket.socketToHandle connectionSocket ReadWriteMode
                putMVar handleVar h
                void (takeMVar finishVar)
        h <- takeMVar handleVar
        return
            (Bot
                (PlayerId index)
                ("port-" <> port)
                (outChannelFromHandle h)
                (inChannelFromHandle h)
                (hClose h >> putMVar finishVar ()))
    launch (index, path) = do
        handles <-
            createProcess
                (proc path [])
                    { std_out = CreatePipe
                    , std_in = CreatePipe
                    , create_group = True
                    }
        case handles of
            (Just hIn, Just hOut, _, procHandle) -> do
                hSetBuffering hOut LineBuffering
                hSetBuffering hIn LineBuffering
                return
                    (Bot
                        (PlayerId index)
                        path
                        (outChannelFromHandle hIn)
                        (inChannelFromHandle hOut)
                        (catch
                            (terminateProcess procHandle)
                            (\(_ :: SomeException) -> return ())))
            _ -> do 
                print ("failed to start " ++ path)
                exitWith (ExitFailure 10)

-- TODO: fair map generation
createWorld :: Int -> Int -> [Planet]
createWorld mapSize neutralCount
    = Planet (PlanetId 0)
        (Point2D 0 mapSize) (ShipCount 1) (ShipCount 10) (PlayerId 1)
    : Planet (PlanetId 1)
        (Point2D mapSize 0) (ShipCount 1) (ShipCount 10) (PlayerId 2)
    : neutrals
    where
    neutrals = take neutralCount
        [ Planet (PlanetId (i * mapSize + j + 2))
            (Point2D i j) (ShipCount 1) (ShipCount 3) nobody
        | i <- [0 .. mapSize]
        , j <- [0 .. mapSize]
        ]

simulateMatch :: [Bot] -> [Planet] -> Turn -> IO MatchResult
simulateMatch bots initialPlanets turnLimit =
    go (World mempty initialPlanets [])
    where
    go world = do
        nextWorldOrGameOver <- simulateTurn bots world turnLimit
        case nextWorldOrGameOver of
            Right nextWorld -> go nextWorld
            Left matchResult -> return matchResult

simulateTurn :: [Bot] -> World -> Turn -> IO (Either MatchResult World)
simulateTurn bots w@(World turn planets _) _turnLimit
    | not (isEliminated (PlayerId 1) w) && isEliminated (PlayerId 2) w =
    return (Left (MatchResult
        turn planets (Just (botName (head bots))) Elimination))
simulateTurn bots w@(World turn planets _) _turnLimit
    | isEliminated (PlayerId 1) w && not (isEliminated (PlayerId 2) w) =
    return (Left (MatchResult
        turn planets (Just (botName (last bots))) Elimination))
simulateTurn _ w@(World turn planets _) _turnLimit
    | isEliminated (PlayerId 1) w && isEliminated (PlayerId 2) w =
    return (Left (MatchResult
        turn planets Nothing Elimination))
simulateTurn bots (World turn planets _) turnLimit | turn > turnLimit = do
    let winner = case compare totalPopulation1 totalPopulation2 of
            EQ -> Nothing
            LT -> Just (botName (last bots))
            GT -> Just (botName (head bots))
        totalPopulation1 =
            mconcat [plPopulation p | p <- planets, plOwner p == PlayerId 1]
        totalPopulation2 =
            mconcat [plPopulation p | p <- planets, plOwner p == PlayerId 2]
    return (Left (MatchResult turnLimit planets winner TurnLimit))
simulateTurn bots world@(World turn planets _) _ = do
    sendWorld world bots
    orders <- getOrders world bots
    case orders of
        [Right (BotMessage ls1 ps1), Right (BotMessage ls2 ps2)] -> do
            let nextWorld =
                    ( advanceTurn
                    . removeArrivedFleets
                    . resolveBattles
                    . simulateProduction
                    . executeOrders
                        [ (BotMessage ls1 ps1, PlayerId 1)
                        , (BotMessage ls2 ps2, PlayerId 2)
                        ]
                    ) world
            return (Right nextWorld)
        [Right _, Left failure] -> do
            hPrint stderr failure
            return (Left (MatchResult
                turn
                planets
                (Just (botName (head bots)))
                (Disqualification failure)))
        [Left failure, Right _] -> do
            hPrint stderr failure
            return (Left (MatchResult
                turn
                planets
                (Just (botName (last bots)))
                (Disqualification failure)))
        [Left failure1, Left failure2] -> do
            hPrint stderr failure1
            hPrint stderr failure2
            return (Left (MatchResult
                turn
                planets
                Nothing
                (Disqualification failure1)))
        _ -> error "this shouldn't happen"

sendWorld :: World -> [Bot] -> IO ()
sendWorld (World _ planets _) =
    mapM_ $ \(Bot { botId = (PlayerId me), botInput = ch }) -> do
        forM_ planets $ \(Planet (PlanetId pid) (Point2D x y) (ShipCount prod)
                            (ShipCount pop) (PlayerId owner)) ->
            sendLine ch (printf "P %d %d %d %d %d %d" pid x y prod owner pop)
        sendLine ch ("Y " <> show me)
        sendLine ch "."

advanceTurn :: World -> World
advanceTurn (World turn planets fleets) =
    (World (succ turn) planets fleets)

simulateProduction :: World -> World
simulateProduction (World turn planets fleets) =
    (World turn (map produce planets) fleets)
    where
    produce p | plOwner p == nobody = p
    produce p = p { plPopulation = plPopulation p <> plProduction p }

executeOrders :: [(BotMessage, PlayerId)] -> World -> World
executeOrders orders world = foldr executeOrder world orders
    where
    executeOrder (BotMessage launches upgrades, owner)
            (World turn planets fleets) =
        let freshFleets =
                [ Fleet
                    turn (turn <> distance src dst)
                    srcId dstId
                    n owner
                | Launch srcId dstId n <- launches
                , let Just src = getPlanetById srcId world
                , let Just dst = getPlanetById dstId world
                ]
            newPlanets = map spendShips planets
            spendShips p@(Planet pid xy (ShipCount prod) (ShipCount pop) o) =
                let upgraded = pid `elem` upgrades
                    (ShipCount spent) = (mconcat . mconcat)
                        [ [n | Launch from _to n <- launches, from == pid]
                        , [upgradeCost p | upgraded]
                        ]
                    newProd = ShipCount (if upgraded then prod + 1 else prod)
                in Planet pid xy newProd (ShipCount (pop - spent)) o
            newFleets = freshFleets <> fleets
        in World turn newPlanets newFleets

removeArrivedFleets :: World -> World
removeArrivedFleets (World turn planets fleets) =
    World turn planets (filter ((> turn) . flArrivalTurn) fleets)

resolveBattles :: World -> World
resolveBattles (World turn planets fleets) =
    World turn (map battle planets) fleets
    where
    battle (Planet pid coords prod pop owner) =
        Planet pid coords prod newPop newOwner
        where
        (newOwner, newPop) = case (winners, sortBy (comparing snd) losers) of
            ([(lonelyPlayer, lonelyForce)], []) ->
                (lonelyPlayer, lonelyForce)
            ([(winPlayer, ShipCount winForce)], (_, ShipCount contenderForce) : _) ->
                (winPlayer, ShipCount (winForce - contenderForce))
            _ ->
                if owner `elem` map fst winners
                then (owner, mempty)
                else (nobody, mempty)
        competitors = groupArmies
            ( (owner, pop)
            : [ (sender, count)
              | Fleet _launchTurn eta _srcId dstId count sender <- fleets
              , dstId == pid && eta == turn]
            )
        groupArmies = M.toList . M.fromListWith mappend
        maxForce = maximum (map snd competitors)
        (winners, losers) = partition ((== maxForce) . snd) competitors

getOrders :: World -> [Bot] -> IO [Either BotError BotMessage]
getOrders (World _ planets _) =
    mapM $ \(Bot { botId = me, botOutput = ch }) ->
        let isPlanetMine pid =
                not (null ([p | p <- planets, plId p == pid, plOwner p == me]))
            validate (Right (BotMessage launches _))
                | not (all (isPlanetMine . lSrc) launches)
                = Left (BotIllegal "attempting to launch from enemy planet")
            validate (Right (BotMessage _ upgrades))
                | not (all isPlanetMine upgrades)
                = Left (BotIllegal "attempting to upgrade enemy planet")
            validate (Right (BotMessage _ upgrades))
                | not (distinct upgrades)
                = Left (BotIllegal "attempting to upgrade twice")
            validate (Right (BotMessage launches upgrades))
                | or [ cost > pop
                     | p@(Planet pid _ _ pop _) <- planets
                     , let cost = (mconcat . mconcat)
                             [ [lCount l | l <- launches, lSrc l == pid]
                             , [upgradeCost p | pid `elem` upgrades]
                             ]
                     ]
                = Left (BotIllegal "not enough ships to execute all orders")
            validate o = o
        in fmap
            (validate . fromMaybe (Left BotTimeout))
            (timeout
                1000000 -- timeout 1 second
                (fmap
                    (either (Left . BotIllegal) Right)
                    (chReadBotMessage ch)))

chReadBotMessage :: InChannel -> IO (Either String BotMessage)
chReadBotMessage ch = chReadMessage ch
    (\inputLines ->
        Just
            (BotMessage
                [f | LaunchLine f <- inputLines]
                [p | UpgradeLine p <- inputLines]))
