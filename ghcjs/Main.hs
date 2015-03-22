{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import JavaScript.Canvas hiding (Left, Right)
import JavaScript.JQuery (select)
import GHCJS.DOM (runWebGUI, webViewGetDomDocument)
import GHCJS.DOM.Document (documentGetBody, documentCreateElement, documentGetElementById)
import GHCJS.DOM.Element (elementSetAttribute)
import GHCJS.DOM.Node (nodeAppendChild)
import GHCJS.DOM.HTMLElement (htmlElementGetInnerHTML, htmlElementSetInnerHTML)
import GHCJS.DOM.HTMLPreElement (castToHTMLPreElement)
import GHCJS.Types (castRef)
import GHCJS.Foreign (indexArray)
import Control.Concurrent

import Types
import Util
import JSON ()

screenWidth, screenHeight, border, planetSize, fleetSize :: Num a => a
planetSize = 32
fleetSize = 28
screenWidth = 800
screenHeight = 600
border = 30

data PlanetVM = PlanetVM (Double, Double) Int Int
    deriving Show
data FleetVM = FleetVM (Double, Double) (Double, Double) Double Int Int
    deriving Show
type BoundingBox = (Int, Int, Int, Int)

data WorldVM = WorldVM
    { _vmTurn :: Turn
    , _vmPlanets :: [PlanetVM]
    , _vmFleets :: [FleetVM]
    } deriving Show

main :: IO ()
main = runWebGUI $ \webView -> do
    putStrLn "Starting"
    Just document <- webViewGetDomDocument webView
    Just body <- documentGetBody document
    Just pre <- fmap castToHTMLPreElement <$> documentGetElementById document "json" 
    _canvas <- attachCanvas body document
    resultElem <- attachResult body document
    jsonString <- htmlElementGetInnerHTML pre :: IO String
    putStrLn "ohai"
    let replay = A.eitherDecode (B.pack jsonString) :: Either String Replay
    case replay of
        Left err -> htmlElementSetInnerHTML resultElem err
        Right (Replay result states) -> do
            putStrLn "Success!"
            htmlElementSetInnerHTML resultElem (show result)

            -- TODO: this should be possible without jquery
            -- ctx <- getContext canvas
            ctx <- getContext =<< indexArray 0 . castRef =<< select (T.pack "#theCanvas")

            void . forkIO . forM_ states $ \world -> do
                catch
                    (drawWorld ctx (projectWorld world))
                    (\(e :: SomeException) -> do
                        print e
                        htmlElementSetInnerHTML resultElem (show e))
                threadDelay 100000

attachCanvas body document = do
    Just canvas <- documentCreateElement document ("canvas" :: String)
    elementSetAttribute canvas "id" "theCanvas"
    elementSetAttribute canvas "width" (show (screenWidth :: Int))
    elementSetAttribute canvas "height" (show (screenHeight :: Int))
    nodeAppendChild body (Just canvas)

attachResult body document = do
    Just pre <- documentCreateElement document ("pre" :: String)
    void (nodeAppendChild body (Just pre))
    return (castToHTMLPreElement pre)

exampleWorlds :: [World]
exampleWorlds =
    map (\i -> World
            (Turn i)
            [ Planet
                (PlanetId 1)
                (Point2D 0 0)
                (ShipCount 1)
                (ShipCount (1 + i))
                (PlayerId 1)
            , Planet
                (PlanetId 2)
                (Point2D 6 8)
                (ShipCount 1)
                (ShipCount (1 + i))
                (PlayerId 2)
            ]
            [Fleet
                (Turn (10 * div i 10))
                (Turn (10 * (div i 10 + 1)))
                (PlanetId 1)
                (PlanetId 2)
                (ShipCount i)
                (PlayerId 1)])
        [1..1000]

drawWorld :: Context -> WorldVM -> IO ()
drawWorld ctx (WorldVM turn planets fleets) = do
    save ctx
    fillStyle 255 255 255 1 ctx
    fillRect 0 0 800 600 ctx
    textAlign Center ctx
    textBaseline Middle ctx
    font (T.pack "15px verdana") ctx
    mapM_ (drawPlanet ctx) planets
    mapM_ (drawFleet ctx) fleets
    drawLegend ctx turn
    restore ctx

drawFleet :: Context -> FleetVM -> IO ()
drawFleet ctx (FleetVM (x1, y1) (x2, y2) progress owner size) = do
    save ctx
    let x = x1 * (1.0 - progress) + x2 * progress
    let y = y1 * (1.0 - progress) + y2 * progress
    translate x y ctx
    let (r, g, b) = colorForOwner owner
    strokeStyle r g b 1 ctx
    strokeRect (- (fleetSize / 2)) (- (fleetSize / 2)) fleetSize fleetSize ctx
    fillStyle 0 0 0 1 ctx
    fillText (showT size) 0 0 ctx
    restore ctx

drawPlanet :: Context -> PlanetVM -> IO ()
drawPlanet ctx (PlanetVM (x, y) owner pop) = do
    save ctx
    translate x y ctx
    let (r, g, b) = colorForOwner owner
    fillStyle r g b 1 ctx
    fillRect (- (planetSize / 2)) (- (planetSize / 2)) planetSize planetSize ctx
    fillStyle 255 255 255 1 ctx
    fillText (showT pop) 0 0 ctx
    restore ctx

drawLegend :: Context -> Turn -> IO ()
drawLegend ctx (Turn turn) = do
    save ctx
    translate (screenWidth / 2) (screenHeight - border / 2) ctx
    fillStyle 0 0 0 1 ctx
    fillText (showT turn) 0 0 ctx
    restore ctx

colorForOwner :: Int -> (Int, Int, Int)
colorForOwner 0 = (200, 200, 200)
colorForOwner 1 = (200, 0, 0)
colorForOwner _ = (0, 200, 50)

projectWorld :: World -> WorldVM
projectWorld world@(World turn planets fleets) =
    WorldVM
        turn
        (fmap (projectPlanet boundingBox) planets)
        (fmap (projectFleet turn world boundingBox) fleets)
    where
    boundingBox =
        (edge minimum pX, edge maximum pX, edge minimum pY, edge maximum pY)
    edge f g = f (map (g . plCoords) planets)

projectPlanet :: BoundingBox -> Planet -> PlanetVM
projectPlanet bb p =
    PlanetVM coords owner pop
    where
    coords = projectPoint bb (plCoords p)
    PlayerId owner = plOwner p
    ShipCount pop = plPopulation p

projectFleet :: Turn -> World -> BoundingBox -> Fleet -> FleetVM
projectFleet (Turn turn) world bb
        (Fleet (Turn srcTurn) (Turn dstTurn) srcId dstId (ShipCount size) (PlayerId pl)) =
    FleetVM
        (projectPoint bb (plCoords src))
        (projectPoint bb (plCoords dst))
        (interp srcTurn turn dstTurn)
        pl
        size
    where
    Just src = getPlanetById srcId world
    Just dst = getPlanetById dstId world

projectPoint :: BoundingBox -> Point2D -> (Double, Double)
projectPoint (left, right, top, bottom) (Point2D x y) =
     ( border + width * interp left x right
     , border + height * interp top y bottom
     )
     where
     width = screenWidth - 2 * border
     height = screenHeight - 2 * border

interp :: Int -> Int -> Int -> Double
interp lo value hi = fromIntegral (value - lo) / fromIntegral (hi - lo)

showT :: Show a => a -> T.Text
showT = T.pack . show