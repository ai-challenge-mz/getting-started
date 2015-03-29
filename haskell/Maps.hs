
module Maps
    ( small, smallInverted
    , medium, mediumInverted
    , large
    ) where

import Types
import Util

small :: [Planet]
small
    = Planet (PlanetId 0)
        (Point2D 3 3) (ShipCount 1) (ShipCount 10) (PlayerId 1)
    : Planet (PlanetId 1)
        (Point2D (-3) (-3)) (ShipCount 1) (ShipCount 10) (PlayerId 2)
    : neutrals
    where
    neutrals = 
        [ Planet (PlanetId 3) (Point2D 2 (-2)) (ShipCount 3) (ShipCount 5) nobody
        , Planet (PlanetId 4) (Point2D 1 1) (ShipCount 1) (ShipCount 3) nobody
        , Planet (PlanetId 5) (Point2D 0 0) (ShipCount (-3)) (ShipCount 0) nobody
        , Planet (PlanetId 6) (Point2D (-1) (-1)) (ShipCount 1) (ShipCount 3) nobody
        , Planet (PlanetId 7) (Point2D (-2) (2)) (ShipCount 3) (ShipCount 5) nobody
        ]

smallInverted :: [Planet]
smallInverted = invertMap small

medium :: [Planet]
medium
    = Planet (PlanetId 0)
        (Point2D 0 1) (ShipCount (-5)) (ShipCount 50) (PlayerId 1)
    : Planet (PlanetId 1)
        (Point2D 0 (-1)) (ShipCount (-5)) (ShipCount 50) (PlayerId 2)
    : neutrals
    where
    neutrals = concat
        [ [ Planet
                (PlanetId (2 + i))
                (Point2D (i - size `div` 2) (- size - 1))
                (ShipCount 3) (ShipCount 10) nobody
          | i <- [0 .. size]
          ]
        , [ Planet
                (PlanetId (3 + size + i))
                (Point2D (i - size `div` 2) (size + 1))
                (ShipCount 3) (ShipCount 10) nobody
          | i <- [0 .. size]
          ]
        , [ Planet
                (PlanetId (4 + 2 * size + i))
                (Point2D (size + 1) (i - size `div` 2))
                (ShipCount 1) (ShipCount 3) nobody
          | i <- [0 .. size]
          ]
        , [ Planet
                (PlanetId (5 + 3 * size + i))
                (Point2D (- size - 1) (i - size `div` 2))
                (ShipCount 1) (ShipCount 3) nobody
          | i <- [0 .. size]
          ]
        ]
    size = 6

mediumInverted :: [Planet]
mediumInverted = invertMap medium

invertMap :: [Planet] -> [Planet]
invertMap = map invert
    where
    invert p@(Planet {plProduction = ShipCount prod}) =
        p {plProduction = ShipCount (- prod)}

large :: [Planet]
large =
    [ Planet
        (PlanetId (i * (size + 1) + j))
        (Point2D (i * 7) (j * 7))
        (ShipCount 1)
        (ShipCount 3)
        (PlayerId (1 + (i + j) `rem` 2))
    | i <- [0 .. size]
    , j <- [0 .. size]
    ]
    where size = 7