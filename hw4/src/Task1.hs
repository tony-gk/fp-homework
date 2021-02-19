{-# LANGUAGE BangPatterns #-}

module Task1
  ( Point(..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , perimeterNaive
  , perimeterStrict
  , doubleArea
  , doubleAreaNaive
  , doubleAreaStrict
  ) where


import Control.DeepSeq (NFData (..), deepseq)

-- | Representation of two-dimensional point.
data Point = Point { getX :: Int, getY :: Int }

instance NFData Point where
  rnf (Point x y) = x `deepseq` y `deepseq` ()

-- | Coordinate-wise addition of points.
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Coordinate-wise subtraction of points.
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar product of points.
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Cross product of points.
crossProduct  :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Euclidean distance between points.
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt $ fromIntegral $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

-- | Returns perimeter of polygon given by points.
perimeter  :: [Point] -> Double
perimeter = perimeterStrict

-- | Returns doubled area of polygon without sself-intersections
-- given by points in counterclockwise order.
doubleArea :: [Point] -> Int
doubleArea = doubleAreaStrict

perimeterNaive :: [Point] -> Double
perimeterNaive [] = 0.0
perimeterNaive points = foldPoints points 0.0
  where
    firstPoint = head points
    foldPoints []              _   =  0.0
    foldPoints [lst]           acc = acc + distance firstPoint lst
    foldPoints (prev:next:rst) acc = foldPoints (next:rst) (acc + distance prev next)

perimeterStrict :: [Point] -> Double
perimeterStrict [] = 0.0
perimeterStrict points = foldPoints points 0.0
  where
    firstPoint = head points
    foldPoints []               _   =  0.0
    foldPoints [lst]            acc = acc + distance firstPoint lst
    foldPoints (prev:next:rst) !acc = foldPoints (next:rst) (acc + distance prev next)

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive [] = 0
doubleAreaNaive points = abs $ foldPoints points 0
  where
    firstPoint = head points
    foldPoints []              _   = 0
    foldPoints [lst]           acc = acc + crossProduct firstPoint lst
    foldPoints (prev:next:rst) acc = foldPoints (next:rst) (acc + crossProduct prev next)

doubleAreaStrict :: [Point] -> Int
doubleAreaStrict [] = 0
doubleAreaStrict points = abs $ foldPoints points 0
  where
    firstPoint = head points
    foldPoints []               _   = 0
    foldPoints [lst]            acc = acc + crossProduct firstPoint lst
    foldPoints (prev:next:rst) !acc = foldPoints (next:rst) (acc + crossProduct prev next)
