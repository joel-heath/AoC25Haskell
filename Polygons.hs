{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Polygons (
    Rectangle,
    rectangle,
    minMin, minMax, maxMin, maxMax,
    area,
    Line,
    line,
    lineIntersectsRectangle,
    edges,
) where

import Grid (Point(..), getX, getY)

data Rectangle = Rectangle { minMin :: Point, minMax :: Point, maxMin :: Point, maxMax :: Point }

rectangle :: Point -> Point -> Rectangle
rectangle p1 p2 =
  let xMin = min (x p1) (x p2)
      yMin = min (y p1) (y p2)
      xMax = max (x p1) (x p2)
      yMax = max (y p1) (y p2)
  in Rectangle (Point xMin yMin) (Point xMin yMax) (Point xMax yMin) (Point xMax yMax)

area :: Rectangle -> Int
area rect =
    let width = x (maxMax rect) - x (minMin rect) + 1
        height = y (maxMax rect) - y (minMin rect) + 1
    in width * height


data Line = Horizontal { lineY :: Int, xMin :: Int, xMax :: Int }
          | Vertical   { lineX :: Int, yMin :: Int, yMax :: Int }

line :: Point -> Point -> Line
line (Point x1 y1) (Point x2 y2)
    | y1 == y2  = Horizontal y1 (min x1 x2) (max x1 x2)
    | x1 == x2  = Vertical   x1 (min y1 y2) (max y1 y2)
    | otherwise = error "Line must be either horizontal or vertical"

lineIntersectsRectangle :: Rectangle -> Line -> Bool
lineIntersectsRectangle rect line =
    let (xMinR, yMinR) = (getX (minMin rect), getY (minMin rect))
        (xMaxR, yMaxR) = (getX (maxMax rect), getY (maxMax rect))
    in case line of
        Horizontal y xMinL xMaxL ->
            yMinR < y && y < yMaxR &&
            xMinR < xMaxL && xMinL < xMaxR
        Vertical x yMinL yMaxL ->
            xMinR < x && x < xMaxR &&
            yMinR < yMaxL && yMinL < yMaxR

edges :: [Point] -> [Line]
edges vertices =
    let closed = vertices ++ [head vertices]
        makeEdges (p1:p2:ps) = line p1 p2 : makeEdges (p2:ps)
        makeEdges _ = []
    in makeEdges closed
