module Lines (chainFromDirections, cross, distance) where

import Prelude hiding (Left, Right)
import Directions

data Point = Point Int Int deriving Show
type Line =  (Point, Point)
type Chain = [Line]

chainFromDirections :: [Direction] -> Chain
chainFromDirections directions = (chainFromPoints []) $ pointsFromDirections [] directions

cross :: (Line, Line) -> [Maybe Point]
cross (line1, line2) = if linesCross line1 line2 
                          then [Just (findCrossingPoint line1 line2)] 
                          else pointsInclusive line1 line2 


distance (Point x y) = (abs x) + (abs y)

-- private 

chainFromPoints acc [] = acc
chainFromPoints [] (b:e:t) = chainFromPoints [(b,e)] t
chainFromPoints acc (e:t) = chainFromPoints (line:acc) t
  where
    (_, begin) = head acc
    line = (begin, e)

pointsFromDirections acc [] = acc
pointsFromDirections [] (h:t) = pointsFromDirections (point:[zeroPoint]) t
  where
    zeroPoint = Point 0 0
    point = movePointByVector zeroPoint h
pointsFromDirections acc (h:t) = pointsFromDirections ((movePointByVector (head acc) h) : acc) t

movePointByVector (Point x y) direction = Point (x + dX) (y + dY)
  where 
    dX = deltaX direction
    dY = deltaY direction 

deltaX (Left x) = -x
deltaX (Right x) = x
deltaX _ = 0

deltaY (Up y) = y
deltaY (Down y) = -y
deltaY _ = 0 


data LineEquation = X Int | Y Int

lineEquation ((Point x1 y1), (Point x2 y2)) 
  | x1 == x2 = X x1 
  | y1 == y2 = Y y1 


linesCross (p1, p2) (p3, p4) = (det p1 p2 p3) * (det p1 p2 p4) < 0
findCrossingPoint line1 line2 = 
  let le1 = lineEquation line1
      le2 = lineEquation line2
   in doFindCrossingPoint le1 le2

doFindCrossingPoint (X x) (Y y) = (Point x y)
doFindCrossingPoint (Y y) (X x) = (Point x y)

det (Point x1 y1) (Point x2 y2) (Point x3 y3) = x1*y2 + x2*y3 + x3*y1 - x3*y2 - x1*y3 - x2*y1

pointsInclusive line1 line2 =  doGetPointsInclusive line1 line2 ++ doGetPointsInclusive line2 line1
doGetPointsInclusive line (p1, p2) = [maybeInclusive line p1, maybeInclusive line p2]
maybeInclusive line p = if inclusive line p then Just p else Nothing
inclusive (Point x1 y1, Point x2 y2) (Point x3 y3) = minimum (x1, x2) <= x3 && x3 <= maximum (x1, x2) && minimum (y1, y2) <= y3 && y3 <= maximum (y1, y2)



