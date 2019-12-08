module Directions (Direction(Up, Down, Right, Left), stringToDirections) where

import Prelude hiding (Either, Left, Right)

data Direction = Up Int | Down Int | Left Int | Right Int
  deriving Show

stringToDirections :: String -> [Direction]
stringToDirections string  =  map stringToDirection $ splitStringBy ',' string

-- private 

splitStringBy :: Char -> String -> [String]
splitStringBy _ [] =  [""]
splitStringBy delimiter (c:s)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where rest = splitStringBy delimiter s

stringToDirection (d:n) = case d of
                            'U' -> Up number
                            'D' -> Down number
                            'L' -> Left number
                            'R' -> Right number
                          where number = read n
