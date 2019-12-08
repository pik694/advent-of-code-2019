module CorssedWires where 
import Lines
import Directions
import Data.List 

main = do
  wire1 <- getLine
  wire2 <- getLine
  let chain1 =  chainFromDirections $ stringToDirections wire1
  let chain2 = chainFromDirections $ stringToDirections wire2
  let crossings =  map cross [(l1, l2) | l1 <- chain1, l2 <- chain2]
  print$ take 5 $ sort $  map distance $ (\c -> [x | Just x <- c]) $  concat crossings

