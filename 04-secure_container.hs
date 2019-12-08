
range = [130254..678275]

digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

adjacent [] = False
adjacent l = if skipped_count == 2 then True else adjacent rest
  where (skipped_count, rest) = skipAdjacent l

skipAdjacent digits = doSkipDigits 0 (head digits) digits
doSkipDigits count _ [] = (count, [])
doSkipDigits count digit (x:t) 
  | digit == x = doSkipDigits (count + 1) digit t
  | otherwise = (count, x:t)

increasing [_] = True
increasing (x:t) = if x > head t then False else increasing t

number r = length $ filter increasing $ filter adjacent $ map digs r 
