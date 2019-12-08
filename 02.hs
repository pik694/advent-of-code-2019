prog :: [Int]
prog = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,2,23,6,27,1,27,5,31,2,6,31,35,1,5,35,39,2,39,9,43,1,43,5,47,1,10,47,51,1,51,6,55,1,55,10,59,1,59,6,63,2,13,63,67,1,9,67,71,2,6,71,75,1,5,75,79,1,9,79,83,2,6,83,87,1,5,87,91,2,6,91,95,2,95,9,99,1,99,6,103,1,103,13,107,2,13,107,111,2,111,10,115,1,115,6,119,1,6,119,123,2,6,123,127,1,127,5,131,2,131,6,135,1,135,2,139,1,139,9,0,99,2,14,0,0]


find_params program = 
  let params = [(noun, verb) | noun <- [1..99], verb <- [1..99]]
      in do_find_params program params

do_find_params program ((noun, verb):tail) = 
  let (head, _:_:program_tail) = splitAt 1 program  
      result = run (head ++ (noun:verb:program_tail))
   in case result of 
    19690720 -> (noun, verb)
    _ -> do_find_params program tail


run program = do_run program 0

do_run program pos =
  let maybe_func = opcode (program !! pos)
   in case maybe_func of 
          Just func -> do_run modified_program (pos+4) 
            where
              (lval, rval, rpos) = find_values program pos
              result_val = func lval rval
              modified_program = modify_program program rpos result_val
          Nothing -> program !! 0 
 

find_values :: [Int] -> Int -> (Int, Int, Int)
find_values program pos = (lval, rval, return_pos)
  where
    lpos = program !! (pos + 1)
    lval = program !! lpos
    rpos = program !! (pos + 2)
    rval = program !! rpos
    return_pos = program !! (pos + 3)


modify_program program return_pos result_val = 
  let (front, _:tail) = splitAt return_pos program
   in front ++ (result_val:tail)

opcode 99 = Nothing
opcode 1 = Just (+)
opcode 2 = Just (*)


