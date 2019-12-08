
run program = execute (0, program, [])
-- private 

type Memory = [Int]
type Address = Int
type State = (Address, Memory, [ExecutionLog])

data ParamMode = Position | Immediate deriving (Enum, Show) 
data Instruction = Return | Instruction ( State -> State )
data Function = Unary | Ternary
data Opcode = Empty | Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals deriving (Enum, Show)

data ExecutionLog = ExecutionLog Opcode [Int] Int deriving Show

execute :: State -> (Memory, [ExecutionLog])
execute (sp, memory, log) = doExecute currentInstruction (sp, memory, log) 
  where currentInstruction = instruction sp memory

doExecute ::  Instruction -> State -> (Memory, [ExecutionLog])
doExecute Return (_, memory, log) = (memory, log) 
doExecute (Instruction executable) state  = execute $ executable state


instruction sp memory
  | memory !! sp == 99 = Return
  | otherwise = buildInstruction sp memory


buildInstruction sp memory =
  let (opcode, paramModes) = retriveModesAndOpcode (memory !! sp)
   in Instruction (executable opcode paramModes)

retriveModesAndOpcode :: Int -> (Opcode, [ParamMode])
retriveModesAndOpcode code = ( toEnum (code `mod` 100), modes code)

modes :: Int -> [ParamMode]
modes code = map toEnum $ getModes 3 (code `div` 100)
getModes c code = (code `mod` 10) : getModes (c-1) (code `div` 10)

executable Add params = (\(sp, memory, logs) -> 
  let (lval:rval:_)= getParams 2 (sp+1) memory params
      result = lval + rval
      (returnAddress:_) = getParams 1 (sp+3) memory [Immediate]
      updated_sp = sp + 4
      updated_memory = putAt returnAddress result memory
   in (updated_sp, updated_memory, (ExecutionLog Add [lval, rval] result):logs)
  )
executable Multiply params = (\(sp, memory, logs) -> 
  let (lval:rval:_)= getParams 2 (sp+1) memory params
      result = lval * rval
      (returnAddress:_) = getParams 1 (sp+3) memory [Immediate]
      updated_sp = sp + 4
      updated_memory = putAt returnAddress result memory
   in (updated_sp, updated_memory, (ExecutionLog Multiply [lval, rval] result):logs)
  )
executable Input params = (\(sp, memory, logs) -> 
  let result = 5
      (returnAddress:_) = getParams 1 (sp+1) memory [Immediate]
      updated_sp = sp + 2
      updated_memory = putAt returnAddress result memory
   in (updated_sp, updated_memory, (ExecutionLog Input [] result):logs)
  )

executable Output params = (\(sp, memory, logs) -> 
  let (val:_)= getParams 1 (sp+1) memory params
      result = val 
      updated_sp = sp + 2
   in (updated_sp, memory, (ExecutionLog Output [val] val):logs)
  )

executable JumpIfTrue params = (\(sp, memory, logs) -> 
  let (val:dest:_)= getParams 2 (sp+1) memory params
      result = val 
      updated_sp = if val /= 0 then dest else sp + 3
   in (updated_sp, memory, (ExecutionLog JumpIfTrue [val,dest] updated_sp):logs)
  )

executable JumpIfFalse params = (\(sp, memory, logs) -> 
  let (val:dest:_)= getParams 2 (sp+1) memory params
      result = val 
      updated_sp = if val == 0 then dest else sp + 3
   in (updated_sp, memory, (ExecutionLog JumpIfTrue [val,dest] updated_sp):logs)
  )

executable LessThan params = (\(sp, memory, logs) ->
  let (lval:rval:_)= getParams 2 (sp+1) memory params
      result = if lval < rval then 1 else 0 
      (returnAddress:_) = getParams 1 (sp+3) memory [Immediate]
      updated_sp = sp + 4
      updated_memory = putAt returnAddress result memory
   in (updated_sp, updated_memory, (ExecutionLog LessThan [lval, rval] result):logs)
  )

executable Equals params = (\(sp, memory, logs) ->
  let (lval:rval:_)= getParams 2 (sp+1) memory params
      result = if lval == rval then 1 else 0
      (returnAddress:_) = getParams 1 (sp+3) memory [Immediate]
      updated_sp = sp + 4
      updated_memory = putAt returnAddress result memory
   in (updated_sp, updated_memory, (ExecutionLog Multiply [lval, rval] result):logs)
  )

getParams 0 sp memory _ = []
getParams c sp memory (h:t) = (getParam sp memory h) : getParams (c-1) (sp+1) memory t

getParam sp memory mode = case mode of 
                           Immediate -> memory !! sp
                           Position -> memory !! (memory !! sp)


putAt address value memory = 
  let (f, _:t) = splitAt address memory
   in f ++ (value:t)

filterOutput (ExecutionLog Output _ _) = True
filterOutput _ = False
