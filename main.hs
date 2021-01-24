-- Main module

type Register = Int
type ProgramCounter = Int
type Program = [] Instruction
type Memory = [] Int
type ProgramState = (ProgramCounter, Memory)

data Instruction = Zero Register 
    | Successor Register 
    | Transfer Register Register 
    | Jump Register Register ProgramCounter

instance Show Instruction where
    show (Zero r) = "(Zero " ++ (show r) ++ ")"
    show (Successor r) = "(Successor " ++ (show r) ++ ")"
    show (Transfer r1 r2) = "(Transfer " ++ (show r1) ++ " -> " ++ (show r2) ++ ")"
    show (Jump r1 r2 pc) = "(Jump " ++ (show r1) ++ ", " ++ (show r2) ++ ", " ++ (show pc) ++ ")"

-- type 

-- | Initialize the memory to infinitely many zeros.
memory :: Memory
memory = [0, 0..]

-- | Load values to consecutive registers. Mostly used to set up memory before running a program.
loadConsecutiveRegisters :: Memory -> Int -> Memory
loadConsecutiveRegisters registerVals startIndex = (take startIndex memory) ++ registerVals ++ (drop startIndex memory)

someMemory :: Memory
someMemory = loadConsecutiveRegisters [1..10] 0

-- | Execute a program, printing the contents of memory out at each step.
execute :: Program -> ProgramCounter -> Memory -> IO Memory
execute = undefined

-- | Execute a single instruction purely. Note that although we use the Either monad, it
-- | doesn't signify an error here, but is just used for convenience while returning a value.
executeInstruction :: Instruction -> ProgramState -> ProgramState
executeInstruction (Zero r) (pc, mem) = (pc + 1, x ++ [0] ++ xs)
    where
        (x, _:xs) = splitAt r mem
executeInstruction (Successor r) (pc, mem) = (pc + 1, x ++ [y + 1] ++ ys)
    where
        (x, y:ys) = splitAt r mem
executeInstruction (Transfer r1 r2) (pc, mem) = (pc + 1, x ++ [mem!!r1] ++ y)
    where
        (x, y) = splitAt r2 mem
executeInstruction (Jump r1 r2 jumpPc) (pc, mem) 
    | mem!!r1 == mem!!r2 = (jumpPc, transferredMem)
    | otherwise = (pc + 1, mem)
    where
        transferredMem = x ++ [mem!!r1] ++ ys
        (x, _:ys) = splitAt r2 mem


-- @todo getChar, 's' is for step, 'r' is for run through
main :: IO ()
main = putStrLn $ show (Zero 1)