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

someProgram :: Program
someProgram = [(Successor 1), (Successor 1), (Successor 1), (Successor 2), (Successor 2), (Successor 2), (Successor 3)]

-- | Execute a program, printing the contents of memory out at each step.
execute :: Program -> ProgramState -> IO ()
execute program (pc, mem) 
    | pc >= length program = putStrLn $ printMem mem
    | otherwise = 
        do
            (newPc, newMem) <- executeInstruction (program!!pc) (pc, mem)
            putStrLn $ printMem newMem
            execute program (newPc, newMem)
    where
        printMem m = show $ take 10 m

-- | Execute a single instruction purely.
executeInstruction :: Instruction -> ProgramState -> IO ProgramState
executeInstruction (Zero r) (pc, mem) = return (pc + 1, x ++ [0] ++ xs)
    where
        (x, _:xs) = splitAt r mem
executeInstruction (Successor r) (pc, mem) = return (pc + 1, x ++ [y + 1] ++ ys)
    where
        (x, y:ys) = splitAt r mem
executeInstruction (Transfer r1 r2) (pc, mem) = return (pc + 1, x ++ [mem!!r1] ++ y)
    where
        (x, y) = splitAt r2 mem
executeInstruction (Jump r1 r2 jumpPc) (pc, mem) 
    | mem!!r1 == mem!!r2 = return (jumpPc, transferredMem)
    | otherwise = return (pc + 1, mem)
    where
        transferredMem = x ++ [mem!!r1] ++ ys
        (x, _:ys) = splitAt r2 mem


-- @todo getChar, 's' is for step, 'r' is for run through
main :: IO ()
main = execute someProgram (0, memory)