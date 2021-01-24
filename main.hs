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

-- | Load values to consecutive registers. Mostly used to set up memory before running a program.
loadConsecutiveRegisters :: Memory -> Int -> Memory
loadConsecutiveRegisters registerVals startIndex = (take startIndex memory) ++ registerVals ++ (drop startIndex memory)

-- | For convenience, initializing step to zero and printLen to 10.
execute :: Program -> ProgramState -> IO ()
execute = execute' 0 10

-- | Execute a program, printing the contents of memory out at each step.
execute' :: Int -> Int -> Program -> ProgramState -> IO ()
execute' step printLen program (pc, mem) 
    | pc >= length program = pure ()
    | otherwise = 
        do
            (newPc, newMem) <- pure $ executeInstruction (program!!pc) (pc, mem) -- execute instr, lift into IO monad
            putStrLn $ printMem newMem          -- print step
            execute' (step + 1) printLen program (newPc, newMem)     -- recurse
    where
        printMem m = "(" ++ (show step) ++ ") " ++ (show $ take printLen m)

-- | Execute a single instruction purely.
executeInstruction :: Instruction -> ProgramState -> ProgramState
executeInstruction (Zero r) (pc, mem) =  (pc + 1, x ++ [0] ++ xs)
    where
        (x, _:xs) = splitAt r mem
executeInstruction (Successor r) (pc, mem) =  (pc + 1, x ++ [y + 1] ++ ys)
    where
        (x, y:ys) = splitAt r mem
executeInstruction (Transfer r1 r2) (pc, mem) =  (pc + 1, x ++ [mem!!r1] ++ ys)
    where
        (x, _:ys) = splitAt r2 mem
executeInstruction (Jump r1 r2 jumpPc) (pc, mem) 
    | mem!!r1 == mem!!r2 =  (jumpPc, transferredMem)
    | otherwise =  (pc + 1, mem)
    where
        transferredMem = x ++ [mem!!r1] ++ ys
        (x, _:ys) = splitAt r2 mem

-----------------------------------------------------
-- Examples

-- | Initialize the memory to infinitely many zeros.
memory :: Memory
memory = [0, 0..]

someProgram :: Program
someProgram = [(Successor 3), (Successor 3), (Successor 3), 
    (Successor 1), (Transfer 1 0), (Transfer 1 9), (Transfer 1 6),
    (Jump 1 3 999), (Jump 1 1 3)]

-- @todo getChar, 's' is for step, 'r' is for run through
main :: IO ()
main = execute someProgram (0, memory)