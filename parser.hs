{-# LANGUAGE RecordWildCards #-}
import Sprockell.System
import Sprockell.TypesEtc
import Data.Int
import Data.Ord
import System.IO
import System.Environment
import Control.Monad
import Data.Conduit
type Code = [Operand]
type Function = ([Operand], [Operand])
type Op = Char
data Operand = RealOp Char | PseudoOp String [Int] deriving (Show, Eq)

findNext [] c = error ("Could not find expected character " ++ (show c))
findNext (x:xs) c | x == c = 0
                  | otherwise = 1 + (findNext xs c)

unique [] = []
unique (x:xs) | elem x xs = unique xs
              | otherwise = x : (unique xs)

findWhileLoop :: Code -> Int -> Code
findWhileLoop [] _ = error "Could not match while-brackets"
findWhileLoop ((RealOp x):xs) c | x == '[' = (RealOp x) : (findWhileLoop xs (c+1))
                  | x == ']' && c == 0 = []
                  | x == ']' = (RealOp x) : (findWhileLoop xs (c-1))
                  | otherwise = (RealOp x) : (findWhileLoop xs c)
findWhileLoop ((PseudoOp x i):xs) c =  (PseudoOp x i) : (findWhileLoop xs c)

findIfLoop :: Code -> Int -> Code
findIfLoop [] _ = error "Could not match If-brackets"
findIfLoop ((RealOp x):xs) c | x == '(' = (RealOp x) : (findIfLoop xs (c+1))
                    | x == ')' && c == 0 = []
                    | x == ')' = (RealOp x) : findIfLoop xs (c-1)
                    | otherwise = (RealOp x) : (findIfLoop xs c)
findIfLoop ((PseudoOp x i):xs) c =  (PseudoOp x i) : (findIfLoop xs c)

findThreadBlock :: Code -> Int -> Code
findThreadBlock [] _ = error "Could not match thread slashes"
findThreadBlock ((RealOp x):xs) c | x == '/' = (RealOp x) : (findThreadBlock xs (c+1))
                    | x == '\\' && c == 0 = []
                    | x == '\\' = (RealOp x) : findThreadBlock xs (c-1)
                    | otherwise = (RealOp x) : (findThreadBlock xs c)
findThreadBlock ((PseudoOp x i):xs) c =  (PseudoOp x i) : (findThreadBlock xs c)

filterComments :: Code -> Code
filterComments [] = []
filterComments (x:xs) | x == (RealOp '%') = filterComments $ drop ((findNext xs (RealOp '%'))+1) xs
                      | otherwise = x : (filterComments xs)

findFunctionIndex :: [Function] -> [Operand] -> Int
findFunctionIndex [] s = 0
findFunctionIndex (f:fs) s  | fst f == s = 1
                            | not (elem s (map fst fs)) = 0
                            | otherwise = 1 + findFunctionIndex fs s

getFunctions :: Code -> [Function]
getFunctions [] = [([RealOp 'b'], []), ([RealOp 'b', RealOp '+'], []),
                  ([RealOp 'b', RealOp '-'], []), ([RealOp 'b', RealOp '~'], [])]

getFunctions (x:xs) | x == (RealOp ':') = (fh,sh):(getFunctions r)
                    | otherwise = getFunctions xs
                      where
                          l = findNext xs (RealOp ':')
                          fh = take l xs
                          l2 = findNext (drop (l+1) xs) (RealOp ':')
                          r = drop (l+l2+2) xs
                          sh = take (l2) (drop (l+1) xs)

getCallLetters :: Code -> [Operand]
getCallLetters xs = (unique ( map (last . fst ) (getFunctions xs)))

getFunctionByScope :: [Operand] -> [Function] -> Operand -> [Operand]
getFunctionByScope xs fs (RealOp f) | elem (xs++[RealOp f]) (map fst fs) = xs++[(RealOp f)]
                                    | xs == [] = []
                                    | otherwise = getFunctionByScope (init xs) fs (RealOp f)
getTableAllocation :: Code -> Int
getTableAllocation c = (length (getFunctions c) + 1 ) * (length $ getCallLetters c)

getTableAddress :: [Operand] -> Code -> Int
getTableAddress s c = (findFunctionIndex (getFunctions c) s) * (length $ getCallLetters c) + 128

getFunctionAddress :: Operand -> Code -> Int
getFunctionAddress (RealOp f) c = findNext (getCallLetters c) (RealOp f)

operandOrd :: Operand -> Int
operandOrd (RealOp c) = ord c

ptrWork :: Code -> Value
ptrWork c = fromIntegral(256 + (getTableAllocation c))

allocateSharedMem :: Int  -> [Instruction] --Allocates shared mem and stores pointer in RegE
allocateSharedMem s =     [TestAndSet (Addr 100),
                          Receive RegE, Branch RegE (Rel 2), Jump (Rel (0-3)),
                          Read (Addr 101), Receive RegE, Const (fromIntegral s) RegD,
                          Compute Add RegD RegE RegD, Write RegD (Addr 101),
                          Const 0 RegD, Write RegD (Addr 100)]

lock :: [Instruction]
lock = [Const 5 RegD, Compute Add RegD RegB RegD,
        TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel 2), Jump (Rel (-3))]

unlock :: [Instruction]
unlock = [Const 5 RegD, Compute Add RegD RegB RegD, Write Zero (Deref RegD)]

loadNode :: Code -> [Instruction]
loadNode c = [Read (Deref RegB), Receive RegD, Store RegD (Addr (ptrWork c)), --Load type
              Const 1 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegD), Receive RegD, Store RegD (Addr ((ptrWork c) + 1)), --Load value
              Const 2 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegD), Receive RegD, Store RegD (Addr ((ptrWork c) + 2)), --Load next
              Const 3 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegD), Receive RegD, Store RegD (Addr ((ptrWork c) + 3)), --Load previous
              Const 4 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegD), Receive RegD, Store RegD (Addr ((ptrWork c) + 4)), --Load begin
              Const 0 RegD,
              Store RegD (Addr ((ptrWork c) + 5))] -- Store 0 to dirty flag

storeNode :: Code -> [Instruction]
storeNode c =[Load (Addr ((ptrWork c)+5)) RegD, Branch RegD (Rel 2),
              Jump (Rel 19), -- Jump over store node
              Load (Addr (ptrWork c)) RegD, Write RegD (Deref RegB), --Store Type
              Const 1 RegD, Compute Add RegD RegB RegE,
              Load (Addr ((ptrWork c)+1)) RegD, Write RegD (Deref RegE), --Store value
              Const 2 RegD, Compute Add RegD RegB RegE,
              Load (Addr ((ptrWork c)+2)) RegD, Write RegD (Deref RegE), --Store next
              Const 3 RegD, Compute Add RegD RegB RegE,
              Load (Addr ((ptrWork c)+3)) RegD, Write RegD (Deref RegE), --Store previous
              Const 4 RegD, Compute Add RegD RegB RegE,
              Load (Addr ((ptrWork c)+4)) RegD, Write RegD (Deref RegE) --Store begin
              ]

compile :: Code -> Code -> [Operand] -> [Instruction]
compile [] _ _ = []
compile ((RealOp x):xs) c s = case x of
      ',' -> [Read (Addr 0x1000000), Receive RegD, Const 1 RegE,
              Compute Add RegE RegD RegE, Branch RegE (Rel 2), Jump (Rel (-5)),
              Const b RegE, --Write type
              Store RegE (Addr w), Store RegD (Addr (w+1)),  --Write value
              Const 1 RegD, Store RegD (Addr (w+5))] ++ compile xs c s --Write dirty flag
      '.' -> [Load (Addr (w+1)) RegD, Write RegD (Addr 0x1000000)] ++ compile xs c s
      '<' -> storeNode c ++ unlock ++ [Load (Addr (w+3)) RegB] ++ lock ++
               [Branch RegB (Rel 4),
                Const 1 RegD, Write RegD (Addr 0x1000000), EndProg]
             ++ loadNode c ++ compile xs c s
      '>' -> storeNode c ++ unlock ++ [Load (Addr (w+2)) RegB] ++ lock ++
               [Branch RegB (Rel 4),
                Const 1 RegD, Write RegD (Addr 0x1000000), EndProg]
             ++ loadNode c ++ compile xs c s
      '^' -> [Const root RegD,
              Store RegD (Addr (w)), Store Zero (Addr (w+1)), --Clear node
              Const 1 RegD, Store RegD (Addr (w+5))] --Write dirty flag
              ++ compile xs c s
      '*' -> [Load (Addr w) RegD, Const root RegE, Compute Sub RegD RegE RegD,
          Branch RegD (Rel (fromIntegral (37 + allocSize + lenStore + lenLock + lenUnlock + lenLoad)))]
          ++ (allocateSharedMem 9) ++
         [Write RegB (Deref RegE),                 --Store current node as parent
          --TMNode setup done, threadcount and lock can stay 0.
          --Now setting up the first node
          Const 7 RegD, Compute Add RegD RegE RegD,
          Write RegE (Deref RegD), --Save TMNode into begin of first node
          Const 3 RegD, Compute Add RegD RegE RegD,
          Const root RegC, Write RegC (Deref RegD), --Set root type
          --First node setup done, now old node updating
          Const 1 RegD, Store RegD (Addr (w+5)), --Set dirty flag
          Const t RegD, Store RegD (Addr w), --Set new type
          Store RegE (Addr (w+1)), --Store new TMNode as value
          --End of setting up new chain when new tape is created
          Load (Addr (w+1)) RegD, Const 2 RegE, Compute Add RegD RegE RegD, --threadlock
          TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel 2), Jump (Rel (0-3)), --Lock threadcount
          Const 1 RegE, Compute Sub RegD RegE RegD, Read (Deref RegD), Receive RegC,
          Compute Add RegE RegC RegC, Write RegC (Deref RegD), --Increment threadcount
          Compute Add RegE RegD RegD, Write Zero (Deref RegD)] ++ --Free lock
          storeNode c ++ [Push RegB] ++
          [Load (Addr (w+1)) RegB, Const 3 RegD, Compute Add RegB RegD RegB] ++
          lock ++ loadNode c ++ [Compute Add Zero RegB RegC, Pop RegB] ++ unlock ++
          [Compute Add Zero RegC RegB,
          Jump (Rel (fromIntegral (18 + lenStore + lenUnlock + lenLoad + lenLock)))] ++
          --End of setting up new chain
          [Load (Addr (w+1)) RegD, Const 2 RegE, Compute Add RegD RegE RegD, --threadlock
          TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel 2), Jump (Rel (0-3)), --Lock threadcount
          Const 1 RegE, Compute Sub RegD RegE RegD, Read (Deref RegD), Receive RegC,
          Compute Add RegE RegC RegC, Write RegC (Deref RegD), --Increment threadcount
          Compute Add RegE RegD RegD, Write Zero (Deref RegD)] ++ --Free lock
          storeNode c ++ unlock ++
          [Load (Addr (w+1)) RegB, Const 3 RegD, Compute Add RegB RegD RegB] ++
          lock ++ loadNode c ++ compile xs c s
      '&' -> storeNode c ++ unlock ++ [Load (Addr (w+4)) RegD, Const 2 RegE, Compute Add RegE RegD RegD,
            TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel 2), Jump (Rel (0-3)), --Lock threadcount
            Const 1 RegE, Compute Sub RegD RegE RegC, Read (Deref RegC), Receive RegD,
            Compute Sub RegD RegE RegD, Write RegD (Deref RegC), --Update threadcount
            Compute Add RegC RegE RegC, Write Zero (Deref RegC), --clear thread lock
            Const 2 RegE,
            Compute Sub RegC RegE RegC, Read (Deref RegC), Receive RegB] ++
            lock ++ loadNode c ++ compile xs c s
      '|' -> [Const b RegD, Store RegD (Addr w), --Store byte type
              Const x2 RegD, Store RegD (Addr (w+1)),  --Store value
              Const 1 RegD, Store RegD (Addr (w+5))]++ --Write dirty flag
              compile (tail xs) c s
      '@' -> [Compute Add RegB Zero RegA] ++ compile xs c s
      '$' -> storeNode c ++ unlock ++ [Compute Add RegB Zero RegC, Compute Add RegA Zero RegB,
             Compute Add RegC Zero RegA] ++
             lock ++ loadNode c  ++ compile xs c s

      '?' -> [Push RegB, Load (Addr (w+2)) RegB,
               Branch RegB (Rel 2), Jump (Rel (fromIntegral lenRelease + 8)),
               Const 5 RegD, Compute Add RegD RegB RegD,
               TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel (fromIntegral (3+ lenRelease))),
                 Pop RegB] ++ releaseLock ++ [Jump (Rel (fromIntegral (-lenRelease-12)))] ++
               allocateSharedMem 6 ++ [Const root RegD, Write RegD (Deref RegE), --Write root type
               Const 2 RegC, Compute Add RegC RegE RegD, Write RegB (Deref RegD), --Write next
               Pop RegC, Push RegC, Const 3 RegD, Compute Add RegD RegE RegD,
               Write RegC (Deref RegD), --Write previous
               Load (Addr (w+4)) RegC, Const 4 RegD, Compute Add RegD RegE RegD,
               Write RegC (Deref RegD), --Write begin
               Branch RegB (Rel 2), Jump (Rel 4),
               Const 3 RegD, Compute Add RegB RegD RegD, Write RegE (Deref RegD), --Write previous of next node
               Store RegE (Addr (w+2)), Const 1 RegD, Store RegD (Addr (w+5)),
               Branch RegB (Rel 2), Jump (Rel (fromIntegral ((length unlock) + 1)))] --Write next of current node + dirty flag
               ++ unlock ++ [Pop RegB] ++ compile xs c s
               where
                 releaseLock = storeNode c ++ unlock ++ lock ++ loadNode c
                 lenRelease = fromIntegral (length releaseLock)
                 lenStore = fromIntegral (length (storeNode c))
                 lenUnlock = fromIntegral (length (unlock))

      '!' -> [Push RegB, Load (Addr (w+3)) RegB, Branch RegB (Rel 4),
              Const 2 RegD, Write RegD (Addr 0x1000000), EndProg,
              Const 5 RegD, Compute Add RegD RegB RegD,
              TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel (3+ lenRelease)),
                Pop RegB] ++ releaseLock ++ [Jump (Rel (0-lenRelease-12)),
              Load (Addr (w+2)) RegB, Branch RegB (Rel 2), --RegB points to Node C
                Jump (Rel (fromIntegral (20 + lenUnlock*2))), --Jump to after previous set
              Const 5 RegD, Compute Add RegD RegB RegD,
              TestAndSet (Deref RegD), Receive RegE, Branch RegE (Rel (fromIntegral(6+ lenUnlock))),
                Pop RegB, Push RegB, Load (Addr (w+3)) RegB] ++ unlock ++ [Pop RegB,
                Jump (Rel (0-15-lenUnlock-lenRelease)),
              Const 3 RegC, Compute Add RegC RegB RegD, Read (Deref RegD), Receive RegE, ---RegE points to current
              Compute Add RegC RegE RegE, Read (Deref RegE), Receive RegE, --RegE points to previous
              Write RegE (Deref RegD), --Write previous of next node
              Compute Add Zero RegB RegC] ++ unlock ++ [
              Pop RegB, Compute Add RegB Zero RegC, Load (Addr (w+3)) RegB,
              Const 2 RegD, Compute Add RegB RegD RegE, --RegE points to prevNode next
              Compute Add RegD RegC RegC, Read (Deref RegC), Receive RegC, --RegC contains old next
              Write RegC (Deref RegE)] ++ loadNode c --Write next to prev node
              ++ compile xs c s
              where
                releaseLock = storeNode c ++ unlock ++ lock ++ loadNode c
                lenRelease = fromIntegral (length releaseLock)
                lenStore = fromIntegral (length (storeNode c))
                lenUnlock = fromIntegral (length (unlock))

      '[' -> [Load (Addr (w+1)) RegD,
          Branch RegD (Rel 2), Jump (Rel (whileLoopLen + 1))] ++ whileLoop ++
          [Jump (Rel (0-(whileLoopLen + 2)))] ++ compile whileLoopXs c s
      ']' -> error "Brackets are mismatched: Illegal ']' found \n"
      '(' -> [Load (Addr (w+1)) RegD,
          Branch RegD (Rel ifLoopLen)] ++ ifLoop ++ compile ifLoopXs c s
      '/' -> [Const 4 RegD, Const (-4) RegC,
              Compute Add RegD RegC RegC, Read (Deref RegC), Receive RegE,
              Branch RegE (Rel (-3)), Const 96 RegE, Compute Sub RegC RegE RegE,
              Branch RegE (Rel 2), Jump (Rel (-9)), --If D == 96 start over
              Const 3 RegE, Compute Add RegE RegC RegE,
              TestAndSet (Deref RegE), Receive RegE, --Take lock
              Branch RegE (Rel 2), Jump (Rel (-13)), --If lock fails, search on
              Read (Deref RegC), Receive RegE, Branch RegE (Rel 2), Jump (Rel 5),
                --If this job is not 0 anymore, release lock and go on
                Const 3 RegE, Compute Add RegE RegC RegE,
                Write Zero (Deref RegE), Jump (Rel (-21)), --Release lock
              --Job is locked and still empty. Let's fill it!
              Const 1 RegD, Const 9 RegE, Compute Add PC RegE RegE,
              Write RegE (Deref RegC), --Write PC + 9
              Compute Add RegD RegC RegC, Write RegA (Deref RegC), --Write reg A
              Compute Add RegD RegC RegC, Write RegB (Deref RegC), --Write reg B
              Compute Add RegD RegC RegC, Write Zero (Deref RegC), --Release lock
              Jump (Rel (threadBlockLen + 1)) --Jump over the threadcode
              ] ++ threadBlock ++ [Jump (Abs 0)] ++ compile threadBlockXs c s
      ':' -> compile def c s
      '%' -> compile comment c s
      _ -> [Load (Addr w) RegD, Const fAddress RegE,
          Compute Add RegE RegD RegD, Load (Deref RegD) RegD,
          Const 3 RegE, Compute Add RegE PC RegE, Push RegE,
          Jump (Ind RegD)] ++ compile xs c s
      where b = fromIntegral (getTableAddress [RealOp 'b'] c)
            root = fromIntegral (getTableAddress [] c)
            t = fromIntegral (getTableAddress s c)
            x2 = fromIntegral (operandOrd (head xs))
            whileLoop = compile (findWhileLoop xs 0) c s
            whileLoopXs = drop (length (findWhileLoop xs 0) + 1) xs
            whileLoopLen = fromIntegral (length whileLoop + 1)
            threadBlock = (compile (findThreadBlock xs 0) c s) ++ storeNode c ++ unlock
            threadBlockXs = drop (length (findThreadBlock xs 0) + 1) xs
            threadBlockLen = fromIntegral (length threadBlock + 1)
            ifLoop = compile (findIfLoop xs 0) c s
            ifLoopXs = drop (length (findIfLoop xs 0) + 1) xs
            ifLoopLen = fromIntegral (length ifLoop +1)
            def1 = drop (findNext xs (RealOp ':') + 1) xs
            def = drop (findNext def1 (RealOp ':') +1) def1
            comment = drop (findNext xs (RealOp '%') + 1) xs
            fAddress = fromIntegral (getFunctionAddress (RealOp x) c)
            w = ptrWork c
            allocSize = length (allocateSharedMem 6)
            lenStore = fromIntegral (length (storeNode c))
            lenUnlock = fromIntegral (length (unlock))
            lenLoad = fromIntegral (length (loadNode c))
            lenLock = fromIntegral (length (lock))
compile ((PseudoOp x i):xs) c s = []


threadPool :: Code -> [Instruction]
threadPool c = [Branch SPID (Rel 2), Jump (Rel lenJump), --All threads except main thread
        Read (Addr 99), Receive RegE, Branch RegE (Rel 2), Jump (Rel 2), EndProg,
        Const 4 RegD, Const (-4) RegC,
        Compute Add RegD RegC RegC, Const 96 RegE, Compute Sub RegC RegE RegE,
        Branch RegE (Rel 2), Jump (Rel (-11)), --End of list, jump to beginning
        Read (Deref RegC), Receive RegE, --Read Job PC
        Branch RegE (Rel (2)), Jump (Rel (-8)), --If empty, goto next iteration
        --Now lets try to claim this job!
        Const 3 RegE, Compute Add RegE RegC RegE,
        TestAndSet (Deref RegE), Receive RegE, --Take lock
        Branch RegE (Rel 2), Jump (Rel (-14)), --If lock fails, search on
        --If this job is already cleared, release lock and go on
        Read (Deref RegC), Receive RegE, Branch RegE (Rel 5),
          Const 3 RegE, Compute Add RegE RegC RegE, --Lock
          Write Zero (Deref RegE), Jump (Rel (-21)), --Release and leave!
        --Lets take the job
        Const 1 RegD, Write Zero (Deref RegC), --Clear PC
        Compute Add RegD RegC RegC, Read (Deref RegC), Receive RegA, --Read regA
        Write Zero (Deref RegC), --Clear RegA field
        Compute Add RegD RegC RegC, Read (Deref RegC), Receive RegB, --Read regB
        Write Zero (Deref RegC), --Clear RegB field
        Compute Add RegD RegC RegC, Write Zero (Deref RegC), --Clear lock
        Compute Add Zero RegE RegC --Move Instruction pointer to RegC so it will surive the lock calls
        ] ++ lock ++ loadNode c ++ [Jump (Ind RegC)]
        where
          lenStore = fromIntegral (length (loadNode c))
          lenUnlock = fromIntegral (length (unlock))
          lenJump = 47 + lenStore + lenUnlock


endProgGracefully :: [Instruction]
endProgGracefully = [Const 99 RegC, Write RegC (Addr 99), EndProg]


compileFunction :: Code -> Function -> [Instruction]
compileFunction c ([RealOp 'b'],xs) = [Const b RegD, Store RegD (Addr w),
                          Const 1 RegD, Store RegD (Addr (w+5)), --write dirty
                          Pop RegD, Jump (Ind RegD)]
                where b = fromIntegral (getTableAddress [RealOp 'b'] c)
                      w = ptrWork c
compileFunction c ([RealOp 'b', RealOp '+'],xs)
                              = [Load (Addr (w+1)) RegE, Const 1 RegD,
                              Compute Add RegD RegE RegE,Store RegE (Addr (w+1)),
                              Const 1 RegD, Store RegD (Addr (w+5)), --write dirty
                              Pop RegD, Jump (Ind RegD)]
                              where
                                    w = ptrWork c
compileFunction c ([RealOp 'b', RealOp '-'],xs)
                              = [Load (Addr (w+1)) RegE, Const 1 RegD,
                              Compute Sub RegE RegD RegE,Store RegE (Addr (w+1)),
                              Const 1 RegD, Store RegD (Addr (w+5)), --write dirty
                              Pop RegD, Jump (Ind RegD)]
                              where
                                    w = ptrWork c

compileFunction c ([RealOp 'b', RealOp '~'],xs)
                              = storeNode c ++ unlock ++ [Const 1 RegD,
                              Compute Add RegD RegB RegD, Read (Deref RegD),
                              Receive RegE, Branch RegE (Rel (-2))]
                              ++ lock ++ loadNode c ++ [Pop RegD, Jump (Ind RegD)]



compileFunction c (s,xs) = compile xs c s ++ [Pop RegD, Jump (Ind RegD)]

compileFunctions c = map (compileFunction c ) (getFunctions c)

getFunctionLengths c = map length (compileFunctions c)

getFunctionOffsets' [] _ = []
getFunctionOffsets' (x:xs) o = (o):(getFunctionOffsets' xs (o+x))

getFunctionOffsets xs = getFunctionOffsets' (getFunctionLengths xs) 1

setBlock a [] = []
setBlock a (v:vs) = [Const (fromIntegral v) RegD,
                    Store RegD (Addr(fromIntegral a))] ++ setBlock (a+1) vs


getFunctionOffsetByIndex c (-1) = -1
getFunctionOffsetByIndex c i = (getFunctionOffsets c)!!i

makeBlock c f = setBlock (getTableAddress f c) (map ((getFunctionOffsetByIndex c)) fIndex)
                where
                  functions = map (getFunctionByScope f (getFunctions c)) (getCallLetters c)
                  fIndex = map (+(-1)) (map (findFunctionIndex (getFunctions c)) (functions))

compileBlocks c = concat (map (makeBlock c) (map fst (([],[]):(getFunctions c))))

removeWhiteSpace [] = []
removeWhiteSpace (x:xs) | elem (ord x) [32, 9, 10] = removeWhiteSpace xs
                        | otherwise = x : (removeWhiteSpace xs)


debug :: SystemState -> String
debug SysState{..}
  | ((regbank (sprs!!0))!SPID)== 0 = ""
  | (regbank (sprs!!0))!PC == (regbank (sprs!!0))!RegD
    = "Function call" ++ (show ((regbank (sprs!!0))!PC)) ++ " " ++ "\n"
  | otherwise = show ((regbank (sprs!!0))!PC) ++ " " ++
    (show ((regbank (sprs!!0))!SPID)) ++ "\n"


-- This debug function show a message when a Sprockell reaches an EndProg instruction.
debugEndProg SysState{sprs=sprs,instrs=instrs} = concat $ map isHalting sprs
    where
        isHalting SprState{regbank=regs,halted=halted}
            | not halted && spid> 0
                = "Sprockell " ++ show spid ++ " at addr " ++ show pc ++ " RegB: " ++ (show (regs ! RegB)) ++ "\n"
            | otherwise = ""
            where
                pc   = regs ! PC
                spid = regs ! SPID


link c = [Jump (Rel (fromIntegral((length functions)+1)))] ++ functions ++
          compileBlocks c ++
          threadPool c ++
          [Const (fromIntegral (getTableAddress [] c)) RegD,
           Store RegD (Addr w),
           Const (fromIntegral 108) RegD,
           Const 102 RegB,
           Write RegD (Addr (fromIntegral 101))]  ++
          (compile c c []) ++ endProgGracefully
      where functions = (concat $ compileFunctions c)
            bs = getTableAllocation c
            w = fromIntegral (ptrWork c)

charToOp :: Char -> Operand
charToOp x = (RealOp x)

main = do
  handle <- openFile "include/stl.obf" ReadMode
  contents <- hGetContents handle
  let content = contents
  let code = map charToOp (removeWhiteSpace content)
  runDebug debugEndProg 4 (link code)
  hClose handle
