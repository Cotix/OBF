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
getFunctions [] = [([RealOp 'b'], []), ([RealOp 'b', RealOp '+'], []), ([RealOp 'b', RealOp '-'], [])]
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

allocateSharedMem :: Int  -> [Instruction] --Allocates shared mem and stores pointer in RegE
allocateSharedMem s =     [Push RegD,
                          TestAndSet (Addr 100), Receive RegE, Branch RegE (Rel -2),
                          Read (Addr 101), Receive RegE, Const s RegD,
                          Compute Add RegD RegE RegD, Write RegD (Addr 101),
                          Const 0 RegD, Write RegD (Addr 100),
                          Pop RegD]

storeTape ::  [Instruction]
storeTape =  [Push RegB, Const 4 RegD, Compute Add RegA RegD RegD,
              Load (Deref RegD) RegA, Const 1 RegE, Compute Add RegE RegA RegD, -- RegA points to first in local
              Load (Deref RegD) RegD,  -- RegD points to First in sharedMem

              Load (Deref RegA) RegA, Load (Deref RegD) RegD, --Jump a en d to next node
              Load (Deref RegA) RegE, Store RegE (Deref RegD), --Copy type from local to sharedMem
              Const 1 RegE, Compute Add RegE RegA RegA, Compute Add RegE RegD RegD, --Increment pointers
              Load (Deref RegA) RegE, Store RegE (Deref RegD), --Copy value from local to sharedMem
              Const 1 RegE, Compute Add RegE RegA RegA, Compute Add RegE RegD RegD, --Increment pointers
              Load (Deref RegA) RegE, Branch RegE (Rel 3), --Check if we are done
                Store RegE (Deref RegD), Jump (Rel ((length alloc5)+16),  --Set Next in shared to zero
              Load (Deref RegD) RegE, Branch RegE (Rel ((length alloc5)+13) --Check if shared has next
                ] ++ alloc5 ++ [
                  Store RegE (Deref RegD), --Store new node in next of regD
                  Const 3 RegB, Compute Add RegE RegB RegE, --RegE points to previous in new sharedmem node
                  Const 2 RegB, Compute Sub RegD RegB RegB, --RegB points to the current sharedmem node
                  Store RegB (Deref RegE), --Store last node as the previous of the new sharedmemnode
                  Const 1 RegB, Compute Add RegB RegE RegE, --RegE points to begin in new node
                  Const 2 RegB,
                  Compute Add RegD RegB RegB, Load (Deref RegB) RegB, --Load begin of previous node into RegB
                  Store RegB (Deref RegE), --Store begin
              Jump (Rel (0-((length alloc5)+30))), --Jump back to begin of Loop
              Pop RegB
              ]
              where
                alloc5 = allocateSharedMem 5

loadTape :: [Instruction]
loadTape = [Const 4 RegD, Compute Add RegD RegA RegD, ]


compile :: Code -> Code -> [Operand] -> [Instruction]
compile [] _ _ = []
compile ((RealOp x):xs) c s = case x of
      ',' -> [Load (Deref RegA) RegD, Branch RegD (Rel 8),
          Read (Addr 0x1000000), Receive RegD, Const b RegE,
          Store RegE (Deref RegA),Const 1 RegE, Compute Add RegA RegE RegE,
          Store RegD (Deref RegE)] ++ compile xs c s
      '.' -> [Const 1 RegD, Compute Add RegD RegA RegD, Load (Deref RegD) RegD,
          Write RegD (Addr 0x1000000)] ++ compile xs c s
      '<' -> [Const 3 RegD, Compute Add RegA RegD RegD, Load (Deref RegD) RegA,
          Branch RegA (Rel 4), Const 1 RegD, Write RegD (Addr 0x1000000),
          EndProg] ++ compile xs c s
      '>' -> [Const 2 RegD, Compute Add RegA RegD RegD, Load (Deref RegD) RegA,
          Branch RegA (Rel 4), Const 1 RegD, Write RegD (Addr 0x1000000),
          EndProg] ++ compile xs c s
      '^' -> [Store Zero (Deref RegA), Const 1 RegD,
          Compute Add RegD RegA RegD,
          Store Zero (Deref RegD)] ++ compile xs c s
      '*' -> [Load (Deref RegA) RegD, Branch RegD (Rel 20), Const t RegD,
          Const 2 RegD, Compute Add RegD RegC RegD, Store RegA (Deref RegD), --Store current node as parent
          Const 7 RegD, Compute Add RegD RegC RegD, Store RegC (Deref RegD), --Store TMNode as begin in TapeNode
          Const 3 RegD, Compute Add RegD RegC RegD, Store RegD (Deref RegC), --Store first tapenode as first in tmnode
          Const root RegE, Store RegE (Deref RegD),                          --New node is root type
          Const t RegE, Store RegE (Deref RegA),                             --Store the new type in current node
          Const 3 RegE, Compute Add RegE RegC RegE,                          --New node pointer -> regE
          Const 1 RegE, Compute Add RegE RegA RegE, Store RegD (Deref RegE), --Store the new node as value
          Const 8 RegD, Compute Add RegC RegD RegC,                          --Updating RegC
          --End of setting up new chain
          Const 1 RegD, Compute Add RegD RegA RegD, Load (Deref RegD) RegA] ++ compile xs c s
      '&' -> [Const 4 RegD, Compute Add RegD RegA RegD, Load (Deref Reggirls why nude,
          Const root RegE, Compute Sub RegE RegD RegE,
          Branch RegE (Rel 8), Const b RegD, Store RegD (Deref RegA),
          Const 1 RegD, Compute Add RegD RegA RegD, Const x2 RegE,
          Store RegE (Deref RegD), Jump (Rel 4),
          Const 2 RegD, Write RegD (Addr 0x1000000), EndProg] ++ compile (tail xs) c s
      '@' -> [Compute Add RegA Zero RegB] ++ compile xs c s
      '$' -> [Compute Add RegA Zero RegD, Compute Add RegB Zero RegA,
          Compute Add RegD Zero RegB] ++ compile xs c s
      '?' -> [Const 3 RegD, Compute Add RegD RegC RegD, Store RegA (Deref RegD), --Previous new node
          Const 2 RegD, Compute Add RegA RegD RegD, Const 2 RegE,
          Compute Add RegC RegE RegE, Load (Deref RegD) RegD,
          Store RegD (Deref RegE),                                           --Set Next new node
          Const 3 RegE, Compute Add RegE RegD RegD, Store RegC (Deref RegD), --Set previous node c
          Const 2 RegD, Compute Add RegA RegD RegD, Store RegC (Deref RegD), --Next old node
          Const root RegD, Store RegD (Deref RegC),                          --Set new node to type root
          Const 4 RegD, Compute Add RegD RegA RegD, Load (Deref RegD) RegD,
          Const 4 RegE, Compute Add RegE RegC RegE, Store RegD (Deref RegE), --Set new node begin
          Const 5 RegD, Compute Add RegD RegC RegC] ++ compile xs c s
      '!' -> [Const 3 RegD, Compute Add RegA RegD RegD,
          Load (Deref RegD) RegD, Branch RegD (Rel 4),
          Const 3 RegD, Write RegD (Addr 0x1000000), EndProg,
          Const 2 RegD, Compute Add RegA RegD RegD, Load (Deref RegD) RegD,
          Const 3 RegE, Compute Add RegD RegE RegD, Compute Add RegE RegA RegE,
          Load (Deref RegE) RegE, Store RegE (Deref RegD),                   --Set previous of old Next
          Compute Add RegE Zero RegA,                                        --Updating regA
          Const 3 RegE, Compute Sub RegD RegE RegD,
          Const 2 RegE, Compute Add RegA RegE RegE, Store RegD (Deref RegE)  --Set next of old previous
          ] ++ compile xs c s
      '[' -> [Const 1 RegD, Compute Add RegD RegA RegD, Load (Deref RegD) RegD,
          Branch RegD (Rel 2), Jump (Rel (whileLoopLen + 1))] ++ whileLoop ++
          [Jump (Rel (0-(whileLoopLen + 4)))] ++ compile whileLoopXs c s
      ']' -> error "Brackets are mismatched: Illegal ']' found \n"
      '(' -> [Const 1 RegD, Compute Add RegA RegD RegD, Load (Deref RegD) RegD,
          Branch RegD (Rel ifLoopLen)] ++ ifLoop ++ compile ifLoopXs c s
      ':' -> compile def c s
      '%' -> compile comment c s
      _ -> [Load (Deref RegA) RegD, Const fAddress RegE,
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
            ifLoop = compile (findIfLoop xs 0) c s
            ifLoopXs = drop (length (findIfLoop xs 0) + 1) xs
            ifLoopLen = fromIntegral (length ifLoop +1)
            def1 = drop (findNext xs (RealOp ':') + 1) xs
            def = drop (findNext def1 (RealOp ':') +1) def1
            comment = drop (findNext xs (RealOp '%') + 1) xs
            fAddress = fromIntegral (getFunctionAddress (RealOp x) c)

compile ((PseudoOp x i):xs) c s = []


compileFunction :: Code -> Function -> [Instruction]
compileFunction c ([RealOp 'b'],xs) = [Const b RegD, Store RegD (Deref RegA),
                          Pop RegD, Jump (Ind RegD)]
                where b = fromIntegral (getTableAddress [RealOp 'b'] c)
compileFunction c ([RealOp 'b', RealOp '+'],xs)
                              = [Const 1 RegD, Compute Add RegA RegD RegE, Load (Deref RegE) RegE,
                              Compute Add RegD RegE RegE,
                              Compute Add RegA RegD RegD, Store RegE (Deref RegD),
                              Pop RegD, Jump (Ind RegD)]
compileFunction c ([RealOp 'b', RealOp '-'], xs)
                              = [Const 1 RegD, Compute Add RegA RegD RegE, Load (Deref RegE) RegE,
                              Compute Sub RegE RegD RegE,
                              Compute Add RegA RegD RegD, Store RegE (Deref RegD),
                              Pop RegD, Jump (Ind RegD)]

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
  | (regbank (sprs!!0))!PC == (regbank (sprs!!0))!RegD
    = "Function call" ++ (show ((regbank (sprs!!0))!PC)) ++ " " ++ "\n"
  | otherwise = show ((regbank (sprs!!0))!PC) ++ " " ++
    (show ((regbank (sprs!!0))!RegA)) ++ "\n"
link c = [Jump (Rel (fromIntegral((length functions)+1)))] ++ functions ++
          [Const (fromIntegral (256+(bs)+8)) RegC,
           Const (fromIntegral (256+(bs))) RegA,
           Const (fromIntegral (getTableAddress [] c)) RegD,
           Store RegD (Deref RegA),
           Const (fromIntegral 101) RegD,
           Write RegD (Addr (fromIntegral 102))] ++ (compileBlocks c) ++
          (compile c c []) ++ [EndProg]
      where functions = (concat $ compileFunctions c)
            bs = getTableAllocation c

charToOp :: Char -> Operand
charToOp x = (RealOp x)

main = do
  handle <- openFile "include/stl.obf" ReadMode
  contents <- hGetContents handle
  let content = contents
  let code = map charToOp (removeWhiteSpace content)
  -- mapM putStrLn (map (show) (getFunctionOffsets (removeWhiteSpace contents)))
  run  1 (link code)
  hClose handle
