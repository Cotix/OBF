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

ptrWork :: Code -> Value
ptrWork c = 256 + (getTableAllocation c)

allocateSharedMem :: Int  -> [Instruction] --Allocates shared mem and stores pointer in RegE
allocateSharedMem s =     [TestAndSet (Addr 100),
                          Receive RegE, Branch RegE (Rel 2), Jmp (Rel (0-3)),
                          Read (Addr 101), Receive RegE, Const s RegD,
                          Compute Add RegD RegE RegD, Write RegD (Addr 101),
                          Const 0 RegD, Write RegD (Addr 100)]

lock :: [Instruction]
lock = [Const 5 RegD, Compute Add RegD RegB RegD,
        TestAndSet (Deref RegD), Receive RegE, Branch (Rel 2), Jmp (Rel (0-3))]

unlock :: [Instruction]
unlock = [Const 5 RegD, Compute Add RegD RegB RegD, Store Zero (Deref RegD)]

loadNode :: code -> [Instruction]
loadNode c = [Read (Deref RegB), Receive RegD, Store RegD (Addr (ptrWork c)), --Load type
              Const 1 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegB), Receive RegD, Store RegD (Addr ((ptrWork c) + 1)), --Load value
              Const 2 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegB), Receive RegD, Store RegD (Addr ((ptrWork c) + 2)), --Load next
              Const 3 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegB), Receive RegD, Store RegD (Addr ((ptrWork c) + 3)), --Load previous
              Const 4 RegD, Compute Add RegD RegB RegD,
              Read (Deref RegB), Receive RegD, Store RegD (Addr ((ptrWork c) + 4)), --Load begin
              Const 0 RegD,
              Store RegD (Addr ((ptrWork c) + 5))] -- Store 0 to dirty flag

storeNode :: code -> [Instruction]
storeNode c =[Load (Addr ((ptrWork c))+5) RegD, Branch RegD (Rel 2),
              Jmp (Rel 19), -- Jump over store node
              Load (Addr (ptrWork c)) RegD, Write RegD (Deref RegB), --Store Type
              Const 1 RegD, Compute RegD RegB RegE,
              Load (Addr ((ptrWork c)+1)) RegD, Write RegD (Deref RegE), --Store value
              Const 2 RegD, Compute RegD RegB RegE,
              Load (Addr ((ptrWork c)+2)) RegD, Write RegD (Deref RegE), --Store next
              Const 3 RegD, Compute RegD RegB RegE,
              Load (Addr ((ptrWork c)+3)) RegD, Write RegD (Deref RegE), --Store previous
              Const 4 RegD, Compute RegD RegB RegE,
              Load (Addr ((ptrWork c)+4)) RegD, Write RegD (Deref RegE) --Store begin
              ]

compile :: Code -> Code -> [Operand] -> [Instruction]
compile [] _ _ = []
compile ((RealOp x):xs) c s = case x of
      ',' -> [Read (Addr 0x1000000), Receive RegD, Const b RegE, --Write type
          Store RegE (Addr w), Store RegD (Addr (w+1)),  --Write value
          Const 1 RegD, Store RegD (Addr (w+5))] ++ compile xs c s --Write dirty flag
      '.' -> [Load (Addr (w+1)) RegD, Write RegD (Addr 0x1000000)] ++ compile xs c s
      '<' -> storeNode ++ unlock ++ [Load (Addr (w+3)) RegB] ++ lock ++
               [Branch RegB (Rel 4),
                Const 1 RegD, Write RegD (Addr 0x1000000), EndProg]
             ++ loadNode ++ compile xs c s
      '>' -> storeNode ++ unlock ++ [Load (Addr (w+2)) RegB] ++ lock ++
               [Branch RegB (Rel 4),
                Const 1 RegD, Write RegD (Addr 0x1000000), EndProg]
             ++ loadNode ++ compile xs c s
      '^' -> [Store Zero (Addr (w)), Store Zero (Addr (w+1)), --Clear node
              Const 1 RegD, Store RegD (Addr (w+5))] --Write dirty flag
              ++ compile xs c s
      --TODO *
      '*' -> [Load (Deref RegA) RegD, Branch RegD (Rel (10 + allocSize))]
          ++ (allocateSharedMem 10) ++[
          Write RegB (Deref RegE),                 --Store current node as parent
          --TMNode setup done, threadcount and lock can stay 0.
          --Now setting up the first node
          Const 7 RegD, Compute Add RegD RegE RegD,
          Write RegE (Deref RegD), --Save TMNode into begin of first node
          --First node setup done, now old node updating
          Const 1 RegD, Store RegD (Addr (w+5)), --Set dirty flag
          Const t RegD, Store RegD (Addr w), --Set new type
          Store RegE (Addr (w+1))] ++ --Store new TMNode as value
          --End of setting up new chain
          storeNode ++ unlock ++ [Load (Addr (w+1)) RegB] ++ lock ++ loadNode ++ compile xs c s
      --TODO &
      '&' -> [Const 4 RegD, Compute Add RegD RegA RegD, Load (Deref Reg,
          Const root RegE, Compute Sub RegE RegD RegE,
          Branch RegE (Rel 8), Const b RegD, Store RegD (Deref RegA),
          Const 1 RegD, Compute Add RegD RegA RegD, Const x2 RegE,
          Store RegE (Deref RegD), Jump (Rel 4),
          Const 2 RegD, Write RegD (Addr 0x1000000), EndProg] ++ compile (tail xs) c s

      --End TODO

      '@' -> [Compute Add RegB Zero RegA] ++ compile xs c s
      '$' -> storeNode ++ [Compute Add RegB Zero RegA, Compute Add RegA Zero RegB]
             ++ loadNode ++ compile xs c s

      --TODO
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
      --END TODO

      '[' -> [Load (Addr (w+1)) RegD,
          Branch RegD (Rel 2), Jump (Rel (whileLoopLen + 1))] ++ whileLoop ++
          [Jump (Rel (0-(whileLoopLen + 2)))] ++ compile whileLoopXs c s
      ']' -> error "Brackets are mismatched: Illegal ']' found \n"
      '(' -> [Load (Addr (w+1)) RegD,
          Branch RegD (Rel ifLoopLen)] ++ ifLoop ++ compile ifLoopXs c s
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
            ifLoop = compile (findIfLoop xs 0) c s
            ifLoopXs = drop (length (findIfLoop xs 0) + 1) xs
            ifLoopLen = fromIntegral (length ifLoop +1)
            def1 = drop (findNext xs (RealOp ':') + 1) xs
            def = drop (findNext def1 (RealOp ':') +1) def1
            comment = drop (findNext xs (RealOp '%') + 1) xs
            fAddress = fromIntegral (getFunctionAddress (RealOp x) c)
            w = ptrWork c
            alloc5 = allocateSharedMem 5
            allocSize = length (alloc5)

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
