{-# LANGUAGE RecordWildCards #-}
module Compiler where
import Sprockell.System
import Sprockell.TypesEtc
import System.IO.Unsafe
import System.IO
import System.Environment (getArgs)
import Parser

main = do
  args <- getArgs
  let fs = args
  let stl = unsafePerformIO (readFile "include/stl.obf")
  let input = concat (map  (unsafePerformIO . readFile $) (fs))
  let c = filterComments (stl ++ input)
  let code = map charToOp (removeWhiteSpace c)
  run 3 (link code)
