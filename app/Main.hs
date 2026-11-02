module Main (main) where

import qualified Interpreter (execute, exampleFactorialProgram, exampleContext, Context(..))

main :: IO Interpreter.Context
main = do
  putStrLn "Hello, Haskell!"
  Interpreter.execute Interpreter.exampleFactorialProgram Interpreter.exampleContext
