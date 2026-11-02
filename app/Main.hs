{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Interpreter (execute, exampleContext)
import Parser (parseProgram)
import Text.Megaparsec
import Data.Text (Text)

program :: Text
program = "pass; let num test = 6; emit \"val\" test;"

fibonacci :: Text
fibonacci = "num function fibonacci 0 = 0;num function fibonacci 1 = 1;num function fibonacci x = fibonacci(x-1) + fibonacci(x - 2);emit \"result\" fibonacci(5);"


main :: IO ()
main = do
  putStrLn "running program woo"
  case parseProgram fibonacci of
    Left err -> putStrLn (errorBundlePretty err)
    Right ast -> do
      _ <- Interpreter.execute ast Interpreter.exampleContext
      pure ()
