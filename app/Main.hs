{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO as TIO
import Parser (parseProgram)
import Text.Megaparsec (errorBundlePretty)
import Interpreter (execute, exampleContext)

main :: IO ()
main = do
    source <- TIO.readFile "program.lang"
    case parseProgram source of
        Left err  -> putStrLn (errorBundlePretty err)
        Right ast -> do
            putStrLn (show ast)
            _ <- execute ast exampleContext
            pure ()
