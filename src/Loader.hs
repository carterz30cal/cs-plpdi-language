{-# LANGUAGE OverloadedStrings #-}
module Loader(loadProgram) where

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty)
import Parser (parseImports, parseProgram)

loadProgram :: FilePath -> IO Text
loadProgram path = do
    source <- TIO.readFile path
    case parseImports source of
        Left err -> ioError (userError (errorBundlePretty err))
        Right (imports, rest) -> do
            importedSources <- forM imports $ \name ->
                loadProgram (takeDirectory path </> name)
            pure (T.intercalate "\n" importedSources <> "\n" <> rest)