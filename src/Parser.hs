{-# LANGUAGE OverloadedStrings #-}

module Parser (langparse) where

import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void (Void)
import Interpreter (Expr(..), Literal(..), Statement(..), Sequence(..), Declaration(..), Constructor(..),
                        Type(..), BaseType(..))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

keyword :: Text -> Parser ()
keyword word = lexeme (string word *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = lexeme $
  (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

stringVal :: Parser String
stringVal = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

stringLiteral :: Parser Expr
stringLiteral = Literal . StringLiteral <$> stringVal



toSequence :: [Statement] -> Sequence
toSequence = foldr Seq End

integerOrFloat :: Parser Expr
integerOrFloat = lexeme $ choice
  [ try $ do
      whole <- some digitChar
      void (char '.')
      fractional <- some digitChar
      pure (Literal (FloatLiteral (read (whole ++ "." ++ fractional))))
  , Literal . IntegerLiteral <$> L.decimal
  ]

typeParser :: Parser Type
typeParser = 
    choice [
        Basic Num     <$ keyword "num",
        Basic Str     <$ keyword "str",
        Basic Boolean <$ keyword "bool",
        Basic Any     <$ keyword "any",
        namedType
    ]
    where
        namedType = do
            name <- identifier
            args <- optional $ between (symbol "[") (symbol "]") (typeParser `sepBy1` symbol ",")
            pure $ case args of 
                Just (args') -> Defined name args'
                Nothing -> Defined name []

expr :: Parser Expr
expr = lexeme $ choice
  [ 
  ]



literal = lexeme $ choice
    [
        integerOrFloat
        , stringLiteral
        , Literal (BooleanLiteral True) <$ keyword "true"
        , Literal (BooleanLiteral False) <$ keyword "false"
    ]

assignmentOrDefinition :: Parser Statement
assignmentOrDefinition = undefined

constructor :: Parser Constructor
constructor = do
    name <- identifier
    args <- optional $ between (symbol "(") (symbol ")") (typeParser `sepBy1` symbol ",")
    pure $ case args of
        Just (args') -> Con name args'
        Nothing -> Con name []

typeDeclaration :: Parser Declaration
typeDeclaration = do
    keyword "type"
    name <- identifier
    targs <- many identifier
    symbol "="
    constructors <- constructor `sepBy1` keyword "or"
    pure (TypeDec name targs constructors)

variableDeclaration :: Parser Declaration
variableDeclaration = do
    keyword "let"
    vart <- typeParser
    name <- identifier
    init <- optional $ (symbol "=" *> expr)
    pure $ case init of 
        Just (initializer) -> VariableDec vart name initializer
        Nothing -> VariableDec vart name Empty


declaration :: Parser Declaration
declaration = typeDeclaration <|> variableDeclaration

patternParser :: Parser 

passParse :: Parser Statement
passParse = Pass <$ keyword "pass"

emitParse :: Parser Statement
emitParse = Emit <$> (keyword "emit" *> (identifier)) <*> expr

declareParse :: Parser Statement
declareParse = Declare <$> declaration

functionDecParse :: Parser Statement
functionDecParse = do
    funt <- typeParser
    keyword "function"
    name <- identifier


langparse = undefined