{-# LANGUAGE OverloadedStrings #-}

module Parser (parseProgram, parseExpr, parseImports) where

import qualified Data.Text.IO as TIO
import Control.Applicative (empty, (<|>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Text (Text)
import Data.Void (Void)
import Syntax
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
keyword word = lexeme (string word *> notFollowedBy (alphaNumChar <|> char '_'))

identifier :: Parser String
identifier = lexeme $
  (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

stringVal :: Parser String
stringVal = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

stringLiteral :: Parser Expr
stringLiteral = Literal . StringLiteral <$> stringVal

file :: Parser String
file = do
    name <- identifier
    symbol ".otwo"
    pure (name ++ ".otwo")


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

-- apparently my method didn't work very well so here's a version that does
chainLeft
    :: Parser a
    -> Parser (a -> a -> a)
    -> Parser a
chainLeft operand operator = do
    first <- operand
    go first
  where
    go left =
          (do
              combine <- operator
              right <- operand
              go (combine left right))
      <|> pure left

arithmetic :: Parser (ArithmeticOperation Expr)
arithmetic = addSub

addSub :: Parser (ArithmeticOperation Expr)
addSub = chainLeft mulDiv $
      (AddOp <$ symbol "+")
  <|> (SubOp <$ symbol "-")

mulDiv :: Parser (ArithmeticOperation Expr)
mulDiv = chainLeft unary $
      (MultiplyOp <$ symbol "*")
  <|> (DivideOp   <$ symbol "/")
  <|> (ModuloOp   <$ symbol "%")

unary :: Parser (ArithmeticOperation Expr)
unary =
      (NegateOp <$> (symbol "-" *> unary))
  <|> power

power :: Parser (ArithmeticOperation Expr)
power = do
    left <- arithmeticAtom
    (PowOp left <$> (symbol "^" *> unary))
      <|> pure left

arithmeticAtom :: Parser (ArithmeticOperation Expr)
arithmeticAtom =
      between (symbol "(") (symbol ")") arithmetic
  <|> ArithmeticValue <$> valueExpr

valueExpr :: Parser Expr
valueExpr = choice 
    [
        integerOrFloat,
        bool,
        call,
        inhale,
        variable
    ]

number :: Parser (ArithmeticOperation Expr)
number = try $ do
    n <- integerOrFloat
    pure $ ArithmeticValue (n)

bool :: Parser (Expr)
bool = try $ choice
    [
        Literal (BooleanLiteral True) <$ keyword "true",
        Literal (BooleanLiteral False) <$ keyword "false"
    ]

variable :: Parser Expr
variable = try $ do
    name <- identifier
    pure $ Variable name

arithmeticVariable :: Parser (ArithmeticOperation Expr)
arithmeticVariable = try $ do 
    v <- variable
    pure $ ArithmeticValue (v)

logicVariable :: Parser (TruthyOperation Expr)
logicVariable = try $ do
    v <- variable
    pure $ TruthyValue (v)

truthy :: Parser (TruthyOperation Expr)
truthy = orTruthy

orTruthy  = chainLeft andTruthy (OrOp <$ symbol "||")
andTruthy = chainLeft notTruthy (AndOp <$ symbol "&&")

comparison :: Parser (TruthyOperation Expr)
comparison = do
    lhs <- arithmetic
    choice
      [ EqualOp (Arithmetic lhs) <$> (symbol "==" *> expr)
      , NotEqualOp (Arithmetic lhs) <$> (symbol "!=" *> expr)
      , GreaterThanEqualOp lhs <$> (symbol ">=" *> arithmetic)
      , GreaterThanOp lhs <$> (symbol ">" *> arithmetic) -- see note below
      , LessThanEqualOp lhs <$> (symbol "<=" *> arithmetic)
      , LessThanOp lhs <$> (symbol "<" *> arithmetic)
      ]

notTruthy =
      (NotOp <$> (symbol "!" *> notTruthy))
  <|> try comparison
  <|> logicAtomic



logicAtomic :: Parser (TruthyOperation Expr)
logicAtomic = 
    between (symbol "(") (symbol ")") truthy <|> TruthyValue <$> bool


arithmeticParse :: Parser Expr
arithmeticParse = do
    a <- arithmetic
    pure $ Arithmetic a

truthParse :: Parser Expr
truthParse = do
    t <- truthy
    pure $ Truth t

inhale :: Parser Expr
inhale = do
    keyword "inhale"
    name <- stringVal
    pure $ Inhale name

construct :: Parser Expr
construct = try $ do
    name <- identifier
    symbol "["
    paras <- optional $ (expr `sepBy1` symbol ",")
    symbol "]"
    pure $ case paras of
        Just (p') -> Construct name p'
        Nothing -> Construct name []
-- thought: construct and call are basically identical??
call :: Parser Expr
call = try $ do
    name <- identifier
    symbol "("
    paras <- optional $ (expr `sepBy1` symbol ",")
    symbol ")"
    pure $ case paras of
        Just (p') -> Call name p'
        Nothing -> Call name []

nullExpr :: Parser Expr -- will obviously error if ever actually evaluated
nullExpr = do
    keyword "null"
    pure Empty


expr :: Parser Expr
expr = lexeme $ choice
    [ 
        inhale,
        try truthParse,
        construct,
        arithmeticParse,
        call,
        nullExpr,
        literal,
        variable
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
    _ <- symbol "="
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


wildcardPatternParser :: Parser Pattern
wildcardPatternParser = do
    symbol "_"
    pure $ Wildcard

literalPatternParser :: Parser Pattern
literalPatternParser = try $ do
    (Literal lit) <- literal
    pure $ LiteralPattern lit


bindingPatternParser :: Parser Pattern
bindingPatternParser = do
    name <- identifier
    pure $ Binding name

constructorPatternParser :: Parser Pattern
constructorPatternParser = try $ do
    name <- identifier
    symbol "["
    args <- optional $ (patternParser `sepBy1` symbol ",")
    symbol "]"
    pure $ case args of
        Just (args') -> ConstructorPattern name args'
        Nothing -> ConstructorPattern name []


patternParser :: Parser Pattern
patternParser = wildcardPatternParser <|> literalPatternParser <|> constructorPatternParser <|> bindingPatternParser


passParse :: Parser Statement
passParse = Pass <$ keyword "pass"

emitParse :: Parser Statement
emitParse = Emit <$> (keyword "emit" *> (stringVal)) <*> expr

declareParse :: Parser Statement
declareParse = Declare <$> declaration

bindings :: [Pattern] -> [Maybe String]
bindings [] = []
bindings ((Binding name):xs) = (Just name):(bindings xs)
bindings (_:xs) = Nothing:(bindings xs)

functionDecParseBody :: Parser Statement
functionDecParseBody = try $ do -- so it unconsumes the stuff if fails otherwise the one-liner version may not work
    funt <- typeParser
    keyword "function"
    name <- identifier
    paras <- many patternParser
    _ <- symbol "="
    _ <- symbol "{"
    body <- manyTill
        langparse
        (keyword "return")
    rexpr <- expr
    _ <- symbol ";"
    _ <- symbol "}"
    pure $ FunctionDec funt name paras (FunctionBody (bindings paras) (toSequence body) rexpr)

functionDecParseOneLiner :: Parser Statement
functionDecParseOneLiner = try $ do
    funt <- typeParser
    keyword "function"
    name <- identifier
    paras <- many patternParser
    _ <- symbol "="
    rexpr <- expr
    pure $ FunctionDec funt name paras (FunctionBody (bindings paras) End rexpr)

functionDecParse :: Parser Statement
functionDecParse = functionDecParseBody <|> functionDecParseOneLiner

assignmentParse :: Parser Statement
assignmentParse = do
    vname <- identifier
    _ <- symbol "="
    rhexpr <- expr
    pure $ Assignment vname rhexpr


blockParser :: Parser Sequence
blockParser = between (symbol "{") (symbol "}") collect

forParse :: Parser Statement
forParse = do
    keyword "for"
    symbol "("
    def <- declaration
    symbol ","
    condition <- expr -- replace with explicit truth expr, cannot parse anything else and remain in a good state
    symbol ","
    iterator <- assignmentLike
    symbol ")"
    body <- blockParser
    pure $ For def condition iterator body

ifParse :: Parser Statement
ifParse = do
    keyword "if"
    symbol "("
    condition <- expr
    symbol ")"
    body <- blockParser
    other <- optional $ (keyword "else" *> blockParser)
    pure $ case other of
        Just (other') -> If condition body (Just other')
        Nothing -> If condition body Nothing

assignmentLike :: Parser Statement
assignmentLike = try sugarPlusAssignment 
            <|> try sugarMinusAssignment
            <|> try sugarMultAssignment
            <|> try sugarDivAssignment
            <|> try sugarModAssignment
            <|> assignmentParse

sugarPlusAssignment :: Parser Statement
sugarPlusAssignment = do
    vname <- identifier
    symbol "+="
    rhexpr <- arithmetic
    pure $ Assignment vname (Arithmetic (AddOp (ArithmeticValue (Variable vname)) rhexpr))

sugarMinusAssignment :: Parser Statement
sugarMinusAssignment = do
    vname <- identifier
    symbol "-="
    rhexpr <- arithmetic
    pure $ Assignment vname (Arithmetic (SubOp (ArithmeticValue (Variable vname)) rhexpr))

sugarMultAssignment :: Parser Statement
sugarMultAssignment = do
    vname <- identifier
    symbol "*="
    rhexpr <- arithmetic
    pure $ Assignment vname (Arithmetic (MultiplyOp (ArithmeticValue (Variable vname)) rhexpr))

sugarDivAssignment :: Parser Statement
sugarDivAssignment = do
    vname <- identifier
    symbol "/="
    rhexpr <- arithmetic
    pure $ Assignment vname (Arithmetic (DivideOp (ArithmeticValue (Variable vname)) rhexpr))

sugarModAssignment :: Parser Statement
sugarModAssignment = do
    vname <- identifier
    symbol "%="
    rhexpr <- arithmetic
    pure $ Assignment vname (Arithmetic (ModuloOp (ArithmeticValue (Variable vname)) rhexpr))


compoundStatement :: Parser Statement
compoundStatement = choice
    [ 
        ifParse,
        forParse,
        functionDecParseBody
    ]

simpleStatement :: Parser Statement
simpleStatement = choice
    [ 
        passParse,
        declareParse,
        emitParse,
        functionDecParseOneLiner,
        assignmentLike
    ]

langparse :: Parser Statement
langparse =
    (compoundStatement <* optional (symbol ";"))
    <|> (simpleStatement <* symbol ";")

collect :: Parser Sequence
collect = do
    sc
    atEnd <- optional (lookAhead (void eof <|> void (symbol "}")))
    case atEnd of
        Just _  -> pure End
        Nothing -> do
            stmt <- compoundStatement
                <|> (simpleStatement <* symbol ";")
            rest <- collect
            pure (Seq stmt rest)

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Sequence
parseProgram = runParser (collect <* eof) "<input>"

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> expr <* eof) "<inhale>"


parseImports :: Text -> Either (ParseErrorBundle Text Void) ([FilePath], Text)
parseImports = runParser importsAndRest "<preprocessor>"

importsAndRest :: Parser ([FilePath], Text)
importsAndRest = do
    sc
    paths <- many (try (preImport <* optional (symbol ";")))
    rest <- getInput
    pure (paths, rest)

preImport :: Parser FilePath
preImport = symbol "#import" *> stringVal
