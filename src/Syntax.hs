module Syntax ( Expr(..), Literal(..), Statement(..), Sequence(..), Declaration(..)
  , Constructor(..), Pattern(..), Type(..), BaseType(..), FunctionBody(..)
  , ArithmeticOperation(..), TruthyOperation(..), Context(..), Result(..)
  ) where

data BaseType = Str | Boolean | Num | Int | Float | Any
    deriving (Eq, Show)

data Type
    = Basic BaseType
    | TypeVariable String
    | Defined String [Type]
    deriving (Show)

data Literal
    = StringLiteral String
    | IntegerLiteral Integer
    | FloatLiteral Double
    | BooleanLiteral Bool
    deriving (Eq, Show)

data Constructor = Con String [Type]
    deriving (Show)

data Declaration
    = VariableDec Type String Expr
    | TypeDec String [String] [Constructor]
    deriving (Show)

data Pattern
    = Wildcard
    | Binding String
    | LiteralPattern Literal
    | ConstructorPattern String [Pattern]
    deriving (Show)


data ArithmeticOperation a
    = ArithmeticValue a
    | AddOp (ArithmeticOperation a) (ArithmeticOperation a)
    | SubOp (ArithmeticOperation a) (ArithmeticOperation a)
    | MultiplyOp (ArithmeticOperation a) (ArithmeticOperation a)
    | DivideOp (ArithmeticOperation a) (ArithmeticOperation a)
    | PowOp (ArithmeticOperation a) (ArithmeticOperation a)
    | ModuloOp (ArithmeticOperation a) (ArithmeticOperation a)
    | NegateOp (ArithmeticOperation a)
    deriving (Show)

data TruthyOperation a
    = TruthyValue a
    | AndOp (TruthyOperation a) (TruthyOperation a)
    | OrOp (TruthyOperation a) (TruthyOperation a)
    | NotOp (TruthyOperation a)
    | EqualOp a a
    | NotEqualOp a a
    | GreaterThanOp (ArithmeticOperation a) (ArithmeticOperation a)
    | GreaterThanEqualOp (ArithmeticOperation a) (ArithmeticOperation a)
    | LessThanOp (ArithmeticOperation a) (ArithmeticOperation a)
    | LessThanEqualOp (ArithmeticOperation a) (ArithmeticOperation a)
    deriving (Show)

-- Expressions produce values. A Call has a list of arguments; Construct
-- holds expressions for constructor fields, including lazy fields.
data Expr
    = Literal Literal
    | Variable String
    | Node Expr Context
    | Call String [Expr]
    | Construct String [Expr]
    | Inhale String -- unsafe???
    | Arithmetic (ArithmeticOperation Expr)
    | Truth (TruthyOperation Expr)
    | Empty
    deriving (Show)
-- simmers expressions down into a subset of options which can be worked on
data Result
    = ResultLiteral Literal
    | ResultConstruct String [Expr]
    | ResultPartialFunc String [Expr]
    deriving (Show)

-- A function clause has its own patterns and a final return expression.
-- Repeated FunctionDec nodes retain source order for pattern matching.
data FunctionBody = FunctionBody [Maybe String] Sequence Expr
    deriving (Show)

data Statement
    = Pass
    | Declare Declaration
    | FunctionDec Type String [Pattern] FunctionBody
    | Emit String Expr
    | Assignment String Expr
    | For Declaration Expr Statement Sequence
    | Loop Expr Statement Sequence
    | If Expr Sequence (Maybe Sequence)
    | Block [Statement]
    deriving (Show)

data Sequence = Seq Statement Sequence | End
    deriving (Show)


data Context = Context
    { variables :: [(String, (Type, Expr))]
    , immutables :: [(String, (Type, Expr))]
    , functions :: [(String, (Type, [([Pattern], FunctionBody)]))]
    , constructors :: [(String, Constructor)]
    , types :: [(String, [Constructor])]
    }
    deriving (Show)