data BaseType = Str | Boolean | Num | Any
    deriving (Eq, Show)

data Type
    = Basic BaseType
    | TypeVariable String
    | Defined String [Type]
    deriving (Eq, Show)

data Literal
    = StringLiteral String
    | IntegerLiteral Integer
    | FloatLiteral Double
    | BooleanLiteral Bool
    deriving (Eq, Show)

data Constructor = Con String [Type]
    deriving (Eq, Show)

data Declaration
    = VariableDec Type String (Maybe Expr)
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
    | GreaterThanOp a a
    | GreaterThanEqualOp a a
    | LessThanOp a a
    | LessThanEqualOp a a
    deriving (Show)

-- Expressions produce values. A Call has a list of arguments; Construct
-- holds expressions for constructor fields, including lazy fields.
data Expr
    = Literal Literal
    | Variable String
    | Call String [Expr]
    | Construct String [Expr]
    | Inhale String
    | Arithmetic (ArithmeticOperation Expr)
    | Truth (TruthyOperation Expr)
    deriving (Show)

-- A function clause has its own patterns and a final return expression.
-- Repeated FunctionDec nodes retain source order for pattern matching.
data FunctionBody = FunctionBody [Statement] Expr
    deriving (Show)

data Statement
    = Pass
    | Declare Declaration
    | FunctionDec Type String [Pattern] FunctionBody
    | Emit String Expr
    | Assignment String Expr
    | For Declaration Expr Statement [Statement]
    | If Expr [Statement] (Maybe [Statement])
    | Block [Statement]
    deriving (Show)

data Sequence = Seq Statement Sequence | End
    deriving (Show)

type Program = Sequence
