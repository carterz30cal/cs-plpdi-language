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
-- simmers expressions down into a subset of options which can be worked on
data Result
    = ResultLiteral Literal
    | ResultConstruct String [Expr]
    deriving (Show, Eq)

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


data Context = Context
    { variables :: [(String, Expr)]
    , immutables :: [(String, Expr)]
    , functions :: [(String, FunctionBody)]
    , types :: [(String, [Constructor])]
    }
    deriving (Show)

arithmetic :: ArithmeticOperation Expr -> Context -> Maybe Result
arithmetic (ArithmeticValue (Literal lit)) _ = Just (ResultLiteral lit)
arithmetic (ArithmeticValue (Variable name)) context = evaluate (lookup name (variables context)) context 
arithmetic (ArithmeticValue _) _ = Nothing -- unfinished!
arithmetic (AddOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    return $ ResultLiteral (IntegerLiteral (l + r))
arithmetic (SubOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    return $ ResultLiteral (IntegerLiteral (l - r))
arithmetic (MultiplyOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    return $ ResultLiteral (IntegerLiteral (l * r))
arithmetic (DivideOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    if r == 0
        then Nothing
        else return $ ResultLiteral (IntegerLiteral (div l r))
arithmetic (PowOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    return $ ResultLiteral (IntegerLiteral (l ^ r))
arithmetic (ModuloOp left right) context = do
    ResultLiteral (IntegerLiteral l) <- arithmetic left context
    ResultLiteral (IntegerLiteral r) <- arithmetic right context
    if r == 0
        then Nothing
        else return $ ResultLiteral (IntegerLiteral (mod l r))
arithmetic (NegateOp expr) context = do
    ResultLiteral (IntegerLiteral v) <- arithmetic expr context
    return $ ResultLiteral (IntegerLiteral (-v))

truth :: TruthyOperation Expr -> Context -> Maybe Result
truth (TruthyValue (Literal lit)) _ = Just (ResultLiteral lit)
truth (TruthyValue (Variable name)) context = evaluate (lookup name (variables context)) context
truth (TruthyValue _) _ = Nothing -- maybe there are other cases to consider???
truth (NotOp expr) context = 
    case truth expr context of
        Just (ResultLiteral (BooleanLiteral v)) -> Just (ResultLiteral (BooleanLiteral (not v)))
        _ -> Nothing
truth (EqualOp left right) context = do
    ResultLiteral l <- arithmetic left context
    ResultLiteral r <- arithmetic right context
    return $ ResultLiteral (BooleanLiteral (l == r))

evaluate :: Maybe Expr -> Context -> Maybe Result
evaluate Nothing _ = Nothing
evaluate (Just (Literal lit)) _ = Just (ResultLiteral lit)
evaluate (Just (Arithmetic op)) context = arithmetic op context
evaluate (Just (Variable name)) context =
    case lookup name (variables context) of
        Just expr -> evaluate (Just expr) context
        Nothing   -> evaluate (lookup name (immutables context)) context

execute :: Sequence -> Context -> IO ()
execute End _ = return ()
execute (Seq Pass rest) context = execute rest context

exampleContext :: Context
exampleContext = Context
    { variables = [("test", Literal (IntegerLiteral 3))]
    , immutables = []
    , functions = []
    , types = []
    }

exampleTree :: ArithmeticOperation Expr
exampleTree = MultiplyOp
    (ArithmeticValue (Variable "test"))
    (ArithmeticValue (Literal (IntegerLiteral 9)))

exampleResult :: Maybe Result
exampleResult = arithmetic exampleTree exampleContext
