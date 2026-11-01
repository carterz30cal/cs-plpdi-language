data BaseType = Str | Boolean | Int | Float | Any
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
    | Empty
    deriving (Show)
-- simmers expressions down into a subset of options which can be worked on
data Result
    = ResultLiteral Literal
    | ResultConstruct String [Expr]
    deriving (Show)

equality :: Maybe Result -> Maybe Result -> Context -> Bool
equality (Just (ResultLiteral l1)) (Just (ResultLiteral l2)) _ = literalMatch l1 l2
equality (Just (ResultConstruct name1 fields1)) (Just (ResultConstruct name2 fields2)) context =
    name1 == name2 && length fields1 == length fields2 && all
            (\(x, y) ->
                equality (evaluate x context) (evaluate y context) context)
            (zip fields1 fields2)
equality Nothing Nothing _ = True
equality _ _ _ = False

literalMatch :: Literal -> Literal -> Bool
literalMatch i j = i == j

typeMatch :: Type -> Type -> Bool
typeMatch (Basic b1) (Basic b2) = b1 == b2
typeMatch (Defined n1 _) (Defined n2 _) = n1 == n2
typeMatch _ _ = False

rtype :: Result -> Context -> Type
rtype (ResultLiteral (IntegerLiteral _)) _ = Basic Int
rtype (ResultLiteral (FloatLiteral _)) _ = Basic Float
rtype (ResultLiteral (StringLiteral _)) _ = Basic Str
rtype (ResultLiteral (BooleanLiteral _)) _ = Basic Boolean

ltype :: Literal -> Type
ltype (IntegerLiteral _) = Basic Int
ltype (FloatLiteral _) = Basic Float
ltype (StringLiteral _) = Basic Str
ltype (BooleanLiteral _) = Basic Boolean

cast :: Literal -> Type -> Maybe Literal
cast (IntegerLiteral i) (Basic Int) = Just (IntegerLiteral i)
cast (IntegerLiteral i) (Basic Float) = Just (FloatLiteral (fromIntegral i))
cast (IntegerLiteral i) (Basic Str) = Just (StringLiteral (show i))
cast (FloatLiteral f) (Basic Float) = Just (FloatLiteral f)
cast (FloatLiteral f) (Basic Str) = Just (StringLiteral (show f))
cast (StringLiteral s) (Basic Str) = Just (StringLiteral s)
cast (BooleanLiteral b) (Basic Boolean) = Just (BooleanLiteral b)
cast lit (Basic Any) = Just lit
cast lit _ = Just lit



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
    | If Expr Sequence (Maybe Sequence)
    | Block [Statement]
    deriving (Show)

data Sequence = Seq Statement Sequence | End
    deriving (Show)


data Context = Context
    { variables :: [(String, (Type, Expr))]
    , immutables :: [(String, (Type, Expr))]
    , functions :: [(String, Type, FunctionBody)]
    , types :: [(String, [Constructor])]
    }
    deriving (Show)

variable :: String -> Context -> Maybe (Type, Expr)
variable name context =
    case lookup name (variables context) of
        Just (t, expr) -> Just (t, expr)
        Nothing -> lookup name (immutables context)

add :: Result -> Result -> Maybe Result
add (ResultLiteral left) (ResultLiteral right) = do
    if typeMatch (ltype left) (ltype right) then do
        case (left, right) of
            (IntegerLiteral l, IntegerLiteral r) ->
                Just (ResultLiteral (IntegerLiteral (l + r)))
            (FloatLiteral l, FloatLiteral r) ->
                Just (ResultLiteral (FloatLiteral (l + r)))
            _ -> Nothing
    else do
        let cleft = cast left (ltype right)
        let cright = cast right (ltype left)
        case (cleft, cright) of
            (Just l, Just r) -> add (ResultLiteral l) (ResultLiteral r)
            _ -> Nothing
add _ _ = Nothing

sub :: Result -> Result -> Maybe Result
sub (ResultLiteral left) (ResultLiteral right) = do
    if typeMatch (ltype left) (ltype right) then do
        case (left, right) of
            (IntegerLiteral l, IntegerLiteral r) ->
                Just (ResultLiteral (IntegerLiteral (l - r)))
            (FloatLiteral l, FloatLiteral r) ->
                Just (ResultLiteral (FloatLiteral (l - r)))
            _ -> Nothing
    else do
        let cleft = cast left (ltype right)
        let cright = cast right (ltype left)
        case (cleft, cright) of
            (Just l, Just r) -> sub (ResultLiteral l) (ResultLiteral r)
            _ -> Nothing
sub _ _ = Nothing

mult :: Result -> Result -> Maybe Result
mult (ResultLiteral left) (ResultLiteral right) = do
    if typeMatch (ltype left) (ltype right) then do
        case (left, right) of
            (IntegerLiteral l, IntegerLiteral r) ->
                Just (ResultLiteral (IntegerLiteral (l * r)))
            (FloatLiteral l, FloatLiteral r) ->
                Just (ResultLiteral (FloatLiteral (l * r)))
            _ -> Nothing
    else do
        let cleft = cast left (ltype right)
        let cright = cast right (ltype left)
        case (cleft, cright) of
            (Just l, Just r) -> mult (ResultLiteral l) (ResultLiteral r)
            _ -> Nothing
mult _ _ = Nothing

divide :: Result -> Result -> Maybe Result
divide (ResultLiteral left) (ResultLiteral right) = do
    if typeMatch (ltype left) (ltype right) then do
        case (left, right) of
            (IntegerLiteral l, IntegerLiteral r) ->
                if r == 0
                    then Nothing
                    else Just (ResultLiteral (IntegerLiteral (div l r)))
            (FloatLiteral l, FloatLiteral r) ->
                if r == 0
                    then Nothing
                    else Just (ResultLiteral (FloatLiteral (l / r)))
            _ -> Nothing
    else do
        let cleft = cast left (ltype right)
        let cright = cast right (ltype left)
        case (cleft, cright) of
            (Just l, Just r) -> divide (ResultLiteral l) (ResultLiteral r)
            _ -> Nothing
divide _ _ = Nothing

power :: Result -> Result -> Maybe Result
power (ResultLiteral left) (ResultLiteral right) = do
    if typeMatch (ltype left) (ltype right) then do
        case (left, right) of
            (IntegerLiteral l, IntegerLiteral r) ->
                Just (ResultLiteral (IntegerLiteral (l ^ r)))
            (FloatLiteral l, FloatLiteral r) ->
                Just (ResultLiteral (FloatLiteral (l ** r)))
            _ -> Nothing
    else do
        let cleft = cast left (ltype right)
        let cright = cast right (ltype left)
        case (cleft, cright) of
            (Just l, Just r) -> power (ResultLiteral l) (ResultLiteral r)
            _ -> Nothing
power _ _ = Nothing

modulo :: Result -> Result -> Maybe Result
modulo (ResultLiteral (IntegerLiteral l)) (ResultLiteral (IntegerLiteral r)) =
    if r == 0
        then Nothing
        else Just (ResultLiteral (IntegerLiteral (mod l r)))
modulo _ _ = Nothing

negation :: Result -> Maybe Result
negation (ResultLiteral (IntegerLiteral v)) = Just (ResultLiteral (IntegerLiteral (-v)))
negation (ResultLiteral (FloatLiteral v)) = Just (ResultLiteral (FloatLiteral (-v)))
negation _ = Nothing

arithmetic :: ArithmeticOperation Expr -> Context -> Maybe Result
arithmetic (ArithmeticValue (Literal lit)) _ = Just (ResultLiteral lit)
arithmetic (ArithmeticValue (Variable name)) context = do
    let var = variable name context
    case var of
        Just (t, expr) -> evaluate expr context
        Nothing -> Nothing
arithmetic (ArithmeticValue _) _ = Nothing -- unfinished!
arithmetic (AddOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    add l r
arithmetic (SubOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    sub l r
arithmetic (MultiplyOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    mult l r
arithmetic (DivideOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    divide l r
arithmetic (PowOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    power l r
arithmetic (ModuloOp left right) context = do
    l <- arithmetic left context
    r <- arithmetic right context
    modulo l r
arithmetic (NegateOp expr) context = do
    u <- arithmetic expr context
    negation u

truth :: TruthyOperation Expr -> Context -> Maybe Result
truth (TruthyValue (Literal lit)) _ = Just (ResultLiteral lit)
truth (TruthyValue (Variable name)) context = do
    let var = variable name context
    case var of
        Just (t, expr) -> evaluate expr context
        Nothing -> Nothing
truth (TruthyValue _) _ = Nothing -- maybe there are other cases to consider???
truth (NotOp expr) context = 
    case truth expr context of
        Just (ResultLiteral (BooleanLiteral v)) -> Just (ResultLiteral (BooleanLiteral (not v)))
        _ -> Nothing
truth (EqualOp left right) context = do
    let l = evaluate left context
    let r = evaluate right context
    if equality l r context
        then Just (ResultLiteral (BooleanLiteral True))
        else Just (ResultLiteral (BooleanLiteral False))

evaluate :: Expr -> Context -> Maybe Result
evaluate Empty _ = Nothing
evaluate (Literal lit) _ = Just (ResultLiteral lit)
evaluate (Arithmetic op) context = arithmetic op context
evaluate (Variable name) context =
    case lookup name (variables context) of
        Just (t, expr) -> evaluate expr context
        Nothing   -> do 
            case lookup name (immutables context) of
                Just (t, expr) -> evaluate expr context
                Nothing -> Nothing

-- basically just makes all variables immutable within the inner context#
-- function calls need to clear the immutables/variables that are not present in scope
freeze :: Context -> Context
freeze context = context { immutables = variables context ++ immutables context, variables = [] }

execute :: Sequence -> Context -> IO ()
execute End _ = return ()
execute (Seq Pass rest) context = execute rest context
execute (Seq (Emit name expr) rest) context = do
    case evaluate expr context of
        Just (ResultLiteral lit) -> putStrLn $ name ++ ": " ++ show lit
        Just (ResultConstruct con fields) -> putStrLn $ name ++ ": " ++ con ++ " " ++ show fields
        Nothing -> putStrLn $ name ++ ": " ++ "failed to emit expression"
    execute rest context -- not proper input/output but i think this is good enough as a demo
execute (Seq (Declare (VariableDec t name assignment)) rest) context = do
    let newContext = context { variables = (name, (t, assignment)) : variables context }
    execute rest newContext
execute (Seq (If condition thenExpr elseExpr) rest) context = do
    case condition of 
        Truth (truthy) -> do 
            let result = truth truthy context
            case result of
                Just (ResultLiteral (BooleanLiteral True)) -> do
                    execute thenExpr (freeze context)
                Just (ResultLiteral (BooleanLiteral False)) -> do
                    case elseExpr of
                        Just elseSeq -> execute elseSeq (freeze context)
                _ -> putStrLn "Condition did not evaluate"
            execute rest context
        _ -> putStrLn "Condition is not a truthy expression"

exampleContext :: Context
exampleContext = Context
    { variables = [("test", ((Basic Int), Literal (IntegerLiteral 3)))]
    , immutables = []
    , functions = []
    , types = []
    }

exampleTree :: Expr
exampleTree = Arithmetic (AddOp
    (ArithmeticValue (Variable "test"))
    (ArithmeticValue (Literal (FloatLiteral 9.5))))

exampleResult :: Maybe Result
exampleResult = evaluate exampleTree exampleContext

exampleProgram :: Sequence
exampleProgram =
    Seq
        (If
            (Truth (EqualOp
                (Arithmetic (MultiplyOp
                    (ArithmeticValue (Variable "test"))
                    (ArithmeticValue (Literal (IntegerLiteral 9)))))
                (Literal (IntegerLiteral 27))))
            (Seq (Emit "result" (Literal (IntegerLiteral 27))) End)
            Nothing)
        End
