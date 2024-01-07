module Expression where
import AST (ASTBuilder, identifierToken,oneOfTokens,token,Expr (NilExpr,PrimaryExpr,KeyedExpr,Slice,Index, Selector, VaridicArg,CallArgs, MethodExpr, ConversionExpr, BasicLit, OperandName, CompositeExpr, BinaryExpr, FunctionLit), TypeInfo (StructType), intToken, floatToken, strToken, runeToken, BodyParser, getIden, Stmt (NilStmt))
import Tokenizor (Token (FullStop, LeftSupScript,Comma, Colon, OptionalToken, RightSupScript, Identifier, LeftParen, RightParen, RightBrace, LeftBrace, IntLit, FloatLit, StringLit, RuneLit, Struct, Map, Plus, Minus, Not, Mul, Divide, Mod, LeftShift, RightShift, BitwiseOR, BitwiseXOR, Eq, NotEq, LTEQ, GTEQ, LTH, GTH, CondAND, CondOR))
import Text.Parsec (option,try,many, lookAhead, count,choice,(<?>),(<|>), unexpected)
import Types (singleTypeDecl, typeName, structType, sliceType, mapType,  funcType)
import Debug.Trace ( trace,traceStack)
import Control.Monad (when)
expr:: BodyParser -> ASTBuilder Expr
expr funcBody =   traceStack "evaluating expr"  try $ orOperator funcBody

-- ExpressionList = Expression { "," Expression } .
exprList :: BodyParser -> ASTBuilder [Expr]
exprList funcBody = trace "evaluating exprList" $ do
    e1 <- expr funcBody
    es <- many commaExpr
    return (e1 : es)
    where commaExpr = traceStack "checking for comma seperation" token Comma >> expr funcBody

-- UnaryExpr  = PrimaryExpr | unary_op UnaryExpr .
unaryExpr :: BodyParser -> ASTBuilder Expr
unaryExpr funcBody = let
    unaryOps = [Plus,Minus,Not,BitwiseXOR]
    unary = do
        t <- oneOfTokens unaryOps
        uexpr <- unaryExpr funcBody
        return $ BinaryExpr uexpr t NilExpr
    in trace "evaluating unaryExpr" (primaryExpr funcBody <|> unary)


-- PrimaryExpr =
-- 	Operand |
-- 	Conversion |
-- 	MethodExpr |
-- 	PrimaryExpr Selector |
-- 	PrimaryExpr Index |
-- 	PrimaryExpr Slice |
-- 	PrimaryExpr Arguments .
primaryExpr :: BodyParser -> ASTBuilder Expr
primaryExpr funcBody= let
    si = sliceOrIndex funcBody
    arg = arguments funcBody
    pe x = do
        y <-  primaryExpr funcBody
        PrimaryExpr y <$> x
    in trace "entering primaryExpr" choice $ try <$> [ operand funcBody, conversion funcBody,methodExpr,pe selector,pe si ,pe arg ]

-- Selector       = "." identifier .
selector :: ASTBuilder Expr
selector = trace "evaluating selector" token FullStop >> do
    (Identifier x) <- identifierToken
    return $ Selector x



-- Index          = "[" Expression [ "," ] "]" .
-- Slice          = "[" [ Expression ] ":" [ Expression ] "]" 
sliceOrIndex :: BodyParser -> ASTBuilder Expr
sliceOrIndex funcBody= trace "evaluating sliceOrIndex" token LeftSupScript >> do
    ex <- optionalExpr
    case ex of
        NilExpr -> token Colon >> Slice ex <$> optionalExpr -- found to be slice
        _ -> do -- found first expression, might be slice
            nextToken <- oneOfTokens  [Comma,Colon,RightSupScript]
            case nextToken of
                Comma -> do -- found index
                    _ <- token RightSupScript
                    return $ Index ex
                RightSupScript -> return $ Index ex -- found index
                _ -> do -- found slice
                    ex2 <- optionalExpr
                    _ <- token RightSupScript
                    return $ Slice ex ex2
    where optionalExpr = option NilExpr $ try $ expr funcBody



-- Arguments      = "(" [ ( ExpressionList | Type [ "," ExpressionList ] ) [ "..." ] [ "," ] ] ")" .
arguments :: BodyParser -> ASTBuilder Expr
arguments funcBody = trace "evaluting arguments" token LeftParen >> do
    tok <- option OptionalToken $ lookAhead $ token RightParen
    case tok of
        RightParen -> do
            _ <- token RightParen
            return $ CallArgs []
        _ -> do
            el <- exprList funcBody
            x <-option [] $ try $ count 3 $ token FullStop
            _ <- token RightBrace
            return $ case x of
                [] -> CallArgs el
                _ -> CallArgs $ markVaradic el

    where markVaradic [] = []
          markVaradic [x] = [VaridicArg x]
          markVaradic (x:xs) = x : markVaradic xs


-- MethodName     = identifier .
methodName :: ASTBuilder Token
methodName = identifierToken


-- ReceiverType  = Type .
receiverType :: ASTBuilder TypeInfo
receiverType = singleTypeDecl

-- MethodExpr    = ReceiverType "." MethodName .
methodExpr :: ASTBuilder Expr
methodExpr = trace "evaluating methodExpr"  (receiverType <* token FullStop) >>= (<$> (getIden <$> methodName)) . MethodExpr

-- Conversion = Type "(" Expression ")" .
conversion :: BodyParser -> ASTBuilder Expr
conversion funcBody = trace "evaluating conversion" singleTypeDecl >>= (<$> parser) . ConversionExpr
    where parser = token LeftBrace >> expr funcBody <* token RightBrace


-- Operand     = Literal | OperandName | "(" Expression ")" .
operand :: BodyParser -> ASTBuilder Expr
operand funcBody = let
    optionalExpr =  token LeftParen >> expr funcBody <* token RightParen
    in trace "evaluating operand"  try (literal  funcBody) <|> operandName <|> optionalExpr

-- Literal     = BasicLit | CompositeLit | FunctionLit  .
literal :: BodyParser -> ASTBuilder Expr
literal funcBody = trace "evaluting literal" (basicLit <|> functionLit funcBody <|> try (compositeLit funcBody) )


-- BasicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
basicLit :: ASTBuilder Expr
basicLit =  trace "evaluating basicLit" $ BasicLit <$>  choice [intToken,floatToken,strToken,runeToken]


-- FunctionLit = "func" Signature FunctionBody .
functionLit :: BodyParser ->  ASTBuilder Expr
functionLit funcBody = trace "evaluting functionLit"  funcType >>= (<$> funcBody) . FunctionLit

-- OperandName = identifier | QualifiedIdent .
operandName :: ASTBuilder Expr
operandName = trace "evaluating operandName" $ do
    (Identifier n) <- identifierToken
    x <- option OptionalToken $ token FullStop
    case x of
        FullStop -> do
            -- QualifiedIdent = PackageName "." identifier .
            -- PackageName    = identifier .
            (Identifier n1) <- identifierToken
            return $ OperandName $ n ++ "." ++ n1
        _ -> return $ OperandName n

-- CompositeLit  = LiteralType LiteralValue .
compositeLit :: BodyParser -> ASTBuilder Expr
compositeLit funcBody = trace "evaluting compositeLit" $ do
    lt <- trace "going for this" literalType
    CompositeExpr lt <$> literalVal funcBody <* unexpected "done eval compositeLit"

-- LiteralType   = StructType | SliceType | MapType | TypeName .
literalType :: ASTBuilder TypeInfo
literalType = trace "evaluating literalType" $ do
    x <- lookAhead $ choice [oneOfTokens [Struct,LeftSupScript,Map],typeName ]
    case x of
        Struct -> structType
        LeftSupScript -> sliceType
        Map -> mapType
        _ -> singleTypeDecl


-- LiteralValue  = "{" [ ElementList [ "," ] ] "}" .
literalVal :: BodyParser -> ASTBuilder [Expr]
literalVal funcBody = trace "evaluating literalVal" token LeftBrace >>
     (( elemList <* token RightBrace) <|> (token RightBrace >> return []))
    where elemList = elementList funcBody 
-- Element       = Expression | LiteralValue .
element :: BodyParser -> ASTBuilder [Expr]
element funcBody = trace "evaluating element" literalVal funcBody <|> (:[]) <$> expr funcBody


-- Key           = FieldName | Expression | LiteralValue .
key :: BodyParser -> ASTBuilder [Expr]
key = trace "evaluating key" element   -- we are not doing any special arrangements for fieldName as expr would take care of that

-- KeyedElement  = [ Key ":" ] Element .
keyedElement :: BodyParser -> ASTBuilder [Expr]
keyedElement funcBody = trace "evaluting keyedElement" $ do
    k <- key funcBody -- although key is optional but parser is same for key and element 
    ( trace "about to consume keyedElement Colon" token Colon >> trace "eval keyedExpr" (:[]) . KeyedExpr k <$> element funcBody ) <|> return [KeyedExpr [] k]

-- ElementList   = KeyedElement { "," KeyedElement } .
elementList :: BodyParser -> ASTBuilder [Expr]
elementList funcBody = trace "evaluating elementList" $ do
    ke <- keyedElement funcBody
    kes <- many withComma
    return $ ke ++ concat kes
    where withComma = token Comma >> keyedElement funcBody

binaryOperator :: ASTBuilder Expr -> [Token] -> ASTBuilder Expr
binaryOperator srcExprType ts = trace "evaluating binaryOperator" $  do
    e1 <- srcExprType
    (oneOfTokens ts >>= (<$> binaryOperator srcExprType ts ) .BinaryExpr e1) <|> return e1

-- Below parsers are defined to manage precedence order in binary expressions
mulOperator :: BodyParser ->ASTBuilder Expr
mulOperator funcBody = binaryOperator (unaryExpr funcBody) [Mul,Divide,Mod,LeftShift,RightShift]

addOperator :: BodyParser ->ASTBuilder Expr
addOperator funcBody = binaryOperator (mulOperator funcBody) [Plus,Minus,BitwiseOR,BitwiseXOR]

relOperator :: BodyParser ->ASTBuilder Expr
relOperator funcBody = binaryOperator (addOperator funcBody) [Eq,NotEq,LTH,LTEQ,GTH,GTEQ]

andOperator :: BodyParser ->ASTBuilder Expr
andOperator funcBody = binaryOperator (relOperator funcBody) [CondAND]

orOperator :: BodyParser -> ASTBuilder Expr
orOperator funcBody = trace "eval orOperator"  binaryOperator (andOperator funcBody) [CondOR]
