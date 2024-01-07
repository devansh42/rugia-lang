module Statement where
import AST (ASTBuilder,emptyString,Stmt(ExprStmt,ReturnStmt,SwitchStmt,WhileStmt, AssignmentStmt, LabelStmt, NilStmt, BreakStmt, ContinueStmt, GotoStmt, IfStmt, WhileStmt, ForStmt, ForRangeStmt),token, identifierToken, listParser, Expr (NilExpr), oneOfTokens, Block)
import Tokenizor (Token(Assign, Return,Case,Default, Identifier,SemiColon, Colon, Break, OptionalToken, Continue, Goto, LeftBrace, RightBrace, If, Else, Switch, For, Range, NewAssign))
import qualified Expression as Expr (expr, exprList  )
import Text.Parsec (choice,try,option,many,(<|>), lookAhead, unexpected)
import Declaration (shortVarDecl,decl)
import Debug.Trace (trace)

expr :: ASTBuilder Expr
expr = Expr.expr block
exprList :: ASTBuilder [Expr]
exprList = Expr.exprList block

-- Statement =
-- 	Declaration | LabeledStmt | SimpleStmt |
-- 	ReturnStmt | BreakStmt | ContinueStmt | GotoStmt |
-- 	Block | IfStmt | SwitchStmt |  ForStmt

stmt :: ASTBuilder [Stmt]
stmt =  trace "--eval stmt" (:[]) <$> choice [decl block,returnStmt,labeledStmt,breakStmt,continueStmt,gotoStmt,ifStmt,switchStmt,forStmt,try simpleStmt] <|>  block  

-- Block = "{" StatementList "}" .
block :: ASTBuilder Block 
block = token LeftBrace >> stmtList <* token RightBrace 

-- StatementList = { Statement ";" } .
stmtList :: ASTBuilder [Stmt]
stmtList = concat <$> many stmtL
    where stmtL = stmt <* token SemiColon 

-- EmptyStmt = .
emptyStmt :: ASTBuilder Stmt
emptyStmt = lookAhead $ oneOfTokens [SemiColon,RightBrace] >> return NilStmt

-- ExpressionStmt = Expression .
expressionStmt :: ASTBuilder Stmt
expressionStmt = ExprStmt <$> expr

-- Assignment = ExpressionList assign_op ExpressionList .
assignmentStmt :: ASTBuilder Stmt
assignmentStmt = exprList <* token Assign >>= (<$> exprList).AssignmentStmt

-- SimpleStmt =  ExpressionStmt |  Assignment | ShortVarDecl .
simpleStmt :: ASTBuilder Stmt
simpleStmt = trace "eval simpleStmt" choice $ try <$> [emptyStmt, expressionStmt,assignmentStmt,shortVarDecl block]


-- ReturnStmt = "return" [ ExpressionList ] .
returnStmt :: ASTBuilder Stmt
returnStmt = token Return >> ReturnStmt <$> optionExpr
    where optionExpr = option [] exprList

-- BreakStmt = "break" [ Label ] .
breakStmt :: ASTBuilder Stmt
breakStmt = token Break >> BreakStmt <$> optionalLabel

-- ContinueStmt = "continue" [ Label ] .
continueStmt :: ASTBuilder Stmt
continueStmt = token Continue >> ContinueStmt <$> optionalLabel

-- GotoStmt = "goto" Label .
gotoStmt :: ASTBuilder Stmt
gotoStmt = token Goto >> GotoStmt <$> getLabel
        where name (Identifier n) = return n
              getLabel = identifierToken >>= name

optionalLabel :: ASTBuilder String
optionalLabel = option OptionalToken identifierToken >>= getIden
         where getIden x = return $ case x of
                (Identifier name) -> name
                _ -> emptyString




-- LabeledStmt = Label ":" Statement .
labeledStmt :: ASTBuilder Stmt 
labeledStmt = label <* token Colon >>= (<$> stmt) . LabelStmt . name 
    where name (Identifier n) = n
-- Label       = identifier .
label :: ASTBuilder Token
label = identifierToken

-- IfStmt = "if" [ SimpleStmt ";" ] Expression Block [ "else" ( IfStmt | Block ) ] .
ifStmt :: ASTBuilder Stmt
ifStmt = token If >> do
    oss <- optionalSimpleStmt
    compExpr <- expr
    bl <- block
    oElse <- optionalElse <|> return []
    return $ IfStmt oss compExpr bl oElse

    where optionalElse = token Else >> ( ((:[])  <$> ifStmt) <|> block)

optionalSimpleStmt :: ASTBuilder Stmt
optionalSimpleStmt = trace "eval optionalSimpleStmt" (try (simpleStmt <*  trace  "eval semicolon" token SemiColon >> unexpected "done reading semicolon")) <|> trace "didn't found simpleStmt" return NilStmt

-- SwitchStmt = "switch" [ SimpleStmt ";" ] [ Expression ] "{" { ExprCaseClause } "}" .
switchStmt :: ASTBuilder Stmt
switchStmt = let
    -- ExprSwitchCase = "case" ExpressionList | "default" .
    exprSwitchCase = (token Case >> exprList) <|> (token Default >> return [] )
    -- ExprCaseClause = ExprSwitchCase ":" StatementList .
    exprCaseClause = do
        cas <- exprSwitchCase <* token Colon
        stmts <- stmtList
        return (cas,stmts)
    in token Switch >> do
        oss <- optionalSimpleStmt
        oexpr <- try expr <|> return NilExpr
        token LeftBrace >> (SwitchStmt oss oexpr <$> many exprCaseClause) <* token RightBrace


-- ForStmt = "for" [ Condition | ForClause | RangeClause ] Block .

forStmt :: ASTBuilder Stmt
forStmt = let

    -- Condition = Expression Block.
    whileLoop = (expr >>= (<$> block) . WhileStmt)

    -- ForClause = [ InitStmt ] ";" [ Condition ] ";" [ PostStmt ] Block.
    -- InitStmt = SimpleStmt .
    -- PostStmt = SimpleStmt .
    forLoop = do
        s1 <- ( simpleStmt <|> return NilStmt) <* token SemiColon
        cond <- ( expr <|> return NilExpr) <* token SemiColon
        (simpleStmt <|> return NilStmt) >>= (<$> block).ForStmt s1 cond

    -- RangeClause = [ ExpressionList "=" | IdentifierList ":=" ] "range" Expression Block .
    forRangeLoop = (exprList >>= (<$> oneOfTokens [Assign,NewAssign]) . ForRangeStmt) <* token Range >>= (<$> expr) >>= (<$> block)   
    
    in token For >> choice (try <$> [whileLoop,forLoop,forRangeLoop])

