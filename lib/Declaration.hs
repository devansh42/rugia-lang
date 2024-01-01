module Declaration (topLevelDecl,shortVarDecl,decl) where


import Tokenizor
    ( Token(RightParen, SemiColon,Assign, Type, OptionalToken, LeftParen, Identifier, Const, RightBrace, Var, NewAssign, Func) )
import AST ( ASTBuilder, token,Block, Declaration (ConstDecl,VarDecl,FuncDecl, MethodDecl), TypeInfo (InvalidTypeInfo), identifierToken, BodyParser, Stmt (DeclStmt) )

import Types ( typeSpec, identifierList, singleTypeDecl,  funcSignature, funcParams )
import Expression (exprList)
import Text.Parsec (option,many,(<|>),try, parse, choice,optional)
import GHC.Generics (DecidedStrictness(DecidedLazy))


-- TopLevelDecl   = ConstDecl | TypeDecl | FunctionDecl | MethodDecl   .
topLevelDecl :: BodyParser -> ASTBuilder [Declaration]
topLevelDecl funcBody= choice [constDecl funcBody,typeDecl ] <|>  (:[]) <$> choice  [try $ methodDecl funcBody ,funcDecl funcBody ]

-- Declaration   = ConstDecl | TypeDecl | VarDecl .
decl :: BodyParser -> ASTBuilder Stmt 
decl funcBody = DeclStmt  <$> ( constDecl funcBody <|> typeDecl <|> varDecl funcBody)

-- TypeDecl = "type" ( TypeSpec | "(" { TypeSpec ";" } ")" ) .
typeDecl :: ASTBuilder [Declaration]
typeDecl = let
    typeSpecWithSemiColon = typeSpec <* token SemiColon
    in token Type >>
    ((token LeftParen >> many typeSpecWithSemiColon <* token RightParen) <|> (:[]) <$> typeSpec)

-- ConstDecl      = "const" ( ConstSpec | "(" { ConstSpec ";" } ")" ) .
-- ConstSpec      = IdentifierList [ Type ] "=" ExpressionList  .
-- IdentifierList = identifier { "," identifier } .
-- ExpressionList = Expression { "," Expression } .
constDecl :: BodyParser -> ASTBuilder [Declaration]
constDecl funcBody = let
    constSpec = do
        idens <-  identifierList
        typ <-(singleTypeDecl <|> return InvalidTypeInfo ) <* token Assign
        eL <- exprList funcBody
        return $ mapper typ <$> zip idens eL
        where mapper typ (Identifier x, y)  = ConstDecl  x typ y

    in multipleDecl Const constSpec

multipleDecl :: Token -> ASTBuilder [Declaration] -> ASTBuilder [Declaration]
multipleDecl t parser = token t >>
     (( token LeftParen >> concat <$> many (parser <* token SemiColon) <* token RightParen  ) <|> parser)

-- VarDecl     = "var" ( VarSpec | "(" { VarSpec ";" } ")" ) .
-- VarSpec     = IdentifierList ( Type [ "=" ExpressionList ] | "=" ExpressionList ) .
varDecl :: BodyParser -> ASTBuilder [Declaration]
varDecl funcBody = let
    varSpec = do
        idens <- identifierList
        typ <- singleTypeDecl <|> return InvalidTypeInfo
        es <- exprs <|> return []
        return $ mapper typ <$> zip idens es
        where exprs = token Assign >> exprList funcBody
              mapper t (Identifier x, y) = VarDecl x t y
    in multipleDecl Var varSpec


-- ShortVarDecl = IdentifierList ":=" ExpressionList .
shortVarDecl :: BodyParser -> ASTBuilder Stmt
shortVarDecl funcBody = do
    idens <- identifierList <* token NewAssign
    exprs <- exprList funcBody
    return $ DeclStmt $ mapper InvalidTypeInfo <$> zip idens exprs
    where mapper typ (Identifier x, y)  =  VarDecl x typ y


-- FunctionDecl = "func" FunctionName Signature [ FunctionBody ] .
-- FunctionName = identifier .
-- FunctionBody = Block .
funcDecl :: BodyParser -> ASTBuilder Declaration
funcDecl funcBody= token Func >>
 (identifierToken >>= (<$> funcSignature) . FuncDecl . name  >>= (<$> optionalBlock funcBody))

optionalBlock:: BodyParser ->  ASTBuilder Block
optionalBlock funcBody = funcBody <|> return []

name :: Token -> String
name (Identifier n) = n

-- MethodDecl = "func" Receiver MethodName Signature [ FunctionBody ] .
-- Receiver   = Parameters .
methodDecl :: BodyParser -> ASTBuilder Declaration
methodDecl funcBody = token Func >> do
    params <- funcParams
    identifierToken >>= (<$> funcSignature) . MethodDecl (head params) . name  >>= (<$> optionalBlock funcBody)