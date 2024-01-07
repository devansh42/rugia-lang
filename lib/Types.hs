module Types where

import Tokenizor (tokenParser, Token (Type,SemiColon,OptionalToken,FullStop,  Assign,LeftBrace,Struct, Comma,  RightBrace, Map, LeftSupScript, RightSupScript, Func, Identifier, LeftParen, RightParen))
import AST (ASTBuilder,listParser,Declaration(..),TypeInfo (StructType, TypeInfo, MapType, SliceType, FuncType, InvalidTypeInfo),token, identifierToken, drainTokens, oneOfTokens, Expr (NilExpr), getIden)
import Text.Parsec (option,many,count,parse,unexpected,try,(<|>),(<?>), lookAhead)
import Debug.Trace (trace)

-- TypeSpec = AliasDecl | TypeDef 
-- AliasDecl = identifier "=" Type .
-- TypeDef = identifier Type .
typeSpec :: ASTBuilder Declaration
typeSpec = do
    (Identifier iden) <- identifierToken
    ( AliasDecl iden <$>  (token Assign >> singleTypeDecl))
     <|>
        (TypeDecl iden <$> singleTypeDecl)


-- TypeLit   =  StructType | FunctionType | 
--              SliceType | MapType .
typeLit :: ASTBuilder TypeInfo
typeLit = do
    t <- lookAhead $ oneOfTokens [Struct,Map,Func,LeftSupScript]
    case t of
        Struct -> structType
        Map -> mapType
        Func -> funcType
        LeftSupScript -> sliceType
        _ -> return InvalidTypeInfo

-- StructType    = "struct" "{" { FieldDecl ";" } "}" .
structType :: ASTBuilder TypeInfo
structType = drainTokens [Struct,LeftBrace] >> StructType . concat  <$>
    (many fieldsBeforeSemicolon <* token RightBrace)
    where fieldsBeforeSemicolon = fieldDecl <* token SemiColon


-- TODO: Add QualifiedIdent as well
typeName :: ASTBuilder Token
typeName =  identifierToken

-- Type      = TypeName | TypeLit | "(" Type ")" .
singleTypeDecl :: ASTBuilder TypeInfo
singleTypeDecl = trace "eval singleTypeDecl"  (TypeInfo . getIden <$> typeName)  <|> (token LeftParen >> singleTypeDecl <* token RightParen) <|> typeLit


-- FieldDecl     = IdentifierList Type . 
fieldDecl :: ASTBuilder [Declaration]
fieldDecl = do
    fieldList <- identifierList
    fieldsType <- singleTypeDecl
    return  $ (valFn fieldsType) <$> fieldList
    where valFn typ (Identifier name) = VarDecl name typ NilExpr


-- IdentifierList = identifier { "," identifier } .
identifierList :: ASTBuilder [Token]
identifierList = listParser identifierToken Comma

-- KeyType     = Type .
keyType :: ASTBuilder TypeInfo
keyType  = singleTypeDecl

-- ElementType = Type .
elementType :: ASTBuilder TypeInfo
elementType = singleTypeDecl

-- MapType     = "map" "[" KeyType "]" ElementType .
mapType :: ASTBuilder TypeInfo
mapType = drainTokens [Map,LeftSupScript] >>( keyType <* token RightSupScript >>= (<$> elementType) . MapType)

-- SliceType = "[" "]" ElementType .
sliceType :: ASTBuilder TypeInfo
sliceType = SliceType <$> (drainTokens [LeftSupScript,RightSupScript] >> elementType )

-- FunctionType   = "func" Signature .
funcType :: ASTBuilder TypeInfo
funcType = token Func >> funcSignature

-- Parameters     = "(" [ ParameterList ] ")" .
funcParams :: ASTBuilder [Declaration]

funcParams = let
    varWithType typ (Identifier name) = VarDecl name typ NilExpr

    -- ParameterList  = ParameterDecl { "," ParameterDecl } .
    paramList = concat <$> listParser paramDecl Comma

    -- map[string]int
    namesWithType [] typ False = return [AnonymousDecl typ] 

    -- ...map[string]int 
    namesWithType [] typ True = return [AnonymousDecl (SliceType typ)]

    -- x ...int
    namesWithType [name] typ True = return [varWithType (SliceType typ) name]
    
    -- int,int,int
    namesWithType names InvalidTypeInfo False = return $ AnonymousDecl . TypeInfo. getIden <$> names 

    -- x int or x, y, z int 
    namesWithType names typ False =  return  $ varWithType typ <$> names
    
    -- x, y ...int (illegal)
    namesWithType names _ True = unexpected $ "only one varidic arguments is allowed, found: " ++ show (length names)


    
    -- ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
    paramDecl :: ASTBuilder [Declaration]
    paramDecl = do
        names <- option [] identifierList
        vari <- option [] (count 3 $ token FullStop) 
        typ <- option InvalidTypeInfo singleTypeDecl
        namesWithType names typ $ not (null vari) 
    
    in token LeftParen >>
    ((token RightParen >> return []) <|> ( paramList <* token RightParen ))

-- Result         = Parameters | Type .
funcResult :: ASTBuilder [Declaration]
funcResult = funcParams <|> (:[]) <$> (AnonymousDecl <$> singleTypeDecl)

-- Signature      = Parameters [ Result ] .
funcSignature :: ASTBuilder TypeInfo
funcSignature = funcParams >>= ( <$> (funcResult <|> return []) ) . FuncType

