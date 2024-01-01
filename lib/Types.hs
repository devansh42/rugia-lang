module Types where

import Tokenizor (tokenParser, Token (Type,SemiColon,OptionalToken,FullStop,  Assign,LeftBrace,Struct, Comma,  RightBrace, Map, LeftSupScript, RightSupScript, Func, Identifier, LeftParen, RightParen))
import AST (ASTBuilder,listParser,Declaration(..),TypeInfo (StructType, TypeInfo, MapType, SliceType, FuncType, InvalidTypeInfo),token, identifierToken, drainTokens, oneOfTokens, Expr (NilExpr))
import Text.Parsec (option,many,count,parse,unexpected,try,(<|>),(<?>), lookAhead)


-- TypeSpec = AliasDecl | TypeDef 
-- AliasDecl = identifier "=" Type .
-- TypeDef = identifier Type .
typeSpec :: ASTBuilder Declaration
typeSpec = do
    (Identifier iden) <- identifierToken
    x <- option OptionalToken $ token Assign
    case x of
        Assign -> AliasDecl iden <$> singleTypeDecl -- AliasDecl = identifier "=" Type .
        _ -> TypeDecl iden <$> singleTypeDecl -- TypeDef = identifier Type .



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
singleTypeDecl  = do
    name <- option OptionalToken typeName
    case name of
        (Identifier n) -> return $ TypeInfo n -- referencing existing type 
        _ -> (token LeftParen >> singleTypeDecl <* token RightParen) <|> typeLit  

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
-- TODO : Fix this
funcParams :: ASTBuilder [Declaration]

funcParams = let
    varWithType typ (Identifier name) = VarDecl name typ NilExpr

    idenName (Identifier x) = x

    paramDeclWithComma = token Comma >> paramDecl

    -- ParameterList  = ParameterDecl { "," ParameterDecl } .
    paramList = concat <$> listParser paramDecl Comma

    varidicType = option [] $ count 3 (token FullStop)

    -- ParameterDecl  = [ IdentifierList ] [ "..." ] Type .
    paramDecl :: ASTBuilder [Declaration]
    paramDecl = do
        -- x int
        -- int
        -- int,int
        idens <- option [] identifierList
        case idens of
            [] -> do
                vt <- varidicType   -- lets check if this is varidic or not
                case vt of
                    [] -> (:[]) <$> (AnonymousDecl <$> singleTypeDecl) -- anonymous variable declaration e.g. func(int) int
                    _ -> (:[]) . AnonymousDecl <$> (SliceType <$> singleTypeDecl) -- e.g. func(..int) 

            _ -> do
                 v <- varidicType
                 case v of
                    [] -> do
                            typ <- singleTypeDecl
                            return $ varWithType typ <$> idens -- func(x, y, z int)
                    _ ->
                        if length idens > 1 then unexpected "only one varadic variable is allowed" -- e.g. func(x,y... int)
                        else return [] -- ((:[]) . NilExpr) . VarDecl (idenName (head idens)) <$> ( SliceType <$> singleTypeDecl) -- e.g (x ...int) 

    in token LeftParen >> 
    ((token RightParen >> return []) <|> ( paramList <* token RightParen ))

-- Result         = Parameters | Type .
funcResult :: ASTBuilder [Declaration]
funcResult = funcParams <|> (:[]) <$> (AnonymousDecl <$> singleTypeDecl)

-- Signature      = Parameters [ Result ] .
funcSignature :: ASTBuilder TypeInfo
funcSignature = funcParams >>= ( <$> (funcResult <|> return []) ) . FuncType  
  
