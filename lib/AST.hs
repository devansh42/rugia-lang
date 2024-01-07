{-# LANGUAGE InstanceSigs #-}
module AST(AST(..),Expr(..),getIden,Block,listParser,emptyString,TypeInfo(..),Stmt(..),Declaration(..),ASTBuilder,
identifierToken,
runeToken,
strToken,
intToken,
floatToken,
oneOfTokens,
drainTokens,
BodyParser,
token) where
import Tokenizor (Token (Identifier,RuneLit,StringLit,IntLit,FloatLit, Struct),TokenPos(TokenPos), tokenParser)
import Text.Parsec (Parsec,tokenPrim, incSourceColumn,SourcePos,(<?>), many)
import Data.Int (Int32)
import Debug.Trace (trace)

data TypeInfo = InvalidTypeInfo
 |  TypeInfo String 
 | SliceType TypeInfo 
 | StructType [Declaration] 
 | MapType TypeInfo TypeInfo 
 | FuncType [Declaration] [Declaration] deriving (Show)

data Declaration = AliasDecl String TypeInfo -- type x = y
 | TypeDecl String TypeInfo -- type list []int
 | VarDecl String TypeInfo Expr -- var x int, func(a int) (b int)
 | ConstDecl String TypeInfo Expr
 | FuncDecl String TypeInfo Block
 | MethodDecl Declaration String TypeInfo Block    
 | AnonymousDecl TypeInfo deriving (Show) -- for functions like func(int,int) int 

data AST = SourceFile [Declaration] deriving Show

data Expr = NilExpr | Selector String  -- obj.color 
    | Slice Expr Expr -- ar[1+2:4+5]
    | Index Expr -- ar[1+2]
    | CallArgs [Expr] -- someFunc(1,2+3,4/2)
    | VaridicArg Expr -- someFunc(getThings()...)
    | MethodExpr TypeInfo String -- T.m here T is the actual type and not an instance of type
    | ConversionExpr TypeInfo Expr -- int(34.6)
    | BasicLit Token -- 3 or 2.3 or "Somthing" or 'a'
    | FunctionLit TypeInfo Block
    | OperandName String -- package.something
    | CompositeExpr TypeInfo [Expr] -- []int{1,2+8,3-3}
    | KeyedExpr [Expr] [Expr] -- {1:2}, {1,2} (left expr would be nil), {a:1}
    | PrimaryExpr Expr Expr -- f(3.1415, true), leftExpr = f, rightExpr = (3.1415, true)
    | BinaryExpr Expr Token Expr deriving (Show)


emptyString :: String
emptyString = ""

type BodyParser = ASTBuilder Block 

type Block = [Stmt]

data Stmt = NilStmt 
    | ExprStmt Expr 
    | AssignmentStmt [Expr] [Expr] 
    | ReturnStmt [Expr]
    | LabelStmt String Block 
    | BreakStmt String
    | ContinueStmt String
    | GotoStmt String
    | IfStmt Stmt Expr Block [Stmt] -- IfStmt SimpleStmt ComparisonExpr ConditionBlock ElseBlock or IfElseBlock 
    | SwitchStmt Stmt Expr [([Expr],[Stmt])] -- SwitchStmt SimpleStmt ComparisonExpr [(CaseClause:CaseStatment)] , empty CaseClause means default 
    | WhileStmt Expr Block -- WhileStmt ConditionExpression Block
    | ForRangeStmt [Expr] Token Expr Block -- ForRangeStmt AssigneesExpr AssignmentOperator(:= | =) IterableExpr Block
    | ForStmt Stmt Expr Stmt Block   -- ForStmt InitExpr ConditionExpr PostExpr Block
    | DeclStmt [Declaration]  deriving Show 

-- instance Show Stmt where
--     show :: Stmt -> String
--     show = showDepth 0  




type ASTBuilder = Parsec [TokenPos] ()


_updatePosToken :: SourcePos -> TokenPos -> [TokenPos] -> SourcePos
_updatePosToken _ (TokenPos _ _ ep) [] = ep  
_updatePosToken _ _ ((TokenPos _ sp _):_) = sp  

satisfyToken :: (Token -> Bool) -> ASTBuilder Token
satisfyToken f = tokenPrim (\c -> show [c]) _updatePosToken (_checkToken f)
                            

_checkToken :: (Token -> Bool) -> TokenPos -> Maybe Token
_checkToken f (TokenPos c _ _) = trace ("eval token: "++ show c) $ if f c then Just c else Nothing

oneOfTokens :: [Token] -> ASTBuilder Token
oneOfTokens ts = satisfyToken (elem_ ts)

elem_ :: [Token] -> Token -> Bool
elem_ tail_ t
  = foldr (\ head_ -> (||) (sameToken_ head_ t)) False tail_

token :: Token -> ASTBuilder Token
token t =  satisfyToken (sameToken_ t) <?> show [t]


identifierToken :: ASTBuilder Token
identifierToken = do
    token iden
    where iden = Identifier "_"

runeToken :: ASTBuilder Token
runeToken = do
    token run
    where run = RuneLit '0'

strToken :: ASTBuilder Token
strToken = do
    token s
    where s = StringLit []

intToken :: ASTBuilder Token
intToken = do
    token int
    where int = IntLit 0

floatToken :: ASTBuilder Token
floatToken = do
    token f
    where f = FloatLit 0

sameToken_ :: Token -> Token -> Bool
sameToken_ (Identifier _) (Identifier _)  = True
sameToken_ (RuneLit _) (RuneLit _) = True
sameToken_ (StringLit _) (StringLit _) = True
sameToken_ (IntLit _) ( IntLit _) = True
sameToken_ (FloatLit _) (FloatLit _) = True
sameToken_ x y = x == y

drainTokens :: [Token] -> ASTBuilder ()
drainTokens   = mapM_ token 


-- listParser parsers things like -->  s, { "sep" s }
listParser :: ASTBuilder a -> Token -> ASTBuilder [a]
listParser parser seperator = do
    r1 <- parser
    r2s <- many optionList
    return (r1:r2s)
    where optionList = token seperator >> parser 
    

getIden :: Token -> String
getIden (Identifier n) = n
    