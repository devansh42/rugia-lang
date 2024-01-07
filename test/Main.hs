module Main where

import Text.Parsec (parse,ParseError)
import SourceFile ( sourceFile )
import Tokenizor ( tokenParser,Token, showTokens, showTokenPos, TokenPos )

import AST ( AST, ASTBuilder,Expr )
import Statement (block, expr)

filePath :: String
filePath = "test/test.go"

content :: IO String
content = readFile filePath


buildAST :: IO String -> IO ()
buildAST sourceCode  = do
    tokens <- parse tokenParser "" <$> sourceCode
    case tokens of
        Left err -> print err
        Right ts ->  processTokens ts

processTokens :: [TokenPos] -> IO ()
processTokens tokens = printResult $ parse expr "" tokens


printResult :: Either ParseError Expr ->  IO ()
printResult x = case x of
    Left err -> print err
    Right tree -> print tree


main :: IO()
main = mapM_ parseExpr testExprs

parseExpr :: String -> IO ()
parseExpr e = case getTokens e of
        Left err -> print err
        Right ts -> processTokens ts

getTokens = parse tokenParser ""

testExprs :: [String]
testExprs = [
    -- "1",
    -- "2+3",
    -- "Point3D{}"]
    -- "Line{e}"]
    "Line{origin, Point3D{y: 4, z: 12.3}}"]


