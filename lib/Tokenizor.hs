module Tokenizor(parser,Token) where

import Text.Parsec.Char (char, oneOf,endOfLine,anyChar,string,digit,spaces,space) 
import Text.Parsec (parsecMap,manyTill,try,choice,eof,many1,(<|>))
import Text.Parsec.String (Parser)

data Token = Plus |  Minus
 | Mul | Divide | CommentHash | Assign | Eq
 | Not | NotEq | LTH | GTH | LTEQ | GTEQ | Mod | SemiColon | LeftParen | RightParen | WhiteSpace
 | LeftShift | RightShift | Comment  
 | NumInt Integer | NumFloat Double deriving (Show)



parseUnitTokens :: Parser  Token 
parseUnitTokens = do
    c <- oneOf "+-*%;/()"  
    return $ case c of
        '+' -> Plus
        '-' -> Minus
        '*' -> Mul
        '%' -> Mod
        ';' -> SemiColon
        '/' -> Divide
        '(' -> LeftParen
        ')' -> RightParen
    


parseBinaryTokens :: Parser Token
parseBinaryTokens = do
    result <- choice ar 
    return $ case result of
        "<<" -> LeftShift
        "<=" -> LTEQ
        "<" -> LTH
        ">>" -> RightShift
        ">=" -> GTEQ
        ">" -> GTH
        "==" -> Eq
        "=" -> Assign
        "!=" -> NotEq
        "!" -> Not 
    where ar = fmap (try.string)  ["<<","<=","<",">>",">=",">","==","=","!=","!"] 
    -- we are using try because string combinator does backtrace in case of failure


parseFloat :: Parser Token
parseFloat = do
    number <- many1 digit 
    char '.'
    floatingPart <- many1 digit
    return $ NumFloat (read (number ++ "." ++ floatingPart) :: Double)
    
parseInteger :: Parser Token    
parseInteger = do
    number <- many1 digit
    return $ NumInt ( read number :: Integer)

parseNumber :: Parser Token 
parseNumber = try parseFloat <|> parseInteger 


parseComment :: Parser Token 
parseComment = do
    char '#' 
    manyTill anyChar $ try endOfLine 
    return Comment 

parseWhiteSpace :: Parser Token
parseWhiteSpace = do
    space
    spaces
    return WhiteSpace

parser :: Parser [Token] 
parser = let 
    parsers = choice [
        parseWhiteSpace,
        parseUnitTokens,
        parseComment,
        parseBinaryTokens,
        parseNumber
        ] 
    in manyTill parsers eof