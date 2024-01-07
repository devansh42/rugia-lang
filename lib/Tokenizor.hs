-- Tokenizor contains primitives for tokenizing code
{-# LANGUAGE InstanceSigs #-}
module Tokenizor(Token(..),TokenPos(..),showTokens,showTokenPos,tokenParser) where


import Text.Parsec.Char (char, oneOf,endOfLine,anyChar,string,digit,spaces,space,letter,octDigit, hexDigit, newline)
import Text.Parsec (manyTill,try,getPosition,choice,many1,(<|>), many,option,eof,count, unexpected, skipMany1, SourcePos, incSourceColumn, sourceLine, sourceColumn  )
import Text.Parsec.String (Parser)
import Data.Int (Int32)
import Numeric (readHex,readOct,readBin, readFloat)
import Control.Monad (void)
import Data.Char (chr)


data TokenPos = TokenPos Token SourcePos SourcePos

instance Show TokenPos where
    show :: TokenPos -> String
    show (TokenPos tk pos _) = "(" ++ show tk ++ " L:" ++ show (sourceLine pos) ++ " C:"++show (sourceColumn pos) ++")"

data Token = OptionalToken |  Plus  |  Minus
 | Mul | Divide | CommentHash | Assign | Eq
 | Not | NotEq | LTH | GTH | LTEQ | GTEQ | Mod | SemiColon |EOL| LeftParen | RightParen
 | LeftBrace | RightBrace
 | LeftSupScript | RightSupScript
 | WhiteSpace
 | Comma | Colon | FullStop
 | NewAssign
 | BitwiseAND | BitwiseOR
 | BitwiseXOR
 | CondAND | CondOR
 | Break | Case | Const | Continue | Default | Else | For | Func | Goto | If
 | Import | Map | Package | Range | Return | Struct | Switch | Type | Var
 | LeftShift | RightShift | Comment | Identifier String
 | RuneLit Char
 | StringLit String
 | IntLit Integer | FloatLit Double deriving (Show,Eq)



showTokens :: [Token] -> String
showTokens (SemiColon: xs) = show SemiColon ++ ['\n'] ++ showTokens xs
showTokens (EOL:tokens) = show EOL ++ ['\n'] ++ showTokens tokens

showTokens (t:tokens) = show t ++ ", " ++ showTokens tokens
showTokens [] = ""

showTokenPos :: [TokenPos] -> String
showTokenPos [] = ""
showTokenPos (TokenPos tk p1 ep1: TokenPos tk2 p2 ep2 : tail_  ) = show (TokenPos tk p1 ep1)
    ++ (if sourceLine p1 == sourceLine p2
        then ""
        else "\n") ++ showTokenPos (TokenPos tk2 p2 ep2 :tail_)
showTokenPos (h:tail_) = show h ++ showTokenPos tail_


-- operator parses an operator
operator :: Parser Token
operator = do
    result <- choice ar
    return $ case result of
        "." -> FullStop
        "," -> Comma
        ":=" -> NewAssign
        ":" -> Colon
        "+" -> Plus
        "-" -> Minus
        "*" -> Mul
        "%" -> Mod
        ";" -> SemiColon
        "/" -> Divide
        "(" -> LeftParen
        ")" -> RightParen
        "{" -> LeftBrace
        "}" -> RightBrace
        "[" -> LeftSupScript
        "]" -> RightSupScript
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
        "&&" -> CondAND
        "&" -> BitwiseAND
        "||" -> CondOR
        "|" -> BitwiseOR
        "^" -> BitwiseXOR
    where ar = fmap (try.string)  [".",",",":=",":","+","-","*","%",";","/","(",")","{","}","[","]", "<<","<=","<",">>",">=",">","==","=","!=","!","&&","&","||","|","^"]
    -- we are using try because string combinator does backtrace in case of failure



binaryDigit_ :: Parser Char
binaryDigit_ = oneOf "01"


letter_ :: Parser Char
letter_ = letter <|> char '_'

-- identifierOrkeyword parses an identifier or keyword
identifierOrkeyword :: Parser Token
identifierOrkeyword = do
    prefix <- letter_
    sufix <- many suffixParser
    return $ let
        name = (prefix:sufix)
        in case name of
            "break" -> Break
            "case" -> Case
            "const" -> Const
            "continue" -> Continue
            "default" -> Default
            "else" -> Else
            "for" -> For
            "func" -> Func
            "goto" -> Goto
            "if" -> If
            "import" -> Import
            "map" -> Map
            "package" -> Package
            "range" -> Range
            "return" -> Return
            "struct" -> Struct
            "switch" -> Switch
            "type" -> Type
            "var" -> Var
            _ ->  Identifier name
    where suffixParser = letter_ <|> digit


integer :: Parser Token
integer = let


    bin_lit_ = lit_ binaryDigit_ readBin
    hex_lit_ = lit_ hexDigit readHex
    oct_lit_ = lit_ octDigit readOct
    lit_ prs red = IntLit . retrive . red <$> many1 prs

    num = do
        d <- digit
        if d == '0' then do
            -- following literal can be octal, binary or hexadecimal
            -- lets try to look for octal digits without prefix 'o' or 'O'
            oct_digits <- many octDigit
            if not (null oct_digits) then  return $ IntLit $ retrive $ readOct oct_digits
            else
                -- check for hexa, binary and octal literal
                -- we are using option parser as there might be nothing to parse ahead
                -- and 0 is the value
                (oneOf "bBoOxX" <|> return '0') >>=non_decimal_literal
        else do
            suf <-  many digit
            return $ IntLit $ read (d:suf)

    non_decimal_literal l = case l of

        'b' -> bin_lit_
        'B' -> bin_lit_
        'x' -> hex_lit_
        'X' -> hex_lit_
        'o' -> oct_lit_
        'O' -> oct_lit_
        _ -> return $ IntLit 0

    retrive [(x,_)] = x

    in num

-- parser for floating point number
-- while parsing numbers we should give this method priority over 
-- integer and operator parser 
-- we are considering operator parser because a 
-- literal starting from "." can be a full stop also
-- and we are checking that possibility in float parser as well
float :: Parser Token
float = let

    retrive [(x,_)] = x

    decimal_exponent_ = do
        x <- oneOf "eE"
        y <-option '+' $ oneOf "+-"
        z<-many1 digit
        return (x:y:z)

    no_intregal_ = do
        digits <- many digit
        if not (null digits) then do
            expo <- option [] decimal_exponent_

            return $  FloatLit $ retrive $ readFloat ("0." ++ digits ++ expo)
        else
            return $ FullStop
    fractional_ = do
        frac_digits <- many digit
        expo <- option [] decimal_exponent_
        return $ frac_digits ++ expo

    with_intregal_ = do
        digits <- many1 digit
        x <- option '0' $ char '.'
        case x of
                '.' -> do
                        y <-  fractional_
                        return $ FloatLit $ retrive $ readFloat $ digits ++ "." ++ if not (null y) then y else "0"
                _ -> do
                    y <- option [] decimal_exponent_
                    case y of
                        [] -> unexpected "need to be parsed as integer"
                        _ -> return $ FloatLit $ retrive $ readFloat $ digits ++ y


    float_lit_ = do
        x <- option '0' $ char '.'
        case x of
            -- literal without any intregal part, e.g. .256, .12345E+5
            '.' -> no_intregal_
            _ -> try with_intregal_ -- we have applied "try" as there is a possibility of having an integer 

    in float_lit_


runeLit_ :: Parser Char
runeLit_ = let

    retrive [(x,_)] = chr x

    rune_lit__ :: Parser Char
    rune_lit__ = do
        _ <- char '\\'
        y <- option '0' $ oneOf "abfnrtvuUx\\'\""
        case escaped_char y of
            0 ->
                case y of
                    'U' -> big_u_val
                    'u' -> little_u_val
                    'x' -> hex_byte_val
                    _ -> oct_byte_val
            _ -> return y

    escaped_char :: Char -> Int32
    escaped_char n =
            case n of
                'a' -> 0x7
                'b' -> 0x8
                'f' -> 0xc
                'n' -> 0xa
                'r' -> 0xd
                't' -> 0x9
                'v' -> 0xb
                '\\' -> 0x5c
                '\'' -> 0x27
                '\"' -> 0x22
                _ -> 0


    big_u_val = do
        d <- count 8 hexDigit
        return $ retrive $ readHex d

    little_u_val = do
        d <- count 4 hexDigit
        return $ retrive $ readHex d

    hex_byte_val = do
        d <- count 2 hexDigit
        return $ retrive $ readHex d
    oct_byte_val = do
        d <- count 3 octDigit
        return $ retrive $ readOct d

    unicode_char :: Parser Char
    unicode_char = anyChar
    in rune_lit__ <|> unicode_char

rune :: Parser Token
rune = RuneLit <$> ( char '\'' >> runeLit_ <* char '\'' )

str :: Parser Token
str = let
    raw_str = StringLit  <$>  manyTill anyChar ( char '`')
    interpreted_str = StringLit  <$> manyTill runeLit_ ( char '"')
    in ( char '`' >> raw_str) <|> (char '\"' >> interpreted_str)


-- comment parses comments
-- comments started with // would end at the end of line
-- comments started with /* would end at */ 
comment :: Parser Token
comment = char '/' >>
    ((char '/' >> manyTill anyChar endOfComment) <|> (char '*' >> manyTill anyChar (try $ string "*/"))) >> return Comment
    where endOfComment = (el <|> eof) >> return '\n'
          el = void endOfLine

-- whiteSpace parses white space
whiteSpace :: Parser Token
whiteSpace = skipMany1 space >> return WhiteSpace


eol :: Parser Token
eol = skipMany1 endOfLine >> return EOL

tokenParser :: Parser [TokenPos]
tokenParser = let

    wrapperParser :: Parser Token -> Parser TokenPos
    wrapperParser parser = do
        startPos<-getPosition
        res <- parser
        TokenPos res startPos <$> getPosition


    -- Note: Order of these parsers really matters
    parser_combinator = wrapperParser $ choice [
        eol,
        whiteSpace,
        comment,
        rune,
        str,
        float,
        integer,
        identifierOrkeyword,
        operator]

    getTk (TokenPos tk _ _) = tk
    -- addSemicolons adds semicolons as per Golang Spec
    addSemicolons [] = []
    addSemicolons [TokenPos WhiteSpace _ _] = []
    addSemicolons [TokenPos Comment _ _] = []
    addSemicolons [x] = [x]
    addSemicolons (TokenPos WhiteSpace _ _:tail_) = addSemicolons tail_
    addSemicolons (TokenPos Comment _ _:tail_) = addSemicolons tail_
    addSemicolons (TokenPos EOL _ _:x:tail_) = addSemicolons (x:tail_)
    addSemicolons (TokenPos SemiColon _ _ :TokenPos SemiColon x y:tail_) = addSemicolons $ TokenPos SemiColon x y: tail_ -- merging semicolons incase user has already put an semicolon
    addSemicolons (x:TokenPos EOL sp ep  :tail_) = case getTk x of
            Identifier _  -> fn
            IntLit _ -> fn
            RuneLit _ -> fn
            FloatLit _ -> fn
            StringLit _ -> fn
            Break  -> fn
            Continue -> fn
            Return -> fn
            RightBrace -> fn
            RightSupScript -> fn
            RightParen -> fn
            _ -> x : addSemicolons tail_
        where
        fn = x : TokenPos SemiColon sp ep : addSemicolons tail_
    addSemicolons (x:tail_) = x : addSemicolons tail_

    addLastSemiColon [] = []
    addLastSemiColon [TokenPos SemiColon sp ep] = [TokenPos SemiColon sp ep]
    addLastSemiColon [TokenPos tk sp ep] = TokenPos tk sp ep : [ TokenPos SemiColon ep $ incSourceColumn ep 1 ]
    addLastSemiColon (x:tail_) = x: addLastSemiColon tail_

    in do
        tokens <- manyTill parser_combinator eof
        return $  (addLastSemiColon . addSemicolons) tokens
