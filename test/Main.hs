module Main where

import Text.Parsec (parse)
import Test.HUnit
import Tokenizor

-- Helper function to parse tokens
parseTokens :: String -> [Token]
parseTokens input = case parse tokenParser "" input of
    Left err -> error (show err)
    Right tokens -> tokens

-- Test cases for the token parser
testTokenParser :: Test
testTokenParser = test [
    -- Basic tokens
    "Integer literal" ~: "123" ~: [IntLit 123] ~=? parseTokens "123",
    "Float literal" ~: "3.14" ~: [FloatLit 3.14] ~=? parseTokens "3.14",
    "Identifier" ~: "foo" ~: [Identifier "foo"] ~=? parseTokens "foo",
    "Operator" ~: "+" ~: [Plus] ~=? parseTokens "+",
    "Comment" ~: "// This is a comment" ~: [] ~=? parseTokens "// This is a comment",
    
    -- Keywords
    "Keywords" ~: "if else for func return" ~: [If, Else, For, Func, Return] ~=? parseTokens "if else for func return",
    
    -- Complex expressions
    "Complex expression" ~: "x + 42 * y" ~: [Identifier "x", Plus, IntLit 42, Mul, Identifier "y"] ~=? parseTokens "x + 42 * y",
    
    -- Strings and runes
    "String literal" ~: "\"Hello, World!\"" ~: [StringLit [72,101,108,108,111,44,32,87,111,114,108,100,33]] ~=? parseTokens "\"Hello, World!\"",
    "Rune literal" ~: "'a'" ~: [RuneLit 97] ~=? parseTokens "'a'",
    
    -- Special characters and operators
    "Special characters" ~: "{ ( ) }" ~: [LeftBrace, LeftParen, RightParen, RightBrace] ~=? parseTokens "{ ( ) }",
    "Bitwise operators" ~: "& | ^" ~: [BitwiseAND, BitwiseOR,BitwiseXOR] ~=? parseTokens "& | ^",
    
    -- White space and comments
    "White space" ~: "  x  " ~: [Identifier "x"] ~=? parseTokens "  x  ",
    "Comments" ~: "/* This is a comment */" ~: [] ~=? parseTokens "/* This is a comment */",
    
    -- Edge cases
    "Empty input" ~: "" ~: [] ~=? parseTokens "",
    
    -- Test different literals
    "Binary integer literal" ~: "0b101" ~: [IntLit 5] ~=? parseTokens "0b101",
    "Hexadecimal integer literal" ~: "0x1A" ~: [IntLit 26] ~=? parseTokens "0x1A",
    "Octal integer literal" ~: "0o777" ~: [IntLit 511] ~=? parseTokens "0o777",
    
    -- Test floating-point literals
    "Floating-point literal" ~: "1.23" ~: [FloatLit 1.23] ~=? parseTokens "1.23",
    "Exponential notation" ~: "1.2e3" ~: [FloatLit 1200.0] ~=? parseTokens "1.2e3",
    
    -- Test string literals
    "Interpreted string literal" ~: "\"Hello\"" ~: [StringLit [72, 101, 108, 108, 111]] ~=? parseTokens "\"Hello\"",
    "Raw string literal" ~: "`Hello`" ~: [StringLit [72, 101, 108, 108, 111]] ~=? parseTokens "`Hello`",
    
    -- Test rune literals
    "Escape characters in rune" ~: "'\\n'" ~: [RuneLit 10] ~=? parseTokens "'\\n'",
    "Unicode character in rune" ~: "'\x03A9'" ~: [RuneLit 937] ~=? parseTokens "'\x03A9'",
    
    -- Test for specific operators
    "Left shift operator" ~: "<<" ~: [LeftShift] ~=? parseTokens "<<",
    "Right shift operator" ~: ">>" ~: [RightShift] ~=? parseTokens ">>",
    "Not equal operator" ~: "!=" ~: [NotEq] ~=? parseTokens "!=",
    "Less than or equal operator" ~: "<=" ~: [LTEQ] ~=? parseTokens "<=",
    "Greater than or equal operator" ~: ">=" ~: [GTEQ] ~=? parseTokens ">=",
    "Short variable declaration" ~: ":=" ~: [NewAssign] ~=? parseTokens ":=",
    
    -- Test for white space and comments within expressions
    "Whitespace within expression" ~: "x  +  y" ~: [Identifier "x", Plus, Identifier "y"] ~=? parseTokens "x  +  y",
    "Comment within expression" ~: "x /* comment */ + y" ~: [Identifier "x", Plus, Identifier "y"] ~=? parseTokens "x /* comment */ + y"]

-- Run the test suite
main :: IO Counts
main = runTestTT testTokenParser
