
lib/Expression.hs:2:1-332: warning: [-Wunused-imports]
    The import of ‘NilStmt, Stmt, StructType’
    from module ‘AST’ is redundant
  |
2 | import AST (ASTBuilder, identifierToken,oneOfTokens,token,Expr (NilExpr,PrimaryExpr,KeyedExpr,Slice,Index, Selector, VaridicArg,CallArgs, MethodExpr, ConversionExpr, BasicLit, OperandName, CompositeExpr, BinaryExpr, FunctionLit), TypeInfo (StructType), intToken, floatToken, strToken, runeToken, BodyParser, getIden, Stmt (NilStmt))
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

lib/Expression.hs:3:1-335: warning: [-Wunused-imports]
    The import of ‘FloatLit, IntLit, RuneLit, StringLit’
    from module ‘Tokenizor’ is redundant
  |
3 | import Tokenizor (Token (FullStop, LeftSupScript,Comma, Colon, OptionalToken, RightSupScript, Identifier, LeftParen, RightParen, RightBrace, LeftBrace, IntLit, FloatLit, StringLit, RuneLit, Struct, Map, Plus, Minus, Not, Mul, Divide, Mod, LeftShift, RightShift, BitwiseOR, BitwiseXOR, Eq, NotEq, LTEQ, GTEQ, LTH, GTH, CondAND, CondOR))
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

lib/Expression.hs:4:62-66: warning: [-Wunused-imports]
    The import of ‘<?>’ from module ‘Text.Parsec’ is redundant
  |
4 | import Text.Parsec (option,try,many, lookAhead, count,choice,(<?>),(<|>), unexpected)
  |                                                              ^^^^^

lib/Expression.hs:7:1-27: warning: [-Wunused-imports]
    The import of ‘Control.Monad’ is redundant
      except perhaps to import instances from ‘Control.Monad’
    To import instances alone, use: import Control.Monad()
  |
7 | import Control.Monad (when)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^
evaluating expr
eval orOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating unaryExpr
entering primaryExpr
evaluating operand
evaluting literal
evaluating basicLit
eval token: Identifier "Line"
eval token: Identifier "Line"
eval token: Identifier "Line"
eval token: Identifier "Line"
evaluting functionLit
eval token: Identifier "Line"
evaluting compositeLit
going for this
evaluating literalType
eval token: Identifier "Line"
eval token: Identifier "Line"
eval singleTypeDecl
eval token: Identifier "Line"
evaluating literalVal
eval token: LeftBrace
evaluating elementList
evaluting keyedElement
evaluating key
evaluating element
eval token: Identifier "origin"
evaluating expr
eval orOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating unaryExpr
evaluting literal
eval token: Identifier "origin"
eval token: Identifier "origin"
eval token: Identifier "origin"
eval token: Identifier "origin"
eval token: Identifier "origin"
evaluting compositeLit
eval token: Identifier "origin"
eval token: Identifier "origin"
eval token: Identifier "origin"
eval token: Comma
evaluating operandName
eval token: Identifier "origin"
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
about to consume keyedElement Colon
eval token: Comma
eval token: Comma
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: Identifier "Point3D"
eval token: LeftBrace
evaluating elementList
evaluting keyedElement
eval token: Identifier "y"
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating unaryExpr
evaluting literal
eval token: Identifier "y"
eval token: Identifier "y"
eval token: Identifier "y"
eval token: Identifier "y"
eval token: Identifier "y"
evaluting compositeLit
eval token: Identifier "y"
eval token: Identifier "y"
eval token: Identifier "y"
eval token: Colon
eval token: Identifier "y"
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: IntLit 4
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating binaryOperator
evaluating unaryExpr
evaluting literal
eval token: IntLit 4
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Comma
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Identifier "z"
eval token: Colon
eval token: Identifier "z"
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: Colon
eval token: FloatLit 12.3
eval token: FloatLit 12.3
eval token: FloatLit 12.3
eval token: RightBrace
eval token: RightBrace
eval token: RightBrace
eval token: RightBrace
eval token: RightBrace
eval token: RightBrace
eval token: RightBrace
eval token: Identifier "Point3D"
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: Identifier "Line"
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
eval token: LeftBrace
