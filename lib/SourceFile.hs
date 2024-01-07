module SourceFile where


import Declaration ( topLevelDecl )
import AST (ASTBuilder,token, AST (SourceFile))
import Text.Parsec (many)
import Statement (block)
import Tokenizor (Token(SemiColon))
-- SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
-- TODO: Add package and import support as well
sourceFile :: ASTBuilder AST 
sourceFile = SourceFile . concat <$>  many ( topLevelDecl block <* token SemiColon)


    