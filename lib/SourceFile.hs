module SourceFile where


import Declaration ( topLevelDecl )
import AST (ASTBuilder,Declaration, AST (SourceFile))
import Text.Parsec (many)
import Statement (block)
-- SourceFile       = PackageClause ";" { ImportDecl ";" } { TopLevelDecl ";" } .
-- TODO: Add package and import support as well
sourceFile :: ASTBuilder AST 
sourceFile = SourceFile . concat <$>  many ( topLevelDecl block)


    