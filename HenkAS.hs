module HenkAS where

-- Abstract Syntax --

-- The Program
data Program
  = Prog [TDeclaration] [VDeclaration]
    deriving (Show,Eq)

-- Data Type Declaration
data TDeclaration
 = TDecl TCons [DCons]
   deriving (Show,Eq)

type TCons = TVariable  -- Type Constructor
type DCons = TVariable  -- Data Constructor

-- Value Declaration
data VDeclaration
 = VDecl TVariable Expr
   deriving (Show,Eq)

-- Expression
data Expr
 = LamExpr TVariable Expr            -- Lambda Abstraction
 | PiExpr TVariable Expr             -- Pi
 | AppExpr Expr Expr                 -- Application
 | CaseExpr Expr [CaseAlt] Expr      -- Case
 | VarExpr TVariable                 -- Typed Variable
 | LitExpr Lit                       -- Literal
 | SortExpr Sort                     -- Sorts
 | Unknown                           -- for untyped variables
   deriving (Show,Eq)

-- Typed Variable
data TVariable
 = TVar Variable Expr
   deriving (Show,Eq)

-- Variable
data Variable
 = Var String
 | Anonymous
 deriving (Show,Eq)

-- Case Alternative
data CaseAlt
 = Alt TCons [TCA] [DCA] Expr
 deriving (Show,Eq)

type TCA = TVariable -- type constructor argument
type DCA = TVariable -- data constructor argument

-- Literals
data Lit
 = LitInt  Integer
 | IntType
 deriving (Show,Eq)

-- Sorts
data Sort
 = Star
 | Box
 deriving (Show,Eq)
