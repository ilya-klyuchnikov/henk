module HenkAS where

-- Abstract Syntax --

-- The Program
data Program
  = Program [TDeclaration] [VDeclaration]
    deriving (Show,Eq)

-- Data Type Declaration
data TDeclaration
 = TDecl TCons [DCons]
   deriving (Show,Eq)

type TCons = TVar  -- Type Constructor
type DCons = TVar  -- Data Constructor

-- Value Declaration
data VDeclaration
 = VDecl TVar Expr
   deriving (Show,Eq)

-- Expression
data Expr
 = LamExpr TVar Expr            -- Lambda Abstraction
 | PiExpr TVar Expr             -- Pi
 | AppExpr Expr Expr            -- Application
 | CaseExpr Expr [Alt] Expr     -- Case
 | VarExpr TVar                 -- Typed Variable
 | LitExpr Lit                  -- Literal
 | SortExpr Sort                -- Sorts
 | Unknown                      -- for untyped variables
   deriving (Show,Eq)

-- Typed Variable
data TVar
 = TVar Var Expr
   deriving (Show,Eq)

-- Variable
data Var
 = Var String
 | Anonymous
 deriving (Show,Eq)

-- Case Alternative
data Alt
 = Alt TCons [TCA] [DCA] Expr
 deriving (Show,Eq)

type TCA = TVar -- type constructor argument
type DCA = TVar -- data constructor argument

-- Literals
data Lit
 = LitInt  Integer
 | IntType
 deriving (Show,Eq)

-- Sorts
data Sort
 = Star
 | Box
 | SortNum Integer
 deriving (Show,Eq)
