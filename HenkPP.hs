 module HenkPP where

import Pretty -- John Hughes pretty printing library
import HenkAS


program2string :: Program -> String
program2string = (render.program)

expr2string :: Expr -> String
expr2string = (render.expr)

var2string :: Variable -> String
var2string = (render.var)

tVar2string :: TVariable -> String
tVar2string = (render.boundVar)

-- just for testing
--rules2string :: Rules -> String
--rules2string rs = concat $ map (\r -> (rule2string r)++"\n") rs
--rule2string :: Rule -> String
--rule2string (Rule ex1 ex2) = (expr2string ex1) ++ " --> "  ++ (expr2string ex2)

----------------------------------------------------------------
-- The Program
----------------------------------------------------------------
program :: Program -> Doc
program (Prog tds vds) =    vsep (map tDecl tds)
                            $$ vsep (map vDecl vds)

----------------------------------------------------------------
-- Type Declaration
----------------------------------------------------------------
tDecl :: TDeclaration -> Doc
tDecl (TDecl tv tvs) =     text "data"
                       <+> bindVar tv
                       $$  indent (    text "="
                                   <+> br_list (map bindVar tvs))

----------------------------------------------------------------
-- Value Declaration
----------------------------------------------------------------
vDecl :: VDeclaration -> Doc
vDecl (VDecl tv ex) = sep[bindVar tv, text "=" <+> expr ex]

----------------------------------------------------------------
-- Expression
----------------------------------------------------------------
expr :: Expr -> Doc
expr ex = case ex of
 LamExpr tv ex1      -> lamExpr (LamExpr tv ex1)
 PiExpr  tv ex1      -> piExpr  (PiExpr  tv ex1)
 AppExpr ex1 ex2     | isList ex   -> list ex
                     | otherwise   -> left_parents ex1 (expr ex1) <+>  right_parents ex2 (expr ex2)
 CaseExpr ex1 as exs -> caseExpr (CaseExpr ex1 as exs)
 VarExpr tv          -> boundVar tv
 LitExpr l           -> lit l
 SortExpr s          -> sort s
 Unknown             -> text "?"

right_parents :: Expr -> Doc -> Doc
right_parents ex d = case ex of
 AppExpr _ _ -> if (isList ex) then d else parens d
 LamExpr _ _ -> parens d
 PiExpr  _ _ -> parens d
 _           -> d


left_parents :: Expr -> Doc -> Doc
left_parents ex d = case ex of
 LamExpr _ _ -> parens d
 PiExpr  _ _ -> parens d
 _           -> d


----------------------------------------------------------------
-- (Capital) Lambda Expression
----------------------------------------------------------------
lamExpr :: Expr -> Doc
lamExpr ex = case ex of
 LamExpr (TVar (Var v) (SortExpr Star)) ex1 -> sep [text "/\\" <> text v <> text ".", expr ex1]
 LamExpr tv ex1                  -> sep [text "\\" <> bindVar tv <> text ".", expr ex1]

----------------------------------------------------------------
-- Pi,ForAll,Function Expression
----------------------------------------------------------------
piExpr :: Expr -> Doc
piExpr ex = case ex of
 PiExpr (TVar Anonymous ex1) ex2            -> sep [left_parents_function ex1 (expr ex1), text "->", (expr ex2)]
 PiExpr (TVar (Var v) (SortExpr Star)) ex2  -> sep [text "\\/" <> text v <> text ".", expr ex2]
 PiExpr tv ex1                              -> sep [text "|~|" <> bindVar tv <> text ".", expr ex1]

left_parents_function :: Expr -> Doc -> Doc
left_parents_function ex d = case ex of
 PiExpr (TVar Anonymous _) _  -> parens d
 _                            -> d

----------------------------------------------------------------
-- Case Expression
----------------------------------------------------------------
caseExpr :: Expr -> Doc
caseExpr (CaseExpr ex1 as ex)  =     text "case"
                                 <+> expr ex1
                                 <+> text "of"
                                 <+> indent (br_list (map alt as))
                                 -- $$  text ":"
                                 -- <+> expr ex

alt :: CaseAlt -> Doc
alt (Alt tc tcas dcas ex) =
                         boundVar tc
                     <+> (if (null tcas) then (empty) else (comma_sep (map boundVar tcas)))
                     <+> (if (null dcas) then (empty) else (comma_sep (map boundVar dcas)))
                     <+> text "=>"
                     <+> expr ex

----------------------------------------------------------------
-- Variable
----------------------------------------------------------------
bindVar :: TVariable ->  Doc
bindVar tv = case tv of
 TVar (Var v) (SortExpr Star)          -> text v                         -- un-annotated binding variables have type star
 TVar (Anonymous) (SortExpr Star)      -> text "_"
 TVar (Var v) e                        -> text v   <> text ":" <> expr e
 TVar (Anonymous) e                    -> text "_" <> text ":" <> expr e

boundVar :: TVariable ->  Doc
boundVar tv = case tv of
   TVar v Unknown         -> var v -- <> text ": ? "                     -- the type of un-annotated bound variables should be derived from the context
   TVar v e               -> var v -- <> text ":" <> expr e


var :: Variable -> Doc
var v = case v of
 Var n     -> text n
 Anonymous -> text "_"

----------------------------------------------------------------
-- Literal
----------------------------------------------------------------
lit :: Lit -> Doc
lit l = case l of
 LitInt i -> text $ show i
 IntType  -> text "Int"

----------------------------------------------------------------
-- Sorts
----------------------------------------------------------------
sort :: Sort -> Doc
sort s = case s of
 Star      -> text "*"
 Box       -> text "[]"

----------------------------------------------------------------
-- Some Sugar
----------------------------------------------------------------


----------------------------------------------------------------
-- Lists
----------------------------------------------------------------

--isList is slow

isList :: Expr -> Bool
isList ex = case ex of
 AppExpr (VarExpr (TVar (Var "Nil") _ )) _                                -> True
 AppExpr (AppExpr ( AppExpr (VarExpr (TVar (Var "Cons") _)) _) elem) rest -> isList rest
 _                                                                        -> False

list :: Expr -> Doc
list ex = case ex of
 AppExpr (VarExpr (TVar (Var "Nil") _ )) _                                 -> text "[]"
 AppExpr (AppExpr ( AppExpr (VarExpr (TVar (Var "Cons") _)) _) elem) rest  -> text "[" <> expr elem <> listbody rest <> text "]"

listbody :: Expr -> Doc
listbody ex  = case ex of
 AppExpr (VarExpr (TVar (Var "Nil") _ )) _                               -> text ""
 AppExpr (AppExpr ( AppExpr (VarExpr (TVar (Var "Cons") _)) _) elem) rest -> text "," <> expr elem <> listbody rest


----------------------------------------------------------------
-- help functions
----------------------------------------------------------------
indent :: Doc -> Doc
indent = nest 2

vsep xs  = vcat (map ($$ text "") xs)

br_list :: [Doc] -> Doc
br_list (x:xs) = sep $    [text "{" <+> x]
                       ++ foldr (\x y -> [text ";" <+> x] ++ y) [] (xs)
                       ++ [text"}"]
br_list []     = text "{}"

comma_sep :: [Doc] -> Doc
comma_sep (x:xs) = x <> foldr (\x y -> text "," <+> x <> y) empty (xs)
