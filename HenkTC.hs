module HenkTC where

import HenkAS
import Debug.Trace
import HenkPP(var2string,tVar2string,expr2string)
import HenkInt(reduce_to_whnf,reduce_to_nf)
import TermSupport
import Classification
import TypeSystems


--------------------------------------------------------------------------------
-- The Type Check Monad
--------------------------------------------------------------------------------
type Error       = String
type Errors      = [Error]

newtype TC t = TC (Errors,t)

instance Functor TC where
  fmap f (TC (es, ts)) = TC (es, f ts)

instance Applicative TC where
  pure t = TC ([], t)
  (TC (err1, f)) <*> (TC (err2, t)) = (TC (err1 ++ err2, f t))

instance Monad TC where
 return x    = TC ([],x)
 f >>= g     = TC (let TC (erf,x)     = f
                       TC (erg,y)     = g x
                       in (erf++erg,y))

runTC        ::  TC a -> (Errors,a)
runTC (TC (er,result)) = (er,result)


tcmain :: DeltaRules -> Specification -> Program -> (Errors,())
tcmain dr sp p = runTC (program dr sp p)

tcexpr :: DeltaRules -> Specification -> Expr -> (Errors,Expr)
tcexpr dr sp ex = runTC (expr dr sp ex)

addErrorMsg :: String -> TC ()
addErrorMsg s = TC([s],())


type TypeCheck a b = DeltaRules -> Specification -> a -> TC b


--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------
program :: TypeCheck Program ()
program  dr sp (Program tds vds) = do{tds <- mapM (tDecl dr sp) tds
                                    ;vds <- mapM (vDecl dr sp) vds
                                    ;return $ () }

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------
tDecl    :: TypeCheck TDeclaration ()
tDecl dr sp td@(TDecl tv tvs) = do{mapM (\(TVar _ t) -> expr dr sp t) (tv:tvs)
                                  ;isOfRightForm dr sp td
                                  ;return ()}


isOfRightForm :: TypeCheck TDeclaration ()
isOfRightForm dr sp td = return ()
-- should check whether the ADT is of the form
-- described in definition 3.2 of the thesis
-- but is not yet implemented

--------------------------------------------------------------------------------
-- Value Declarations
--------------------------------------------------------------------------------
vDecl    :: TypeCheck VDeclaration ()
vDecl dr sp (VDecl tv@(TVar _ tv_type) ex)
 = do {ex_type <- expr dr sp ex
      ;if
         not $ equal tv_type ex_type
       then
         do{addErrorMsg $ bindMsg tv tv_type ex ex_type
           ;return $ ()}
       else
         return ()
      }


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
expr :: TypeCheck Expr Expr
expr dr sp ex = case ex of
 SortExpr _         -> sortExpr dr sp ex                  -- (STAR)
 VarExpr _          -> varExpr  dr sp ex                  -- (VAR)
 PiExpr _  _        -> piExpr   dr sp ex                  -- (PI)
 LamExpr _ _        -> lamExpr  dr sp ex                  -- (LAM)
 AppExpr _ _        -> appExpr  dr sp ex                  -- (APP)
 CaseExpr _ _ _     -> caseExpr dr sp ex                  -- (CASE)
 LitExpr _          -> litExpr  dr sp ex                  -- (INT)
 Unknown            -> return Unknown

exprwh :: TypeCheck Expr Expr
exprwh dr sp ex =
 do{ex <- expr dr sp ex
   ;return $ reduce_to_whnf dr ex}


--------------------------------------------------------------------------------
-- (SORT)
--------------------------------------------------------------------------------
sortExpr :: DeltaRules -> Specification -> Expr -> TC Expr
sortExpr _ sp@(s,a,_) (SortExpr s1)
 = case lookup s1 a of
   Just s2 -> do{return $ SortExpr s2}
   Nothing -> do{addErrorMsg $ noAxiomMsg s1
                ;return Unknown}


--------------------------------------------------------------------------------
-- (VAR)
--------------------------------------------------------------------------------
varExpr :: DeltaRules -> Specification -> Expr -> TC Expr
varExpr _ _ (VarExpr (TVar _ ex)) = return ex


--------------------------------------------------------------------------------
-- (APP)
--------------------------------------------------------------------------------
appExpr :: TypeCheck Expr Expr
appExpr dr sp (AppExpr f c)
 = do{f_type <- exprwh dr sp f
     ;a      <- expr dr sp c
     ;if
        (not (isPi f_type))
      then
        do{addErrorMsg $ appMsg2 f f_type c a
          ;return  Unknown}
      else
        let
          (PiExpr tv@(TVar x a') b)=f_type
        in
          do{a  <- return $ reduce_to_nf dr a
            ;a' <- return $ reduce_to_nf dr a'
            ;if
               not (equal a a')
             then
               do{addErrorMsg $ appMsg1 f f_type c a a'
                 ;return Unknown}
             else
               do{return $ applySSubst (Sub tv c) b}
             }
     }


--------------------------------------------------------------------------------
-- (LAM)
--------------------------------------------------------------------------------
lamExpr :: TypeCheck Expr Expr
lamExpr dr sp ex@(LamExpr tv@(TVar x a) m)
 = do{b <- expr dr sp m
     ;case rho sp (sortt sp $ Just a) (elmt sp $ Just m) of
       Just _  -> return $ PiExpr tv b
       Nothing -> do{addErrorMsg $ lamMsg ex ; return Unknown}}


--------------------------------------------------------------------------------
-- (PI)
--------------------------------------------------------------------------------
piExpr :: TypeCheck Expr Expr
piExpr dr sp@(_,_,r) ex@(PiExpr (TVar v a) b)
 = do{btype        <- exprwh dr sp b
     ;s1           <- return $ sortt sp (Just a)
     ;case s1 of
       Nothing -> do{addErrorMsg $ noSortAMsg ex
                    ;return Unknown}
       Just s1  ->
        do{case btype of
         SortExpr s2 -> do{case lookup' (s1,s2) r of
                            Nothing       -> do{addErrorMsg $ ruleMsg ex s1 s2
                           ;return Unknown}
                            Just (_,_,s3) -> return $ SortExpr s3
              }
         _           -> do{addErrorMsg $ noSortBMsg ex btype
                  ;return Unknown}
      }
      }


lookup' :: (Eq a , Eq b) => (a,b) -> [(a,b,c)] -> Maybe (a,b,c)
lookup' (a,b) l
 = case filter (\(t1,t2,_) -> t1==a && t2==b) l of
   x:xs -> Just x
   _    -> Nothing


--------------------------------------------------------------------------------
-- (CASE)
--------------------------------------------------------------------------------
caseExpr :: TypeCheck Expr Expr
caseExpr dr sp ce@(CaseExpr ex alts Unknown) =
 do{ex_type                      <- expr dr sp ex
   ;tc                           <- return $ leftMost ex_type
   ;atcas                        <- return $ ex_atcas ex_type
   ;rt <- mapM (\(Alt _ tcas _ res)->
          do{subst                        <- return $ zipWith Sub tcas atcas
            ;res                          <- return $ applySubst subst res
            ;expr dr sp res})  alts
   ;ct <- return $ foldr1 (\t1 t2 -> if equal t1 t2 then t1 else Unknown) rt
   ;if
      ct==Unknown
    then
      do{addErrorMsg $ caseMsg ce
        ;return ct}
    else
        return ct
   }
caseExpr _ _ (CaseExpr _ _ t) = return t

ex_atcas :: Expr -> [Expr]
ex_atcas  ex  = tail $ ex_atcas' ex
ex_atcas' ex  = case ex of
 AppExpr ex1 ex2 -> (ex_atcas' ex1)++[ex2]
 _               -> [ex]

--------------------------------------------------------------------------------
-- (LIT)
--------------------------------------------------------------------------------
litExpr :: TypeCheck Expr Expr
litExpr _ _ (LitExpr lit)
 = case lit of
   LitInt _ -> do{return $ LitExpr IntType}
   IntType  -> do{return $ SortExpr Star}



--------------------------------------------------------------------------------
-- Fancy Error Messages
--------------------------------------------------------------------------------
boundVarMsg :: TVar -> Expr -> Error
boundVarMsg tv@(TVar v tv_type1) tv_type2
 = "Type error in bound variable: " ++ var2string  v ++ "\n" ++
   "Annotation                  : " ++ expr2string tv_type1 ++ "\n" ++
   "Derived from context        : " ++ expr2string tv_type2 ++ "\n"

appMsg1 :: Expr -> Expr -> Expr -> Expr -> Expr -> Error
appMsg1 f f_type a a_type var_type
 = "Type error in application   : " ++ expr2string (AppExpr f a) ++ "\n" ++
   "Left Expr                   : " ++ expr2string f             ++ "\n" ++
   "Type Left Expr              : " ++ expr2string f_type        ++ "\n" ++
   "Right Expr                  : " ++ expr2string a             ++ "\n" ++
   "Type Right Expr             : " ++ expr2string a_type        ++ "\n" ++
   "Type mismatch because       : " ++ expr2string a_type        ++ "\n" ++
   "does not match              : " ++ expr2string var_type      ++ "\n"

lamMsg :: Expr -> Error
lamMsg _ = "lamerror"


appMsg2 :: Expr -> Expr -> Expr -> Expr -> Error
appMsg2 ex1 ex1_type ex2 ex2_type
 = "Type error in application   : " ++ expr2string (AppExpr ex1 ex2) ++ "\n" ++
   "Left Expr                   : " ++ expr2string ex1               ++ "\n" ++
   "Type Left Expr              : " ++ expr2string ex1_type          ++ "\n" ++
   "Right Expr                  : " ++ expr2string ex2               ++ "\n" ++
   "Type Right Expr             : " ++ expr2string ex2_type          ++ "\n" ++
   "Left expression does not have PI-type!!"                         ++ "\n"

bindMsg :: TVar -> Expr -> Expr -> Expr -> Error
bindMsg tv tv_type ex ex_type
 = "Type error in binding       : " ++ tVar2string tv ++ " = " ++ expr2string ex  ++ "\n" ++
   "Variable                    : " ++ tVar2string tv                ++ "\n" ++
   "Type                        : " ++ expr2string tv_type           ++ "\n" ++
   "Expression                  : " ++ expr2string ex                ++ "\n" ++
   "Type Expression             : " ++ expr2string ex_type           ++ "\n"

noSortAMsg :: Expr -> Error
noSortAMsg piExpr@(PiExpr tv@(TVar v a) b)
 = "Type error in Pi expression" ++ "\n" ++
   "Pi expression               : " ++ expr2string piExpr            ++ "\n" ++
   "Variable                    : " ++ tVar2string tv                ++ "\n" ++
   "Type of variable            : " ++ expr2string a                 ++ "\n" ++
   "is not typable by a sort!"

noSortBMsg :: Expr -> Expr -> Error
noSortBMsg piExpr@(PiExpr tv@(TVar v a) b) btype
 = "Type error in Pi expression" ++ "\n" ++
   "Pi expression               : " ++ expr2string piExpr            ++ "\n" ++
   "Body                        : " ++ expr2string b                 ++ "\n" ++
   "Type of body reduces to     : " ++ expr2string btype             ++ "\n" ++
   "but should be a sort!"

ruleMsg :: Expr -> Sort -> Sort  -> Error
ruleMsg piExpr s1 s2
 = "Current type system is not strong enough!!\n" ++
   "One needs a rule of the form: (" ++ (expr2string $ SortExpr s1) ++ "," ++ (expr2string $ SortExpr s2) ++ ", _)" ++ "\n" ++
   "To type                     : " ++ expr2string piExpr  ++ "\n"

noSortMsg :: Sort -> Error
noSortMsg s1
 = "Unknown sort: " ++ expr2string (SortExpr s1) ++ " encounterd!! \n"

noAxiomMsg :: Sort -> Error
noAxiomMsg s1
 = "There is no axiom to type sort: " ++ expr2string (SortExpr s1) ++ " !! \n"

caseMsg :: Expr -> Error
caseMsg ce
 = "Result expressions have different types in:\n" ++
    expr2string ce
