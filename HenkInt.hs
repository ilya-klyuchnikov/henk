module HenkInt where

import HenkAS
import HenkPP(expr2string)
import TermSupport
import Control.Monad
import Debug.Trace

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------
intmain :: DeltaRules -> Program -> IO Expr
intmain deltaRules program
  = do{ex     <- case lookup'' (TVar (Var "main") Unknown) deltaRules of
                  Just deltarule -> return $ (reduceRedex deltaRules (DeltaRedex deltarule) (VarExpr $ TVar (Var "main") Unknown))
                  Nothing         -> error "main not defined!!"
      ;putStr $ "\nEvaluating: \n" ++ HenkPP.expr2string ex ++ "\n\n"
      ;nf     <- return $ reduce_to_mnf deltaRules ex
      ;putStr $ "Result: \n" ++ HenkPP.expr2string nf
      ;return nf
      }


--------------------------------------------------------------------------------
-- Delta Rules
--------------------------------------------------------------------------------
prog2DeltaRules :: Program -> DeltaRules
prog2DeltaRules (Prog _ vdecls) = map vDecl2DeltaRule vdecls

vDecl2DeltaRule :: VDeclaration -> DeltaRule
vDecl2DeltaRule (VDecl tv ex)  = MkDeltaRule tv ex


--------------------------------------------------------------------------------
-- Reducing Redexes
--------------------------------------------------------------------------------
reduceRedex :: DeltaRules -> RedexInf -> Expr -> Expr
reduceRedex dr ri ex = case ri of
 BetaRedex      -> reduceBeta ex
 CaseRedex      -> reduceCase dr ex
 DeltaRedex dr1 -> reduceDeltaRule ex dr1

reduceBeta :: Expr -> Expr
reduceBeta (AppExpr  (LamExpr tv ex1) ex2) = applySubst [(Sub tv ex2)] ex1

reduceCase :: DeltaRules -> Expr -> Expr
reduceCase dr ce@(CaseExpr ex alts _) =
 case lookupA whnf alts of
  Just (Alt tc tcas dcas res) -> applySubToLeftMost [Sub tc (foldr LamExpr res (tcas++dcas))] whnf
  Nothing                     -> error $ "runtime error: missing alternative (" ++ expr2string (leftMost whnf) ++") in case expression!!\n" ++ (expr2string ce)
 where whnf = reduce_to_whnf dr ex

lookupA :: Expr -> [CaseAlt] -> Maybe CaseAlt
lookupA ex alts = lookupA' (leftMost ex) alts
lookupA' ex ((a@(Alt  tc@(TVar va _) _ _ _)):as) = case ex of
 (VarExpr (TVar v _))  | v==va      -> Just  a
                       | otherwise  -> lookupA' ex as
lookupA' ex []    = Nothing





reduceDeltaRule :: Expr -> DeltaRule -> Expr
reduceDeltaRule _ (MkDeltaRule _ ex2) = ex2

--------------------------------------------------------------------------------
-- eval Reduction
--------------------------------------------------------------------------------
-- The result of a reduction of the main function should be a data object
-- (no function). So it only makes sense to reduce inner redexes if there are
-- only variables left of them.


eval :: DeltaRules -> Expr -> Maybe Expr
eval dr ex
 | redexinf/=NoRedex                = mplus (eval dr reduced) (Just reduced)
 | isApp  ex                        = evalApp dr ex
 | otherwise                        = Nothing
 where redexinf = redex dr ex
       reduced  = (reduceRedex dr redexinf ex)


evalApp dr (AppExpr ex1 ex2) =
 case (eval dr ex1) of
   Just ex1r -> mplus (eval dr (AppExpr ex1r ex2)) (Just (AppExpr ex1r ex2))
   Nothing   -> case (eval dr ex2) of
                                 Just ex2r   -> Just $ AppExpr ex1 ex2r
                                 Nothing     -> Nothing


-- Reducing using the normal strategy until a "eval" head normal form is reached
reduce_to_mnf :: DeltaRules -> Expr -> Expr
reduce_to_mnf dr ex = case (eval dr ex) of
                        Just exr   -> exr
                        Nothing    -> ex



--------------------------------------------------------------------------------
-- Weak Reduction
--------------------------------------------------------------------------------
-- weak performes normal order reduction steps until
-- a weak head normal formal is reached
-- normal order = left-most outer-most


weak :: DeltaRules -> Expr -> Maybe Expr
weak dr ex
 | redexinf/=NoRedex                = mplus (weak dr reduced) (Just reduced)
 | isApp  ex                        = weakApp dr ex
 | otherwise                        = Nothing
 where redexinf = redex dr ex
       reduced  = (reduceRedex dr redexinf ex)


weakApp dr (AppExpr ex1 ex2) =
 case (weak dr ex1) of
   Just ex1r -> mplus (weak dr (AppExpr ex1r ex2))
                      (Just (AppExpr ex1r ex2))
   Nothing   -> Nothing

-- Reducing using the normal strategy until a weak head normal form is reached
reduce_to_whnf :: DeltaRules -> Expr -> Expr
reduce_to_whnf dr ex = case (weak dr ex) of
                              Just exr  -> reduce_to_whnf dr exr
                              Nothing   -> ex



--------------------------------------------------------------------------------
-- Strong Reduction
--------------------------------------------------------------------------------
-- we do not need strong reduction for interpreting Henk-programs, however
-- in the type checker we need to check whether types are Beta-equivalent.
-- Because recursion is not allowed on the level of types, types will
-- always reduce to a (unique) nf. Therefore we can check whether two
-- types are Beta-equivalent, by reducing them both to a nf, and checking
-- if they are syntacticly equivalent modulo alpha-conversion.

strong :: DeltaRules -> Expr -> Maybe Expr
strong dr ex
 | not $ redexinf `elem` [NoRedex,BetaRedex]
                                             = mplus (strong dr reduced) (Just reduced)
 | redexinf==BetaRedex                       = let (AppExpr  (LamExpr tv ex1) ex2)=ex
                                                   breduced =  applySStrongSubst (Sub tv ex2) ex1 in
                                                   mplus (strong dr breduced) (Just breduced)
 | isApp  ex                               = strongApp  dr ex
 | isLam    ex                             = strongLam  dr ex
 | otherwise                               = Nothing
 where redexinf = redex dr ex
       reduced  = (reduceRedex dr redexinf ex)

strongApp dr ex@(AppExpr ex1 ex2) =
 case strong dr ex1 of
   Just ex1r -> mplus (strong dr (AppExpr ex1r ex2)) (Just $ AppExpr ex1r ex2)
   Nothing   -> case (strong dr ex2) of
                  Just ex2r -> Just $ AppExpr ex1 ex2r
                  Nothing   -> Nothing


strongLam dr (LamExpr (TVar var exv) ex)
 = if
     (mexvr==Nothing && mexr==Nothing)
   then
     Nothing
   else
     do{exvr<-mplus mexvr (Just exv)
       ;exr <-mplus mexr  (Just ex)
       ;return $ LamExpr (TVar var exvr) exr}
   where
    mexvr = strong dr exv
    mexr  = strong dr ex

-- Reducing using the normal strategy until a normal form is reached
reduce_to_nf :: DeltaRules -> Expr -> Expr
reduce_to_nf dr ex = case  mplus (strong dr mnf) (Just mnf) of
                      Just exr  -> exr
                      Nothing   -> ex
          where mnf = reduce_to_mnf dr ex
