module HenkTI where

import HenkAS
import HenkPP(var2string)
import TermSupport(DeltaRules,SSubst(..),Subst,applyStrongSubst)
import Control.Monad(mapAndUnzipM,foldM)
import Parser(parse)
import HenkParser(single_expr)
import HenkPP(expr2string)
trace x y = y

type Annotation  = (Variable,Expr)
type Annotations = [Annotation]
type Error       = String
type Errors      = [String]

tVar2Ann :: TVariable -> Annotation
tVar2Ann (TVar v ex) = (v,ex)

--------------------------------------------------------------------------------
-- The Type Inference Monad
--------------------------------------------------------------------------------
newtype TI t = TI (Annotations -> (Errors,t) )

instance Functor TI where
  fmap f (TI g) = TI (\as -> case (g as) of (es, ts) -> (es, f ts))

instance Applicative TI where
  pure t = TI (\x -> ([], t))
  (TI f) <*> (TI g) = TI (\ann -> let (err1, t1) = f ann
                                      (err2, t2) = g ann
                                  in
                                  (err1 ++ err2, t1 t2))

instance Monad TI where
 return x      = TI (\ann -> ([],x) )
 TI f >>= g    = TI (\ann -> let  (erf,ta)        = f ann
                                  TI gta          = g ta
                                  (erg,tb)        = gta ann
                                  in
                                  (erf++erg,tb))

runTI        :: Annotations -> Program -> TI a -> (Errors,a)
runTI anns p (TI f) = (er,result)
 where (er,result) = f anns

timain :: Annotations -> Program -> (Errors,(Program,Annotations))
timain anns p = runTI anns p (program p)

tiexpr :: Annotations -> Program -> Expr -> (Errors,Expr)
tiexpr anns p ex = runTI anns p (expr p ex)

addErrorMsg :: String -> TI ()
addErrorMsg s = TI (\ann -> ([s],()))

withAnn :: Annotation -> TI a -> TI a
withAnn ann (TI f) = (TI (\anns -> f (ann:anns)))

withAnns :: Annotations -> TI a -> TI a
withAnns anns' (TI f) = (TI (\anns -> f (anns'++anns)))

getAnn :: TI Annotations
getAnn = TI (\ann -> ([],ann))

lookup' :: Variable -> TI Expr
lookup' v = do {ann <- getAnn
               ;case lookup v ann of
                     Just ex -> return ex
                     Nothing -> do{addErrorMsg ("Warning: Could not derive type of bound variable: "++(var2string v))
                                  ;return Unknown}
               }
--------------------------------------------------------------------------------
-- Program
--------------------------------------------------------------------------------
program :: Program -> TI (Program,Annotations)
program p@(Prog tds vds) =
 do{(tds,anns)    <- mapAndUnzipM (\td -> tDeclIdent p td) tds
   ;(tds,annss)   <- mapAndUnzipM (\td-> withAnns anns (tDeclBody p td)) tds
   ;anns          <- return $ concat annss ++ anns
   ;(vds,anns2)   <- help p vds anns
   ;anns          <- return $ anns2 ++ anns
   ; vds          <- mapM (\vd-> withAnns anns (vDeclBody p vd)) vds
   ;return $ (Prog tds vds,anns)
   }


help :: Program -> [VDeclaration] -> Annotations -> TI ([VDeclaration],Annotations)
help p vds anns =
 case vds of
 []      -> return ([],anns)
 vd:vds  -> do {(vd,anns2) <- withAnns anns (vDeclIdent p vd)
               ;(vds,anns) <- help p vds (anns++anns2)
               ;return (vd:vds,anns)}

--------------------------------------------------------------------------------
-- Type Declarations
--------------------------------------------------------------------------------
tDeclIdent :: Program -> TDeclaration -> TI (TDeclaration,Annotation)
tDeclIdent p (TDecl tv tvs)
 = do{tv <- bindVar p tv
     ;return $ ((TDecl tv tvs),tVar2Ann tv)}

tDeclBody    :: Program -> TDeclaration -> TI (TDeclaration,Annotations)
tDeclBody p (TDecl tv tvs) =
 do{tvs  <- mapM (\tv -> bindVar p tv) tvs
   ;anns <- return (map tVar2Ann tvs)
   ;return $ (TDecl tv tvs,anns)}


--------------------------------------------------------------------------------
-- Value Declarations
--------------------------------------------------------------------------------

vDeclIdent :: Program -> VDeclaration -> TI (VDeclaration,Annotations)
vDeclIdent p (VDecl tv ex)
 =  do{tv   <- bindVar p tv
      ;return (VDecl tv ex, [tVar2Ann tv])}


vDeclBody :: Program -> VDeclaration -> TI VDeclaration
vDeclBody p (VDecl tv ex)
 =  do{ex <- expr p ex
      ;return $ VDecl tv ex}


bindVar :: Program -> TVariable -> TI TVariable
bindVar p tv@(TVar v ex) = case ex of
                           Unknown -> do{addErrorMsg $ "Warning: Unannotated bind variable: "++var2string v
                                        ; return $ TVar v Unknown}
                           _       -> do{ ex <- expr p ex
                                        ; return $ TVar v ex}

boundVar :: Program -> TVariable -> TI TVariable
boundVar p tv@(TVar v ex) = case ex of
                            Unknown -> do {ex <- lookup' v;return $ TVar v ex}
                            _       -> do {ex <- expr p ex;return $ TVar v ex}


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
expr :: Program -> Expr -> TI Expr
expr p ex = case ex of
 AppExpr ex1 ex2    -> do{ex1 <- expr p ex1
                         ;ex2 <- expr p ex2
                         ;return $ AppExpr ex1 ex2}
 VarExpr tv         -> do{tv <- boundVar p tv
                         ;return $ VarExpr tv}
 LamExpr tv ex      -> do{tv <- bindVar p tv
                         ;ex <- withAnn (tVar2Ann tv) (expr p ex)
                         ;return $ LamExpr tv ex}
 PiExpr tv ex       -> do{tv <- bindVar p tv
                         ;ex <- withAnn (tVar2Ann tv) (expr p ex)
                         ;return $ PiExpr tv ex}
 CaseExpr ex as ct  -> caseExpr p $ CaseExpr ex as ct
 _                  -> return ex


caseExpr :: Program -> Expr -> TI Expr
caseExpr p (CaseExpr ex as ct) =
 do {ex  <- expr p ex
    ;as  <- mapM (\a -> alt p a) as
    ;ct  <- expr p ct
    ;return $ CaseExpr ex as ct}


alt :: Program -> CaseAlt -> TI CaseAlt
alt p (Alt dc tcas dcas res) =
    do{dc@(TVar _ dc_type) <- boundVar p dc
      ;as                  <- return $ tcas ++ dcas
      ;anns                <- return $ map tVar2Ann $ match dc_type as
      ;res                 <- withAnns anns (expr p res)
      ;return $ Alt dc tcas dcas res}

match :: Expr -> [TVariable] -> [TVariable]
match (PiExpr ptv@(TVar v t) expr) (tv@(TVar v1 _):vs) = (TVar v1 t) : (match (trace (expr2string $  (applyStrongSubst  [Sub ptv $ VarExpr (TVar v1 t)] expr)) (applyStrongSubst  [Sub ptv $ VarExpr (TVar v1 t)] expr)) vs)
match _ _ = []
