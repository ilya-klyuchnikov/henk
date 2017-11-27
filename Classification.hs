module Classification where

import HenkAS
import TypeSystems

crossproduct :: [a] -> [(a,a)]
crossproduct s = [(s1,s2) | s1 <- s, s2 <- s]

functional :: Specification -> Bool
functional (s,a,r) = and (map (\s' -> length (filter (\(s1,_) -> s1==s') a)<2 ) s) &&
                     and (map (\t -> length (filter (\(s1,s2,_) -> (s1,s2)==t) r)<2 ) (crossproduct s))

injective :: Specification -> Bool
injective (s,a,r) = functional (s,a,r) &&
                    and (map (\s -> length (filter (\(_,s2) -> s2==s) a)<2 ) s) &&
                    and (map (\t -> length (filter (\(s1,_,s3) -> (s1,s3)==t) r)<2 ) (crossproduct s))



minus :: Specification -> Maybe Sort -> Maybe Sort
minus _       Nothing  = Nothing
minus (_,a,_) (Just s) = case l of
                        (s',_) :_ -> Just s'
                        []        -> Nothing
      where l = filter (\t -> (snd t) == s) a

plus :: Specification -> Maybe Sort -> Maybe Sort
plus _       Nothing  = Nothing
plus (_,a,_) (Just s) = case l of
                        (_,s') :_ -> Just s'
                        []        -> Nothing
      where l = filter (\t -> (fst t) == s) a

rho :: Specification -> Maybe Sort -> Maybe Sort -> Maybe Sort
rho (_,_,r) (Just s1) (Just s2)
  = case l of
    (_,_,s3') :_ -> Just s3'
    []           -> Nothing
    where l = filter (\(s1',s2',s3') -> s1==s1' && s2'==s2) r
rho _ _ _  = Nothing

mu :: Specification -> Maybe Sort -> Maybe Sort -> Maybe Sort
mu (_,_,r) (Just s1) (Just s2)
  = case l of
    (_,s3',_) :_ -> Just s3'
    []           -> Nothing
    where l = filter (\(s1',s3',s2') -> s1==s1' && s2'==s2) r
mu _ _ _  = Nothing


elmt :: Specification -> Maybe Expr -> Maybe Sort
elmt sp me = case me of
 Nothing -> Nothing
 Just e  -> case e of
  VarExpr  (TVar v e)    -> sortt sp $ Just e
  SortExpr s             -> plus sp $ plus sp $ Just s
  AppExpr m n            -> mu sp (elmt sp $ Just n) (elmt sp $ Just m)
  LamExpr (TVar v e) e2  -> rho sp (sortt sp $ Just e) (elmt sp $ Just e2)
  PiExpr  _ _            -> plus sp $ sortt sp $ Just e
  LitExpr IntType        -> Just $ Star
  LitExpr _              -> Nothing
  CaseExpr _ alts _      -> let ((Alt _ _ _ res):_) = alts in elmt sp $ Just res

sortt :: Specification -> Maybe Expr -> Maybe Sort
sortt sp me = case me of
 Nothing -> Nothing
 Just e  -> case e of
  VarExpr  tv            -> minus sp $ elmt sp $ Just $ VarExpr tv
  SortExpr s             -> plus sp $ Just s
  AppExpr _ _            -> minus sp $ elmt sp $ Just e
  LamExpr _ _            -> minus sp $ elmt sp $ Just e
  PiExpr  (TVar v e) e2  -> rho sp (sortt sp $ Just e) (sortt sp $ Just e2)
  LitExpr (LitInt _)     -> Just Star
  LitExpr IntType        -> Just Box
  CaseExpr _ alts _      -> let ((Alt _ _ _ res):_) = alts in sortt sp $ Just res
