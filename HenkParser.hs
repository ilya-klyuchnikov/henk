module HenkParser where

import Parser
import TokenDef
import ParseToken
import HenkAS
import HenkPP()

----------------------------------------------------------------
-- The Program
----------------------------------------------------------------
program :: Parser Program
program =  do{whiteSpace
             ;(tds,vds) <- manyAlternate tDecl vDecl
             ;eof
             ;return $ Program tds vds
             }

manyAlternate :: Parser a -> Parser b -> Parser ([a],[b])
manyAlternate pa pb = do{as<-many1 pa; (as',bs') <- manyAlternate pa pb; return (as++as',bs')}
                      <|>
                      do{bs<-many1 pb; (as',bs') <- manyAlternate pa pb; return (as',bs++bs')}
                      <|>
                      return ([],[])

----------------------------------------------------------------
-- Type Declaration
----------------------------------------------------------------
tDecl :: Parser TDeclaration
tDecl =  do{reserved "data"
           ;t <- bindVar
           ;symbol "="
           ;ts <- braces $ semiSep1 bindVar
           ;return $ TDecl t ts
           }
           <?> "type declaration"

----------------------------------------------------------------
-- Value Declaration
----------------------------------------------------------------
vDecl :: Parser VDeclaration
vDecl  = letnonrec <?> "value Declaration"

letnonrec :: Parser VDeclaration
letnonrec  = do{reserved "let"
               ;tv <- bindVar'
               ;symbol "="
               ;ex <- expr
               ;return $ VDecl tv ex
               }


----------------------------------------------------------------
-- Expression
----------------------------------------------------------------
expr :: Parser Expr
expr = choice
     [piExpr    --pi (\/) before lambda (\) to improve parser efficiency.
     ,lamExpr
     ,caseExpr
     ,funExpr
     ]
     <?> "expression"


atomExpr :: Parser Expr
atomExpr =  choice
            [varExpr
            ,litExpr
            ,sort
            ,unknown
            ,parens expr
            ]
            <?> "atomic expression"

--single expression
single_expr :: Parser Expr
single_expr =do { whiteSpace
                ; ex <- expr
                ; return ex
                }

-----------------------------------------------------------------
-- Application
-----------------------------------------------------------------
appExpr :: Parser Expr
appExpr = do{atoms <- many1 atomExpr;
             return $  foldl1 AppExpr atoms}
          <?> "application"

----------------------------------------------------------------
-- (Capital) Lambda Expression
----------------------------------------------------------------
lamExpr :: Parser Expr
lamExpr =  do{symbol "\\" <|> symbol "/\\"
            ;tvs <- commaSep1 bindVar
            ;symbol "."
            ;e <- expr
            ;return $ foldr LamExpr e tvs}
            <?> "lambda expression"

----------------------------------------------------------------
-- Pi Expression / ForAll Expression
----------------------------------------------------------------
piExpr :: Parser Expr
piExpr  = do{ (symbol "|~|") <|> token (symbol ("\\/"))
          ;tvs <- sepBy1 bindVar comma
          ;symbol "."
          ;e <- expr
          ;return $ foldr PiExpr e tvs}
          <?> "pi expression"


----------------------------------------------------------------
-- Function Expression
----------------------------------------------------------------
funExpr :: Parser Expr
funExpr = chainr1 appExpr arrow <?> "function expression"
 where
 arrow = do{symbol "->"; return $ \ex1 ex2 -> PiExpr (TVar Anonymous ex1) ex2}



----------------------------------------------------------------
-- Case Expression
----------------------------------------------------------------
caseExpr :: Parser Expr
caseExpr = do{reserved "case"
             ;ex  <- expr
             ;reserved "of"
             ;as  <- braces $ semiSep1 alt
             ;case_type <- option Unknown (do{reserved ":"; case_type <- expr ; return case_type})
             ;return $ CaseExpr ex as case_type
             }
             <?> "Case Expression"

alt :: Parser Alt
alt = do{tc   <- boundVar
        ;tcas <- many var
  ;tcas <- return $ map (\v -> TVar v Unknown) tcas
        ;symbol "=>"
        ;res <- expr
        ;return $ Alt tc tcas [] res
        }
        <?> "case alternative"

----------------------------------------------------------------
-- Variable Expression
----------------------------------------------------------------
varExpr = do{tv <- boundVar
            ;return $ VarExpr tv
            }
            <?> "variable expression"


----------------------------------------------------------------
-- Variable
----------------------------------------------------------------
var :: Parser Variable
var = do{v <- identifier
        ;return $ Var v
        }

anonymousvar :: Parser Variable
anonymousvar =
      do{symbol "_"
        ;v <- option "" identifier
        ;return $ Var ('_':v)
        }

----------------------------------------------------------------
-- Binding Variable
----------------------------------------------------------------
bindVar :: Parser TVar
bindVar = do{v <- (anonymousvar <|> var)
          ;(do {e <- isOfType
               ; return $ TVar v e
               }
            <|>
            (return $ TVar v (SortExpr Star)))      --  convention for binding variables
          }
          <?> "variable"

bindVar' :: Parser TVar
bindVar' = do{v <- (anonymousvar <|> var)
             ;(do {e <- isOfType
                  ; return $ TVar v e
                  }
                  <|>
                 (return $ TVar v Unknown))      --  convention for lets
             }
             <?> "variable"

isOfType :: Parser Expr
isOfType =  do{symbol ":"
              ;aex <- expr
              ;return aex}

----------------------------------------------------------------
-- Bound Variable
----------------------------------------------------------------
boundVar :: Parser TVar
boundVar =  do{v <- var
              ;(do {e <- isOfType
                 ;return $ TVar v e
                 }
                 <|>
              (return $ TVar v Unknown))      --  convention for bound variables
              }
              <?> "variable"

----------------------------------------------------------------
-- Literal Expression
----------------------------------------------------------------
litExpr :: Parser Expr
litExpr = do {l <- lit
             ;return $ LitExpr l
             }
             <?> "literal expression"

----------------------------------------------------------------
-- Literal
----------------------------------------------------------------
lit :: Parser Lit
lit = do{i <- natural
        ;return $ LitInt i
        }
      <|>
      do{reserved "Int"
        ;return $ IntType
        }

----------------------------------------------------------------
-- Sort
----------------------------------------------------------------
sort :: Parser Expr
sort = do{s <-    try (sortNum)
              <|> star
              <|> box
     ;return $ SortExpr s}

sortNum :: Parser Sort
sortNum = do{ symbol "*"
            ; n <- natural
            ; return $ SortNum n
            }


star :: Parser Sort
star = do{ symbol "*"
         ; return Star
         }


box  :: Parser Sort
box  = do{ symbol "||"
         ; return Box
         }

----------------------------------------------------------------
-- Unknown
----------------------------------------------------------------
unknown  :: Parser Expr
unknown  = do{ symbol "?"
             ; return Unknown
             }
