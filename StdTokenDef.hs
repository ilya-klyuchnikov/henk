-----------------------------------------------------------
-- Daan Leijen (c) 1999, daan@cs.uu.nl
--
-- $version: 23 Feb 2000, release version 0.2$
-----------------------------------------------------------
module StdTokenDef (TokenDef(..)
                   ,haskellStyle, javaStyle
                   ,emptyStyle
                   ,haskell, haskellExt
                   ,mondrian
                   ) where

import Parser

-----------------------------------------------------------
-- TokenDef
-----------------------------------------------------------
data TokenDef  = TokenDef 
               { commentStart   :: String
               , commentEnd     :: String
               , commentLine    :: String
               , nestedComments :: Bool
               , identStart     :: Parser Char
               , identLetter    :: Parser Char
               , opStart        :: Parser Char
               , opLetter       :: Parser Char
               , reservedNames  :: [String]
               , reservedOpNames:: [String]
               , caseSensitive  :: Bool
               }                           
           
-----------------------------------------------------------
-- Styles: haskellStyle, javaStyle
-----------------------------------------------------------               
haskellStyle= emptyStyle                      
                { commentStart   = "{-"
                , commentEnd     = "-}"
                , commentLine    = "--"
                , nestedComments = True
                , identStart     = letter
                , identLetter	 = alphaNum <|> oneOf "_'"
                , opStart	 = opLetter haskell
                , opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"              
                , reservedOpNames= []
                , reservedNames  = []
                , caseSensitive  = True                                   
                }         
                           
javaStyle   = emptyStyle
		{ commentStart	 = "/*"
		, commentEnd	 = "*/"
		, commentLine	 = "//"
		, nestedComments = True
		, identStart	 = letter
		, identLetter	 = alphaNum <|> oneOf "_'"
		-- fixed set of operators: use 'symbol'
		, reservedNames  = []
		, reservedOpNames= []	
                , caseSensitive  = False				  
		}

-----------------------------------------------------------
-- Haskell
-----------------------------------------------------------               
haskellExt  = haskell
	        { identLetter	 = identLetter haskell <|> char '#'
	        , reservedNames	 = reservedNames haskell ++ 
    				   ["foreign","import","export","primitive"
    				   ,"_ccall_","_casm_"
    				   ,"forall"
    				   ]
                }
			    
haskell     = haskellStyle
                { reservedOpNames= ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames  = ["let","in","case","of","if","then","else",
                                    "data","type",
                                    "class","default","deriving","do","import",
                                    "infix","infixl","infixr","instance","module",
                                    "newtype","where",
                                    "primitive"
                                    -- "as","qualified","hiding"
                                   ]
                }         
                
                
-----------------------------------------------------------
-- Mondrian
-----------------------------------------------------------               
mondrian    = javaStyle
		{ reservedNames = [ "case", "class", "default", "extends"
				  , "import", "in", "let", "new", "of", "package"
				  ]	
                , caseSensitive  = True				  
		}

				
-----------------------------------------------------------
-- minimal token definition
-----------------------------------------------------------                
emptyStyle
            = TokenDef 
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = ""
               , nestedComments = True
               , identStart     = unexpected "identifier"
               , identLetter    = unexpected "identifier"
               , opStart        = unexpected "operator"
               , opLetter       = unexpected "operator"
               , reservedOpNames= []
               , reservedNames  = []
               , caseSensitive  = True
               }
                
