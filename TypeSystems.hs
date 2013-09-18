module TypeSystems where

import HenkAS

type Specification = (Sorts, Axioms, Rules)
type Sorts         = [Sort]
type Axiom         = (Sort,Sort)
type Axioms        = [Axiom]
type Rule          = (Sort,Sort,Sort)
type Rules         = [Rule]

-- lambda cube sorts 
lcs :: Sorts
lcs = [Star,Box]

-- lambda cube axioms
lca :: Axioms
lca = [(Star,Box)]


-- the rules of lambda arrow, lambda two,
-- lambda omega and the calc. of constructions
lar,l2r,lor,ccr :: Rules
lar = [(Star,Star,Star)]
l2r = lar ++ [(Box,Star,Star)]
lor = l2r ++ [(Box,Box,Box)]
ccr = lor ++ [(Star,Box,Box)]

-- the specification of lambda arrow, lambda two,
-- lambda omega and the calculus of constructions
la,l2,lo,cc :: Specification
la = (lcs,lca,lar)
l2 = (lcs,lca,l2r)
lo = (lcs,lca,lor)
cc = (lcs,lca,ccr)

