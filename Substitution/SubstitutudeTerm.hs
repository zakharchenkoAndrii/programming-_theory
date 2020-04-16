module SubstitutudeTerm where

import Lambda
import Data.List

freshVarsInTerms :: Term -> Term -> [Variable]
freshVarsInTerms x y = intersect (fvars x) (fvars y)

-- new where old
-- function arguments -> 1st new Term, 2nd operation term, 3rd old var that should be substitude
substOperation :: Term -> Term -> Variable -> Term 
substOperation new (VTm y) x | x == y = new --apply substitution by described rules
                         | otherwise = (VTm y)
substOperation new (FTm y m) x | x /= y && not (free x m) = FTm y m
                           | x /= y && free x m && not (free y m) = FTm y (substOperation new m x)
                           | x /= y && free x m && free y m && not (null (freshVarsInTerms new m)) = 
                               FTm ((freshVarsInTerms new m)!!0) (substOperation new (substOperation (VTm ((freshVarsInTerms new m)!!0)) m y) x)
                           | otherwise = FTm x m
substOperation new (ATm t1 t2) x =ATm (substOperation new t1 x) (substOperation new t2 x)
