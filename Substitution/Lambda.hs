module Lambda where

import Prelude
import Data.List(union)


newtype Variable = Var Int deriving Eq

instance Show Variable where
  show (Var iv) = "v" ++ show iv

var :: Int -> Variable  -- smart constructor for Variable
var iv = if iv < 0 then Var 0 else Var iv


data Term
  = VTm Variable
  | FTm Variable Term
  | ATm Term Term
  deriving Eq

instance Show Term where
  show (VTm v)     = show v
  show (FTm v t)   = "(fun " ++ show v ++ " => " ++ show t ++ ")"
  show (ATm t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"


fresh :: Variable -> Term -> Bool
-- checks whether a variable is fresh for a term
fresh v (VTm v')    = v /= v'
fresh v (FTm v' t)  = v /= v' && fresh v t
fresh v (ATm t1 t2) = fresh v t1 && fresh v t2


free :: Variable -> Term -> Bool
-- checks whether a variable is free in a term
free v (VTm v')    = v == v'
free v (FTm v' t)  = v /= v' && free v t
free v (ATm t1 t2) = free v t1 || free v t2


fvars :: Term -> [Variable]
-- buids the list of free variables for a term
fvars (VTm v)     = [v]
fvars (FTm v t)   = filter (\x -> x /= v) (fvars t)
fvars (ATm t1 t2) = fvars t1 ++ fvars t2


combinator :: Term -> Bool
-- checks whether a term is a combinator
combinator t = fvars t == []


subterms :: Term -> [Term]
-- buids the list of subterms for a term
subterms t
  = case t of
    VTm v     -> [t]
    FTm v t'  -> t : subterms t'
    ATm t1 t2 -> t : union (subterms t1) (subterms t2)


bound :: Variable -> Term -> Bool
-- checks whether a variable is bound in a term
bound v t
  = let binder = \tt -> case tt of
                        FTm v _ -> True
                        _       -> False
    in null [t' | t' <- subterms t, binder t']