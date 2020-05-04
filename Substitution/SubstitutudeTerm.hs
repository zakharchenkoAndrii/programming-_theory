import Lambda

fresh_var :: [Term] -> Variable

fresh_var terms = fresh_var' terms 1
    where
        fresh_var' terms count
            | others terms (Var ("v" ++ show count)) = (Var ("v" ++ show count))
            | otherwise                              = fresh_var' terms (count + 1)

        others (x:xs) var = fresh x var && others xs var
        others []     _   = True


subst :: Term -> Term -> Variable -> Term
subst (VTm vt_var) term var
    | var == vt_var = term
    | otherwise     = VTm vt_var

subst (FTm ft_var ft_term) term var
    | var              == ft_var  = FTm var ft_term
    | free term var    == False   = FTm var ft_term
    | free term ft_var == False   = FTm ft_var (subst ft_term term var)
    | otherwise                   = FTm var' term'
        where term'  = (subst ft_term (VTm var') ft_var)
              term'' = (subst term' term var)
              var'   = fresh_var [term, ft_term]

subst (ATm term1 term2) term3 var = ATm (subst term1 term3 var) (subst term2 term3 var)

bt :: [Variable] -> Term -> Term 
bt vars (ATm (FTm var ft_term) at_term) = subst ft_term' at_term var
    where
        ft_term' = replace vars at_term ft_term
        replace  vars at_term  ft_term = replace' vars (fvs at_term) ft_term
        replace' vars []  ft_term      = ft_term
        replace' vars (x:xs) ft_term
            | not(x `elem` b) = subst (replace' vars xs ft_term) (VTm (fresh_var [term])) var
            | otherwise       = replace' vars xs term

        fvs (VTm var)         = [var]
        fvs (ATm term1 term2) = fvs term1 ++ fvs term2
        fvs (FTm var term)    = filter (\x -> x /= var) (fvs term)


et :: Term -> Maybe Term
et (FTm ft_var (ATm at_term (VTm vt_var))) = if ft_var == vt_var && free at_term ft_var == False then Just at_term else Nothing

nrm :: Term -> Term
nrm term = nrm' (Just term) term

nrm' :: Term -> (Maybe Term) -> Term
nrm' term  (Nothing)    = term
nrm' term2 (Just term1) = nrm' term2 (lnrm term1 [])

lnrm :: [Variable] -> Term -> (Maybe Term)
lnrm _    (VTm vt_var)         = Nothing
lnrm vars (FTm ft_var ft_term) =
    let n = bt_et_lnrm vars (FTm ft_var ft_term)
    in case n of
        Nothing ->
            let n_term = lnrm (ft_var:vars) term
            in case n_term of
                    Nothing -> Nothing
                    Just value  -> Just (FTm ft_var value)
        Just value  -> Just value
lnrm vars (ATm term1 term2) =
    let n = bt_et_lnrm vars (ATm term1 term2)
    in case n of
        Nothing ->
            let n_term1 = lnrm vars term1
            in case n_term1 of
                Nothing ->
                    let n_term2 = lnrm vars term2
                    in case n_term2 of
                        Nothing -> Nothing
                        Just value  -> Just (ATm term1 value)
                Just value -> Just (ATm value term2)
        Just value -> n


bt_et_lnrm :: [Variable] -> Term -> (Maybe Term)
bt_et_lnrm  _   (FTm ft_var (ATm at_term at_var))   = et (FTm ft_var (ATm at_term (VTm var)))
bt_et_lnrm vars (ATm (FTm ft_var ft_trem) at_term)  = Just (bt (ATm (FTm ft_var ft_term) at_term) vars)
bt_et_lnrm _     _                                  = Nothing
