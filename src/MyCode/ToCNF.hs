module MyCode.ToCNF where


import FromBNFC.AbsQFBF
import FromBNFC.ErrM


cAnd :: [[Formula]] -> [[Formula]] -> [[Formula]]
cAnd f1 f2 = f1 ++ f2

cOr :: [Formula] -> [Formula] -> [Formula]
cOr f1 f2 = f1 ++ f2

toCNF :: Formula -> Err [[Formula]]
toCNF formula = case formula of

    Var v     -> return [[formula]]
    Not f     -> case f of
                  Var v -> return [[formula]]
                  _     -> fail notDeepEnoughMsg

    Equ   _ _ -> fail complexOpMsg
    RImpl _ _ -> fail complexOpMsg
    LImpl _ _ -> fail complexOpMsg
    XOr   _ _ -> fail complexOpMsg
    
    And f1 f2 -> do
      cF1 <- toCNF f1
      cF2 <- toCNF f2
      return $ cF1 `cAnd` cF2

    Or f1 f2  -> do
      cF1 <- toCNF f1
      cF2 <- toCNF f2
      return $ dealWithOr cF1 cF2

    where 
      complexOpMsg = "Attempt to convert complex operator to CNF"
      notDeepEnoughMsg = "Negation not pushed deep enough"



dealWithOr :: [[Formula]] -> [[Formula]] -> [[Formula]]
dealWithOr [clause] f2 = map (cOr clause) f2
dealWithOr f1 f2 = foldl foldF [] f1 where
  foldF acc clause = acc `cAnd` dealWithOr [clause] f2



-------------------------------------------------------------------------------
assertLiteral :: Formula -> Err Formula
assertLiteral f = case f of
  Var v -> return f
  Not (Var v) -> return f
  _ -> fail $ "Formula `" ++ (show f) ++ "` is not a literal."


fromClause :: [Formula] -> Err Formula
fromClause [literal] = assertLiteral literal
fromClause (lit:lits) = do
  trueLit <- assertLiteral lit
  clause <- fromClause lits
  return $ trueLit `Or` clause


backToFormula :: [[Formula]] -> Err Formula
backToFormula [clause] = fromClause clause
backToFormula (clause:clauses) = foldl foldF (fromClause clause) clauses where
  
  foldF :: Err Formula -> [Formula] -> Err Formula
  foldF errFormula cl = do
    accFormula <- errFormula
    clauseFormula <- fromClause cl
    return $ accFormula `And` clauseFormula
  




