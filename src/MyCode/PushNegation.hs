module MyCode.PushNegation where

import FromBNFC.AbsQFBF


pushNegation :: Formula -> Formula
pushNegation (Not formula) = case formula of

    -- not (f1 <=> f2) ~ (not f1 and f2) or (f1 and not f2)
    Equ f1 f2   -> (Not f1 `And` f2) `Or` (f1 `And` Not f2)

    -- not (f1 ==> f2) ~ f1 and not f2
    RImpl f1 f2 -> f1 `And` Not f2

    -- not (f1 <== f2) ~ (not f1) and f2
    LImpl f1 f2 -> (Not f1) `And` f2

    -- not (f1 and f2) ~ (not f1) or (not f2)
    And f1 f2   -> (Not f1) `Or` (Not f2)

    -- not (f1 or f2) ~ (not f1) and (not f2)
    Or f1 f2    -> (Not f1) `And` (Not f2)

    -- not (f1 xor f2) ~ ((not f1) and (not f2)) or (f1 and f2)
    XOr f1 f2   -> ((Not f1) `And` (Not f2)) `Or` (f1 `And` f2)

    -- not not f ~ f
    Not f       -> f
    Var v       -> Not $ Var v

pushNegation formula = formula


pushNegationDeep :: Formula -> Formula
pushNegationDeep formula = case formula of
  
  Var   v     -> formula
  Not   f     -> case f of
                  Var v -> formula
                  _     -> clean $ pushNegation formula
  
  Equ   f1 f2 -> Equ    (clean f1) (clean f2)
  RImpl f1 f2 -> RImpl  (clean f1) (clean f2)
  LImpl f1 f2 -> LImpl  (clean f1) (clean f2)
  And   f1 f2 -> And    (clean f1) (clean f2)
  Or    f1 f2 -> Or     (clean f1) (clean f2)
  XOr   f1 f2 -> XOr    (clean f1) (clean f2)
  
  where clean = pushNegationDeep
