module MyCode.VariableCollection where

import qualified Data.Set as S

import FromBNFC.AbsQFBF


collectVariables :: Formula -> S.Set Ident
collectVariables formula = case formula of

  Equ   f1 f2 -> S.union (collect f1) (collect f2)
  RImpl f1 f2 -> S.union (collect f1) (collect f2)
  LImpl f1 f2 -> S.union (collect f1) (collect f2)
  And   f1 f2 -> S.union (collect f1) (collect f2)
  Or    f1 f2 -> S.union (collect f1) (collect f2)
  XOr   f1 f2 -> S.union (collect f1) (collect f2)

  Not   f     -> collect f
  Var   v     -> S.singleton v

  where collect = collectVariables





