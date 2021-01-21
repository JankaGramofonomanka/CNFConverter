module MyCode.Disassemble where

import FromBNFC.AbsQFBF




disassemble :: Formula -> Formula
disassemble formula = case formula of

  -- f1 <=> f2 ~ (f1 and f2) or (not f1 and not f2)
  Equ   f1 f2 -> (f1 `And` f2) `Or` ((Not f1) `And` (Not f2))

  -- f1 ==> f2 ~ (not f1) or f2
  RImpl f1 f2 -> (Not f1) `Or` f2

  -- f1 <== f2 ~ f1 or (not f2)
  LImpl f1 f2 -> f1 `Or` (Not f2)

  -- f1 xor f2 ~ (not f1 and f2) or (f1 and not f2)
  XOr   f1 f2 -> (Not f1 `And` f2) `Or` (f1 `And` Not f2)

  _ -> formula


disassembleDeep :: Formula -> Formula
disassembleDeep formula = case formula of

  Var   v     -> formula
    
  Equ   f1 f2 -> clean $ disassemble formula
  RImpl f1 f2 -> clean $ disassemble formula
  LImpl f1 f2 -> clean $ disassemble formula
  XOr   f1 f2 -> clean $ disassemble formula

  And   f1 f2 -> And  (clean f1) (clean f2)
  Or    f1 f2 -> Or   (clean f1) (clean f2)
  
  Not   f     -> Not  (clean f)
  
  where clean = disassembleDeep




