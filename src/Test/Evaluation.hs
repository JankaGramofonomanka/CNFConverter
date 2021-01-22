module Test.Evaluation where


import qualified Data.Map as M

import FromBNFC.AbsQFBF
import FromBNFC.ErrM


type VarMap = M.Map Ident Bool
evaluate :: VarMap -> Formula -> Err Bool
evaluate varMap formula = case formula of
  
  Var   v     -> case M.lookup v varMap of
    Just b -> Ok b
    Nothing -> fail $ "Variable `" ++ (show v) ++ "` doesn't have a value"

  Equ   f1 f2 -> applyBinOp f1 f2 (==)
  RImpl f1 f2 -> applyBinOp f1 f2 rimpl
  LImpl f1 f2 -> applyBinOp f1 f2 limpl
  And   f1 f2 -> applyBinOp f1 f2 (&&)
  Or    f1 f2 -> applyBinOp f1 f2 (||)
  XOr   f1 f2 -> applyBinOp f1 f2 (xor)

  Not   f     -> eval f >>= return . not
  

  where 
    eval = evaluate varMap

    rimpl :: Bool -> Bool -> Bool
    rimpl p q = (not p) || q

    limpl :: Bool -> Bool -> Bool
    limpl p q = p || (not q)

    xor :: Bool -> Bool -> Bool
    xor p q = (p && not q) || (not p && q)

    applyBinOp :: Formula -> Formula -> (Bool -> Bool -> Bool) -> Err Bool
    applyBinOp f1 f2 op = do
      val1 <- eval f1
      val2 <- eval f2
      return $ op val1 val2



