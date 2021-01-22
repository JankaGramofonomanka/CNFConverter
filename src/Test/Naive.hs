module Test.Naive where

import qualified Data.Set as S
import qualified Data.Map as M

import FromBNFC.AbsQFBF
import FromBNFC.ErrM

import MyCode.Disassemble
import MyCode.PushNegation
import MyCode.ToCNF
import MyCode.VariableCollection
import Test.Evaluation



makeVarMaps :: S.Set Ident -> [VarMap]
makeVarMaps varSet = foldl foldF [] varSet where
  
  foldF varMaps var = let
      branchTrue = map (M.insert var True) varMaps
      branchFalse = map (M.insert var False) varMaps

    in branchTrue ++ branchFalse



naiveTest :: Formula -> Err ()
naiveTest formula = do
  
  let variables = collectVariables formula
  let varMaps = makeVarMaps variables

  cnfForm <- (toCNF . pushNegationDeep . disassembleDeep) formula
  cnfFormula <- backToFormula cnfForm

  foldl (assertEqual formula cnfFormula) (Ok ()) varMaps

  where
    assertEqual :: Formula -> Formula -> Err () -> VarMap -> Err ()
    assertEqual f1 f2 (Bad s) _ = Bad s
    assertEqual f1 f2 (Ok ()) varMap = do
      val1 <- evaluate varMap f2
      val2 <- evaluate varMap f2

      if val1 == val2 then 
        return () 
      else 
        fail "Evaluations of the formula are not equal"
    
    



