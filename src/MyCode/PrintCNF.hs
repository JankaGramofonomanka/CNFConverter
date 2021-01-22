module MyCode.PrintCNF where


import qualified Data.Map as M
import qualified Data.Set as S

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import qualified MyCode.ToCNF as CNF
import qualified MyCode.Utils as U



printClause :: (Formula -> Err String) -> String -> [Formula] -> Err String
printClause printLitFunc orSmb literals = do
  literalPrints <- U.errConcat $ map printLitFunc literals

  return $ U.paste literalPrints orSmb
    

printCNF :: (Formula -> Err String) -> String -> String -> [[Formula]] -> 
  Err String
printCNF printLitFunc andSmb orSmb clauses = do
  clausePrints <- U.errConcat $ map (printClause printLitFunc orSmb) clauses

  return $ U.paste clausePrints andSmb





printLiteral :: M.Map Ident String -> String -> Int -> Formula -> Err String
printLiteral varMap notSmb tabLength literal = case literal of

  Var v       -> do
    varName <- getVarName v
    
    let prefix = U.makeSpace $ length notSmb
    let suffix = getSuffix varName

    return $ prefix ++ varName ++ suffix
    
  Not (Var v) -> do
    varName <- getVarName v

    let prefix = notSmb
    let suffix = getSuffix varName
   
    return $ prefix ++ varName ++ suffix
  
  _           -> fail $ "Formula `" ++ (show literal) ++ "` is not a literal."

  where

    getVarName v = case M.lookup v varMap of
    
      Nothing -> fail $ "Variable `" ++ (show v) ++ "` doesn't have a value"
      Just name -> return $ name
    
    getSuffix varName = 
      U.makeSpace $ tabLength - (length varName) - (length notSmb)
    


    
    
    

cadicalPrint :: S.Set Ident -> [[Formula]] -> Int -> Err String
cadicalPrint varSet clauses tabLength = do

  let numClauses = length clauses
  let numVars = length varSet

  let varMap = makeVMap varSet

  let printLitFunc = printLiteral varMap "-" tabLength

  let headline = makeHeadLine numVars numClauses
  cnfPrint <- printCNF printLitFunc " 0\n" " " clauses

  return $ headline ++ "\n" ++ cnfPrint ++ " 0\n"

  where
    makeVMap vSet = vMap where
      (numVars, vMap) = foldl assignInt (1, M.fromList []) vSet

      assignInt (nextInt, oldVMap) var = (nextInt + 1, newVMap) where
        newVMap = M.insert var (show nextInt) oldVMap
    
    makeHeadLine numVars numClauses = U.paste items " " where
      items = ["p", "cnf", show numVars, show numClauses]


humanFriendlyPrint :: S.Set Ident -> [[Formula]] -> Int -> Err String
humanFriendlyPrint varSet clauses tabLength = do

  let numClauses = length clauses
  let numVars = length varSet

  let varMap = makeVMap varSet

  let printLitFunc = printLiteral varMap "!" tabLength

  let headline = makeHeadLine numVars numClauses

  cnfPrint <- printCNF printLitFunc "\nand " " or " clauses

  return $ headline ++ "\n    " ++ cnfPrint

  where
    makeVMap vSet = foldl assignSelf (M.fromList []) vSet where

      assignSelf oldVMap var = newVMap where
        newVMap = M.insert var (getVarName var) oldVMap

      getVarName (Ident name) = name

    makeHeadLine numVars numClauses = U.paste items " " where
      items = [
        "no. variables:", 
        (show numVars), 
        "\nno. clauses:", 
        show numClauses]

