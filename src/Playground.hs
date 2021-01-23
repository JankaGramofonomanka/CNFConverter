module Playground where

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import qualified FromBNFC.LexQFBF as Lex
import qualified FromBNFC.ParQFBF as Par
import qualified FromBNFC.PrintQFBF as Print

import MyCode.PushNegation
import MyCode.Disassemble
import MyCode.ToCNF
import MyCode.VariableCollection
import MyCode.PrintCNF

import Test.Evaluation
import Test.Naive

import qualified Main as M


-------------------------------------------------------------------------------
testDir :: Int -> String
testDir i
  | i < 10    = "../tests/test0" ++ (show i) ++ ".bf"
  | otherwise = "../test/test" ++ (show i) ++ ".bf"

-------------------------------------------------------------------------------
toCNFAndBack :: Formula -> Err Formula
toCNFAndBack formula = do
  cnfForm <- (toCNF . pushNegationDeep . disassembleDeep) formula
  
  backToFormula cnfForm


toCadical :: Formula -> Err String
toCadical formula = do
  cnfForm <- (toCNF . pushNegationDeep . disassembleDeep) formula
  let varSet = collectVariables formula
  cadicalPrint varSet cnfForm 4

toHumanFriendly :: Formula -> Err String
toHumanFriendly formula = do
  cnfForm <- (toCNF . pushNegationDeep . disassembleDeep) formula
  let varSet = collectVariables formula
  humanFriendlyPrint varSet cnfForm 4


tempTest i toTest = readTestNShow (testDir i) toTest prettyPrint
tempTestCombo i = do
  readTestNShow (testDir i) toHumanFriendly (getStr id)
  putStrLn ""
  readTestNShow (testDir i) toCadical (getStr id)

  readTestNShow (testDir i) toCNFAndBack (getStr prettyPrint)

  tempTest i $ pushNegationDeep . disassembleDeep
  tempTest i disassembleDeep
  tempTest i id


-------------------------------------------------------------------------------
lexer = Par.myLexer
parser = Par.pFormula



prettyPrint :: Print.Print a => a -> String
prettyPrint toShow = Print.render $ Print.prt 0 toShow

--justParse :: String -> Err Formula
--justParse = parser . lexer


testFunc :: String -> (Formula -> a) -> Err a
testFunc fileCts funcToTest = do
  f1 <- parser $ lexer fileCts
  let f2 = funcToTest f1

  return f2

getStr :: (a -> String) -> Err a -> String
getStr printFunc (Ok a) = printFunc a
getStr _ (Bad s) = s

readTestNShow :: String -> (Formula -> a) -> (a -> String) -> IO ()
readTestNShow filename funcToTest printFunc = do
  fileCts <- readFile filename

  let output = testFunc fileCts funcToTest
  let toShow = getStr printFunc output


  putStrLn toShow
    

-------------------------------------------------------------------------------
testMainF :: String -> IO ()
testMainF filename = do
  fileCts <- readFile filename
  putStrLn fileCts
  toShow <- M.getErr $ M.compileToCNF fileCts

  putStrLn $ show toShow





