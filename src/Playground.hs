module Playground where

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import qualified FromBNFC.LexQFBF as Lex
import qualified FromBNFC.ParQFBF as Par
import qualified FromBNFC.PrintQFBF as Print

import MyCode.PushNegation
import MyCode.Disassemble


testDir :: Int -> String
testDir i
  | i < 10    = "../tests/test0" ++ (show i) ++ ".bf"
  | otherwise = "../test/test" ++ (show i) ++ ".bf"



tempTest i toTest = readTestNShow (testDir i) toTest prettyPrint
tempTestCombo i = do
  tempTest i $ pushNegationDeep . disassembleDeep
  tempTest i disassembleDeep
  tempTest i id

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
    

