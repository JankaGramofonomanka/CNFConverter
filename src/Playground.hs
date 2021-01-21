module Playground where

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import qualified FromBNFC.LexQFBF as Lex
import qualified FromBNFC.ParQFBF as Par
import qualified FromBNFC.PrintQFBF as Print

import MyCode.PushNegation


testDir :: Int -> String
testDir i
  | i < 10    = "../tests/test0" ++ (show i) ++ ".bf"
  | otherwise = "../test/test" ++ (show i) ++ ".bf"


tempTest1 i = readTestNShow (testDir i) testNegProp prettyPrint
tempTest2 i = readTestNShow (testDir i) justParse prettyPrint
tempTest3 i = do
  tempTest1 i
  tempTest2 i

lexer = Par.myLexer
parser = Par.pFormula



prettyPrint :: Print.Print a => a -> String
prettyPrint toShow = Print.render $ Print.prt 0 toShow

justParse :: String -> Err Formula
justParse = parser . lexer


testNegProp :: String -> Err Formula
testNegProp fileCts = do
  f1 <- parser $ lexer fileCts
  let f2 = pushNegationDeep f1

  return f2

getStr :: (a -> String) -> Err a -> String
getStr printFunc (Ok a) = printFunc a
getStr _ (Bad s) = s

readTestNShow :: String -> (String -> Err a) -> (a -> String) -> IO ()
readTestNShow filename testFunc printFunc = do
  fileCts <- readFile filename

  let output = testFunc fileCts
  let toShow = getStr printFunc output


  putStrLn toShow
    

