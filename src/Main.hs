module Main where


import System.Environment (getArgs)

import qualified Data.Set as S

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import FromBNFC.ParQFBF (myLexer, pFormula)

import MyCode.Disassemble (disassembleDeep)
import MyCode.PushNegation (pushNegationDeep)
import MyCode.ToCNF (toCNF, backToFormula)
import MyCode.VariableCollection (collectVariables)
import MyCode.PrintCNF (cadicalPrint, humanFriendlyPrint)

import qualified MyCode.Utils as U

import Test.Naive (naiveTest)

import qualified Help as H


-------------------------------------------------------------------------------
compileToCNF :: String -> Err ([[Formula]], S.Set Ident)
compileToCNF input = do
  formula <- (pFormula . myLexer) input
  let varSet = collectVariables formula
  cnfForm <- (toCNF . pushNegationDeep . disassembleDeep) formula
  return (cnfForm, varSet)

-------------------------------------------------------------------------------
toHumanFriendly :: String -> Err String
toHumanFriendly input = do
  (cnfForm, varSet) <- compileToCNF input
  toPrint <- humanFriendlyPrint varSet cnfForm 5

  let headline = "input: " ++ input
  return $ U.paste [headline, toPrint] "\n" 

toCadical :: String -> Err String
toCadical input = do
  (cnfForm, varSet) <- compileToCNF input
  toPrint <- cadicalPrint varSet cnfForm 5
  return toPrint

testN :: String -> Err String
testN input = do
  formula <- (pFormula . myLexer) input
  case naiveTest formula of
    Ok () -> return "Ok"
    Bad s -> return s


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
getErr :: Err a -> IO a
getErr (Ok a) = return a
getErr (Bad s) = fail s


main :: IO ()
main = do
  args <- getArgs
  let defaultInput = fail "No input"
  let defaultPrintFunc = toCadical
  let defaultWriteFunc = putStrLn
  
  --(input, printFunc, writeFunc) <- 
  --  dealWithArgs (defaultInput, defaultPrintFunc, defaultWriteFunc, False) args
  --
  --execute input printFunc writeFunc
  dealWithArgs (defaultInput, defaultPrintFunc, defaultWriteFunc) args


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
dealWithArgs :: (IO String, String -> Err String, String -> IO ()) -> 
  [String] -> IO ()
  --[String] -> IO (IO String, String -> Err String, String -> IO (), Bool)

dealWithArgs (input, printFunc, writeFunc) args = case args of
    [] -> execute input printFunc writeFunc

    first:rest1 -> case first of
      '-':'-':flag -> case flag of
        "human-friendly" -> 
          dealWithArgs (input, toHumanFriendly, writeFunc) rest1
        "human" -> dealWithArgs (input, toHumanFriendly, writeFunc) rest1
        "friendly" -> dealWithArgs (input, toHumanFriendly, writeFunc) rest1

        "cadical" -> dealWithArgs (input, toCadical, writeFunc) rest1
        
        "help" -> putStrLn H.helpMsg

        "test" -> do
          putStrLn "Warning: naive testing has exponential time complexity"
          dealWithArgs (input, testN, writeFunc) rest1

        _ -> fail $ "Unknown option: --" ++ flag


      '-':flag -> case flag of
        "o" -> case rest1 of
          [] -> fail "flag '-o' must have a directory specified."
          outDir:rest2 -> 
            dealWithArgs (input, printFunc, writeFile outDir) rest2

        "f" -> case rest1 of
          [] -> fail "flag '-f' must have a directory specified."
          inDir:rest2 -> 
            dealWithArgs (readFile inDir, printFunc, writeFunc) rest2

        _ -> fail $ "Unknown option: -" ++ flag


      whatever -> 
        dealWithArgs (return whatever, printFunc, writeFunc) rest1

execute :: IO String -> (String -> Err String) -> (String -> IO ()) -> IO ()
execute ioInput printFunc writeFunc = do

    input <- ioInput
    toWrite <- getErr $ printFunc input
    writeFunc toWrite


-------------------------------------------------------------------------------


