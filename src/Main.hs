module Main where


import System.Environment (getArgs)

import qualified Data.Set as S

import FromBNFC.AbsQFBF
import FromBNFC.ErrM
import FromBNFC.ParQFBF (myLexer, pFormula)
import FromBNFC.PrintQFBF (render, prt)

import MyCode.Disassemble (disassembleDeep)
import MyCode.PushNegation (pushNegationDeep)
import MyCode.ToCNF (toCNF, backToFormula)
import MyCode.VariableCollection (collectVariables)
import MyCode.PrintCNF (cadicalPrint, humanFriendlyPrint)

import qualified MyCode.Utils as U

import Test.Naive (naiveTest)

import qualified Help as H


-------------------------------------------------------------------------------
compileToCNF :: Formula -> Err [[Formula]]
compileToCNF = toCNF . pushNegationDeep . disassembleDeep

-------------------------------------------------------------------------------
toHumanFriendly :: Formula -> Err String
toHumanFriendly formula = do
  cnfForm <- compileToCNF formula
  let varSet = collectVariables formula
  toPrint <- humanFriendlyPrint varSet cnfForm 5

  let headline = "input: " ++ (render $ prt 0 formula)
  return $ U.paste [headline, toPrint] "\n" 

toCadical :: Formula -> Err String
toCadical formula = do
  cnfForm <- compileToCNF formula
  let varSet = collectVariables formula
  toPrint <- cadicalPrint varSet cnfForm 5
  return toPrint

testN :: Formula -> Err String
testN formula = case naiveTest formula of
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
  
  --(input, middleFunc, writeFunc) <- 
  --  dealWithArgs (defaultInput, defaultPrintFunc, defaultWriteFunc, False) args
  --
  --execute input middleFunc writeFunc
  dealWithArgs defaultInput defaultPrintFunc defaultWriteFunc False args


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
dealWithArgs :: IO String -> (Formula -> Err String) -> (String -> IO ()) -> 
  Bool -> [String] -> IO ()
  --[String] -> IO (IO String, String -> Err String, String -> IO (), Bool)

dealWithArgs input middleFunc writeFunc contradict args = case args of
    [] -> execute input middleFunc writeFunc contradict

    first:rest1 -> case first of
      '-':'-':flag -> case flag of
        "human-friendly" -> 
          dealWithArgs input toHumanFriendly writeFunc contradict rest1
        "human" -> 
          dealWithArgs input toHumanFriendly writeFunc contradict rest1
        "friendly" -> 
          dealWithArgs input toHumanFriendly writeFunc contradict rest1

        "contradict" -> 
          dealWithArgs input middleFunc writeFunc True rest1

        "cadical" -> dealWithArgs input toCadical writeFunc contradict rest1
        
        "help" -> putStrLn H.helpMsg

        "test" -> do
          putStrLn "Warning: naive testing has exponential time complexity"
          dealWithArgs input testN writeFunc contradict rest1

        _ -> fail $ "Unknown option: --" ++ flag


      '-':flag -> case flag of
        "o" -> case rest1 of
          [] -> fail "flag '-o' must have a directory specified."
          outDir:rest2 -> 
            dealWithArgs input middleFunc (writeFile outDir) contradict rest2

        "f" -> case rest1 of
          [] -> fail "flag '-f' must have a directory specified."
          inDir:rest2 -> 
            dealWithArgs (readFile inDir) middleFunc writeFunc contradict rest2

        _ -> fail $ "Unknown option: -" ++ flag


      whatever -> 
        dealWithArgs (return whatever) middleFunc writeFunc contradict rest1

execute :: IO String -> (Formula -> Err String) -> (String -> IO ()) -> 
  Bool -> IO ()
execute ioInput middleFunc writeFunc contradict = do

    input <- ioInput
    inputFormula <- getErr $ pFormula $ myLexer input

    let formula = if contradict then Not inputFormula else inputFormula

    toWrite <- getErr $ middleFunc formula
    writeFunc toWrite


-------------------------------------------------------------------------------


