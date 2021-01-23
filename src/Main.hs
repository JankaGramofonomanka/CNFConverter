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
  toPrint <- humanFriendlyPrint varSet cnfForm 4

  let headline = "input: " ++ input
  return $ U.paste [headline, toPrint] "\n" 

toCadical :: String -> Err String
toCadical input = do
  (cnfForm, varSet) <- compileToCNF input
  toPrint <- cadicalPrint varSet cnfForm 4
  return toPrint

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
  
  (input, printFunc, writeFunc, dispHelp) <- 
    dealWithArgs (defaultInput, defaultPrintFunc, defaultWriteFunc, False) args

  execute input printFunc writeFunc dispHelp

-------------------------------------------------------------------------------
dealWithArgs :: (IO String, String -> Err String, String -> IO (), Bool) -> 
  [String] -> IO (IO String, String -> Err String, String -> IO (), Bool)

dealWithArgs (input, printFunc, writeFunc, dispHelp) args = case args of
    [] -> return (input, printFunc, writeFunc, dispHelp)

    first:rest1 -> case first of
      '-':'-':flag -> case flag of
        "human-friendly" -> 
          dealWithArgs (input, toHumanFriendly, writeFunc, dispHelp) rest1

        "cadical" -> 
          dealWithArgs (input, toCadical, writeFunc, dispHelp) rest1
        
        "help" -> return (input, printFunc, writeFunc, True)


      '-':flag -> case flag of
        "o" -> case rest1 of
          [] -> fail "flag '-o' must have a directory specified."
          outDir:rest2 -> 
            dealWithArgs (input, printFunc, writeFile outDir, dispHelp) rest2

        "f" -> case rest1 of
          [] -> fail "flag '-f' must have a directory specified."
          inDir:rest2 -> 
            dealWithArgs (readFile inDir, printFunc, writeFunc, dispHelp) rest2

        _ -> fail $ "Unknown flag: -" ++ flag


      whatever -> 
        dealWithArgs (return whatever, printFunc, writeFunc, dispHelp) rest1

execute :: IO String -> (String -> Err String) -> (String -> IO ()) -> 
  Bool -> IO ()
execute ioInput printFunc writeFunc dispHelp = if dispHelp then 
    putStrLn helpMsg

  else 
    do

    input <- ioInput
    toWrite <- getErr $ printFunc input
    writeFunc toWrite


-------------------------------------------------------------------------------
progName = "prog"

helpMsg = U.paste lines "\n" where
  lines = [
    "Convert a quantifier free formula to CNF form",
    "Usage: ./" ++ progName ++ " <options> [input]",
    "",
    "no need to provide the input, if the option '-f [DIR]' is provided",
    "input form:",
    "  variables can be any strings beginning with a letter,",
    "  availible operators:",
    "    not",
    "    and",
    "    or",
    "    xor",
    "    ==>",
    "    <==",
    "    <=>",
    "",
    "options:",
    "  -o [DIR]                     write output to DIR",
    "  -f [DIR]                     read input from DIR",
    "  --human-friendly             display output in human friendly form",
    "  --cadical          (default) display output in a form readable by the",
    "                               'cadical' sat solver",
    "  --help                       display help"]

