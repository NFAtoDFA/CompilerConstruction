import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Exit (exitSuccess)

import Typechecker
import AbsKOTLIN
import LexKOTLIN
import ParKOTLIN
import ErrM
-- import SkelKOTLIN
import Environment


typechecker :: Program -> Typechecker.Result
typechecker x = typecheckProgram x

process :: String -> IO ()
process s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR:"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typechecker tree of
                      Ok s -> do    putStrLn s
                                    exitSuccess
                      Bad err -> do putStrLn "TYPE ERROR:"
                                    putStrLn err
                                    exitFailure

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= process
            _      -> getContents >>= process
