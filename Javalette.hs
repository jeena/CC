import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix

import AbsJavalette
import LexJavalette
import ParJavalette
import ErrM
import PrintJavalette

import TypeChecker
import Compiler


check :: String -> IO () 
check s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure 
                          Ok at    -> do --putStrLn $ printTree at ++ "OK\n"
                                         comp at
                                         
comp :: Program -> IO ()
comp p = do a <- getArgs
            putStrLn $ compile p $ takeBaseName $ head a

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= check
            _      -> do putStrLn "Usage: lab2 <SourceFile>"
                         exitFailure
