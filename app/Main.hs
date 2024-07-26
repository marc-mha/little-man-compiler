module Main where

import Data.Maybe (fromJust)
import Parser
import System.Environment (getArgs)

getFilename :: IO String
getFilename = do
  args <- getArgs
  case args of
    [] -> error "File name expected."
    (fn : _) -> return fn

printOpand :: Operand -> String
printOpand (RefLabel l) = l
printOpand (DirValue v) = show v

printOper :: Operation -> String
printOper (Argumented operator operand) = show operator ++ " " ++ printOpand operand
printOper (Unargumented operator) = show operator

printInst :: Instruction -> String
printInst (Labeled l o) = l ++ ":\t" ++ printOper o
printInst (Unlabeled o) = "\t" ++ printOper o

main :: IO ()
main = do
  filename <- getFilename
  source <- readFile filename
  putStrLn ((unlines . map printInst) (fromJust (parseProgram source)))
