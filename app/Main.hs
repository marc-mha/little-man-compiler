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

printOpand :: POperand -> String
printOpand (RefLabel l) = l
printOpand (DirValue v) = show v

printOper :: POperation -> String
printOper (PArgumented operator operand) = show operator ++ " " ++ printOpand operand
printOper (PUnargumented operator) = show operator

printInst :: PInstruction -> String
printInst (PLabeled l o) = l ++ ":\t" ++ printOper o
printInst (PUnlabeled o) = "\t" ++ printOper o

main :: IO ()
main = do
  filename <- getFilename
  source <- readFile filename
  putStrLn ((unlines . map printInst) (fromJust (parseProgram source)))
