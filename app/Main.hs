module Main where

import Data.Maybe (fromJust)
import Parser
  ( PInstruction (..),
    POperand (..),
    POperation (..),
    parseProgram,
  )
import Resolver
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

printAddr :: Address -> String
printAddr (Address v) = show v

printResolved :: Addressed ROperation -> String
printResolved (RArgumented operator operand, i) = printAddr i ++ ":\t" ++ show operator ++ " " ++ printAddr operand
printResolved (RUnargumented operator, i) = printAddr i ++ ":\t" ++ show operator

main :: IO ()
main = do
  filename <- getFilename
  source <- readFile filename
  putStrLn ((unlines . map printResolved) (fromJust (parseProgram source >>= resolveProgram)))
