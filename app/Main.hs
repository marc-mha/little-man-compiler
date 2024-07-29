module Main where

import CodeGener (generateProgram)
import Data.ByteString (writeFile)
import Parser
  ( PInstruction (..),
    POperand (..),
    POperation (..),
    parseProgram,
  )
import Resolver
  ( Address (Address),
    Addressed,
    ROperation (..),
    resolveProgram,
  )
import System.Environment (getArgs)

getFilename :: IO (String, String)
getFilename = do
  args <- getArgs
  case args of
    [] -> error "Input file expected."
    [_] -> error "Output file expected."
    (inp : out : _) -> return (inp, out)

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
  (inputfile, outputfile) <- getFilename
  source <- readFile inputfile
  let compiled =
        parseProgram source
          >>= resolveProgram
          >>= generateProgram
  maybe
    (putStrLn "An error occurred when compiling.")
    (\bytecode -> Data.ByteString.writeFile outputfile bytecode >> putStrLn "Compiled successfully!")
    compiled
