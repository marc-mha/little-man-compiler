{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Applicative
import Data.Maybe (catMaybes)
import ParserLib

-- Type aliases for labels and addresses
type Label = String

data Instruction = Labeled Label Operation | Unlabeled Operation deriving (Show)

data Operation = Argumented Operator Operand | Unargumented Operator deriving (Show)

data Operator
  = HLT
  | ADD
  | SUB
  | STA
  | LDA
  | BRA
  | BRZ
  | BRP
  | INP
  | OUT
  | DAT
  deriving (Show)

operatorStrings :: [(String, Operator)]
operatorStrings =
  [ ("HLT", HLT),
    ("ADD", ADD),
    ("SUB", SUB),
    ("STA", STA),
    ("LDA", LDA),
    ("BRA", BRA),
    ("BRZ", BRZ),
    ("BRP", BRP),
    ("INP", INP),
    ("OUT", OUT),
    ("DAT", DAT)
  ]

data Operand
  = RefLabel Label
  | DirValue Int
  deriving (Show)

isnewline :: Char -> Bool
isnewline '\n' = True
isnewline _ = False

sepSpace :: Parser Char
sepSpace = satisfy isspace
  where
    isspace :: Char -> Bool
    isspace ' ' = True
    isspace '\t' = False
    isspace _ = False

parseProgram :: String -> Maybe [Instruction]
parseProgram = fmap (catMaybes . fst) . parse parseFile

parseFile :: Parser [Maybe Instruction]
parseFile = do
  result <-
    many $ do
      line <- parseLine
      satisfy isnewline
      return line
  eof
  return result

-- | Parses one line of source.
parseLine :: Parser (Maybe Instruction)
parseLine = do
  line <- optional parseSourceLine
  many sepSpace
  optional parseComment
  return line

parseSourceLine :: Parser Instruction
parseSourceLine = parseUnlabeledLine <|> parseLabeledLine

parseComment :: Parser String
parseComment = do
  string "//"
  many $ satisfy (not . isnewline)

parseLabeledLine :: Parser Instruction
parseLabeledLine = do
  label <- parseLabel
  char ':' <|> space
  many space
  operation <- parseOperation
  return (Labeled label operation)

parseUnlabeledLine :: Parser Instruction
parseUnlabeledLine = do
  some sepSpace
  operation <- parseOperation
  return (Unlabeled operation)

parseLabel :: Parser Label
parseLabel = do
  l <- letter
  abel <- many alphanum
  return (l : abel)

parseOperation :: Parser Operation
parseOperation =
  do
    operator <- parseOperator
    ( do
        some sepSpace
        operand <- parseOperand
        return (Argumented operator operand)
      )
      <|> return (Unargumented operator)

parseOperator :: Parser Operator
parseOperator = asum $ map toOp operatorStrings
  where
    toOp (s, o) = o <$ string s

parseValue :: Parser Int
parseValue = read <$> some digit

parseOperand :: Parser Operand
parseOperand = (RefLabel <$> parseLabel) <|> (DirValue <$> parseValue)
