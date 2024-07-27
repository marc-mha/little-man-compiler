{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use <$>" #-}

module Parser
  ( Label,
    PInstruction (..),
    POperation (..),
    POperator (..),
    POperand (..),
    parseProgram,
  )
where

import Control.Applicative
  ( Alternative (many, some, (<|>)),
    asum,
    optional,
  )
import Data.Maybe (catMaybes)
import ParserLib
  ( Parser,
    alphanum,
    char,
    digit,
    eof,
    letter,
    parse,
    satisfy,
    space,
    string,
  )

-- Type aliases for labels and addresses
type Label = String

data PInstruction = PLabeled Label POperation | PUnlabeled POperation deriving (Show)

data POperation = PArgumented POperator POperand | PUnargumented POperator deriving (Show)

data POperator
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

operatorStrings :: [(String, POperator)]
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

data POperand
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

parseProgram :: String -> Maybe [PInstruction]
parseProgram = fmap (catMaybes . fst) . parse parseFile

parseFile :: Parser [Maybe PInstruction]
parseFile = do
  result <-
    many $ do
      line <- parseLine
      satisfy isnewline
      return line
  eof
  return result

-- | Parses one line of source.
parseLine :: Parser (Maybe PInstruction)
parseLine = do
  line <- optional parseSourceLine
  many sepSpace
  optional parseComment
  return line

parseSourceLine :: Parser PInstruction
parseSourceLine = parseUnlabeledLine <|> parseLabeledLine

parseComment :: Parser String
parseComment = do
  string "//"
  many $ satisfy (not . isnewline)

parseLabeledLine :: Parser PInstruction
parseLabeledLine = do
  label <- parseLabel
  char ':' <|> space
  many space
  operation <- parseOperation
  return (PLabeled label operation)

parseUnlabeledLine :: Parser PInstruction
parseUnlabeledLine = do
  some sepSpace
  operation <- parseOperation
  return (PUnlabeled operation)

parseLabel :: Parser Label
parseLabel = do
  l <- letter
  abel <- many alphanum
  return (l : abel)

_parseOperation :: Parser POperation
_parseOperation =
  do
    operator <- parseOperator
    ( do
        some sepSpace
        operand <- parseOperand
        return (PArgumented operator operand)
      )
      <|> return (PUnargumented operator)

parseOperation :: Parser POperation
parseOperation = do
  operator <- parseOperator
  case operator of
    HLT -> return (PUnargumented operator)
    INP -> return (PUnargumented operator)
    OUT -> return (PUnargumented operator)
    DAT -> do
      operand <- optional (some sepSpace *> parseOperand)
      return $ maybe (PUnargumented operator) (PArgumented operator) operand
    _ -> do
      some sepSpace
      operand <- parseOperand
      return (PArgumented operator operand)

parseOperator :: Parser POperator
parseOperator = asum $ map toOp operatorStrings
  where
    toOp (s, o) = o <$ string s

parseValue :: Parser Int
parseValue = read <$> some digit

parseOperand :: Parser POperand
parseOperand = (RefLabel <$> parseLabel) <|> (DirValue <$> parseValue)
