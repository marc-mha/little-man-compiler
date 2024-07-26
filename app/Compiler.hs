module Compiler where

import Data.Bifunctor (Bifunctor (first))
import Parser

newtype Address = Address Int deriving (Show)

type Addressed a = (a, Address)

data SOperation = SArgumented POperator Address | SUnargumented POperator deriving (Show)

address :: [PInstruction] -> [Addressed PInstruction]
address = flip zip (Address <$> [1 ..])

ripLabels :: [Addressed PInstruction] -> [Addressed Label]
ripLabels [] = []
ripLabels ((inst, i) : xs) = case inst of
  PLabeled l _ -> (l, i) : ripLabels xs
  _ -> ripLabels xs

pruneLabelMarkers :: PInstruction -> POperation
pruneLabelMarkers (PLabeled _ o) = o
pruneLabelMarkers (PUnlabeled o) = o

suck :: (Maybe a, b) -> Maybe (a, b)
-- suck (Just a, b) = Just (a, b)
-- suck (Nothing, _) = Nothing
suck (ma, b) = (,) <$> ma <*> pure b

resolveLabel :: [Addressed Label] -> POperation -> Maybe SOperation
resolveLabel labels (PArgumented op (RefLabel l)) = SArgumented op <$> lookup l labels
resolveLabel _ (PArgumented op (DirValue v)) = pure $ SArgumented op $ Address v
resolveLabel _ (PUnargumented op) = pure $ SUnargumented op

resolveLabels :: [Addressed Label] -> [Addressed POperation] -> Maybe [Addressed SOperation]
resolveLabels labels = mapM (suck . first (resolveLabel labels))

semant :: [PInstruction] -> Maybe [Addressed SOperation]
semant program =
  let taggedProgram = address program
      labels = ripLabels taggedProgram
      prunedProgram = map (first pruneLabelMarkers) taggedProgram
      resolvedProgram = resolveLabels labels prunedProgram
   in resolvedProgram
