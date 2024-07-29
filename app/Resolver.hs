module Resolver where

import Control.Applicative (liftA2)
import Data.Bifunctor (Bifunctor (first))
import Data.Maybe (mapMaybe)
import Parser
  ( Label,
    PInstruction (..),
    POperand (DirValue, RefLabel),
    POperation (..),
    POperator,
  )

newtype Address = Address {unAddress :: Int} deriving (Show)

type Addressed a = (a, Address)

data ROperation = RArgumented POperator Address | RUnargumented POperator deriving (Show)

address :: [PInstruction] -> [Addressed PInstruction]
address = flip zip (Address <$> [1 ..])

ripLabels :: [Addressed PInstruction] -> [Addressed Label]
ripLabels = mapMaybe getLabel
  where
    getLabel (PLabeled l _, i) = Just (l, i)
    getLabel _ = Nothing

stripLabels :: [Addressed PInstruction] -> ([Addressed Label], [Addressed POperation])
stripLabels = liftA2 (,) ripLabels (map (first pruneLabelMarkers))
  where
    pruneLabelMarkers (PLabeled _ o) = o
    pruneLabelMarkers (PUnlabeled o) = o

suck :: (Maybe a, b) -> Maybe (a, b)
suck (ma, b) = (,) <$> ma <*> pure b

resolveLabel :: [Addressed Label] -> POperation -> Maybe ROperation
resolveLabel labels (PArgumented op (RefLabel l)) = RArgumented op <$> lookup l labels
resolveLabel _ (PArgumented op (DirValue v)) = pure $ RArgumented op $ Address v
resolveLabel _ (PUnargumented op) = pure $ RUnargumented op

resolveLabels :: [Addressed Label] -> [Addressed POperation] -> Maybe [Addressed ROperation]
resolveLabels labels = mapM (suck . first (resolveLabel labels))

resolveProgram :: [PInstruction] -> Maybe [Addressed ROperation]
resolveProgram = uncurry resolveLabels . stripLabels . address
