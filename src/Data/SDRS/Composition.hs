{- |
Module      :  Data.SDRS.Composition
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS composition
-}

module Data.SDRS.Composition
(
  buildFromDRSs
, addDRS
) where

import Data.SDRS.DataType
import qualified Data.Map as M

---------------------------------------------------------------------------
-- | Build new SDRS using two DRSs and their relation
---------------------------------------------------------------------------
buildFromDRSs :: DRS -> Label -> DRS -> SDRS
buildFromDRSs d1 label d2 = SDRS (M.fromList [(0, Relation label 1 2),
                                              (1, Segment d1),
                                              (2, Segment d2)]) 2

addDRS :: SDRS -> DRS -> [(DisVar, Label)] -> SDRS
addDRS (SDRS m _) d edges = SDRS (updatedMap) updatedLast
  where updatedLast = (fst $ M.findMax m) + 1
        updatedMap = M.insert updatedLast (Segment d) (updateRelations edges m updatedLast)
        -- so far only simple case with no recursive Relations
        updateRelations :: [(DisVar, Label)] -> M.Map DisVar SDRSFormula -> DisVar -> M.Map DisVar SDRSFormula
        updateRelations [] m' _                      = m'
        updateRelations ((dv, label):rest) m' maxKey = M.insert (maxKey + 1)
                                                                (Relation label dv updatedLast)
                                                                (updateRelations rest m' (maxKey + 1))


