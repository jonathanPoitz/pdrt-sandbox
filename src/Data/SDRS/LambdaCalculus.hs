{- |
Module      :  Data.SDRS.LambdaCalculus
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

Lambda Calculus for SDRT
-}

module Data.SDRS.LambdaCalculus
(
  sdrsAlphaConvert
, normalize
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.List (union)
import Data.SDRS.DiscourseGraph

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- ** Alpha Conversion
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Applies alpha conversion to an 'SDRS' on the basis of a conversion list
-- for all embedded 'DisVar's.
---------------------------------------------------------------------------
sdrsAlphaConvert :: SDRS -> M.Map DisVar DisVar -> SDRS
sdrsAlphaConvert (SDRS m l) convMap = SDRS (M.fromList (convert' (M.assocs m) convMap)) (M.findWithDefault l l convMap)
  where convert' :: [(DisVar, SDRSFormula)] -> M.Map DisVar DisVar -> [(DisVar, SDRSFormula)]
        convert' [] _           = []
        convert' (t:rest) nm    = (convertTuple t nm) : (convert' rest nm)
        convertTuple :: (DisVar, SDRSFormula) -> M.Map DisVar DisVar -> (DisVar, SDRSFormula)
        convertTuple (dv, sf) nm = (M.findWithDefault dv dv nm, convertSF sf nm)
        convertSF :: SDRSFormula -> M.Map DisVar DisVar -> SDRSFormula
        convertSF d@(Segment _) _             = d
        convertSF (Relation rel dv1 dv2) nm = Relation rel (M.findWithDefault dv1 dv1 nm) (M.findWithDefault dv2 dv2 nm)
        convertSF (And sf1 sf2) nm            = And (convertSF sf1 nm) (convertSF sf2 nm)
        convertSF (Not sf1) nm                = Not (convertSF sf1 nm)

---------------------------------------------------------------------------
-- | normalizes the nodes in an SDRS.
---------------------------------------------------------------------------
normalize :: SDRS -> SDRS
normalize s = sdrsAlphaConvert s normMap
  where build :: [DisVar] -> DisVar -> M.Map DisVar DisVar -> M.Map DisVar DisVar
        build [] _ nm                     = nm
        build (cur:rest) index nm 
          | ((M.lookup cur g) == Nothing) = M.insert cur index (build rest (index + 1) nm)
          | otherwise                     = M.insert cur index (build (rest `union` (map fst $ g M.! cur)) (index + 1) nm)
        normMap = build (root s) 0 M.empty
        g = discourseGraph s


---------------------------------------------------------------------------
-- | Given two 'SDRS's, @s1@ and @s2@, builds up the alpha conversion map
-- from the 'DisVar's of @s1@ to its possible counterparts in @s2@.
---------------------------------------------------------------------------
--buildConversionMap :: SDRS -> SDRS -> M.Map DisVar DisVar
--buildConversionMap s1@(SDRS m1 l1) s2@(SDRS m2 l2) = convMap
--  where convMap = build g1 g2
--        g1 = discourseGraph s1
--        g2 = discourseGraph s2
--        build :: M.Map DisVarc

-- two SDRS are structurally struc_isomorph if
-- -- build up conversion map from sdrs1 to 2
-- -- 

-- 2 sdrss are not structurally isomorphic if:
-- maps have different number of entries
-- 



