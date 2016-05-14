{- |
Module      :  Data.SDRS.Merge
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS merge
-}

module Data.SDRS.Merge
(
  sdrsMerge
) where

import Data.SDRS.DataType
-- import Data.DRS.Properties
-- import Data.DRS.Merge
import qualified Data.Map as M
import Data.List (insert)
import Data.SDRS.Structure
import Data.SDRS.DiscourseGraph
import Data.SDRS.LambdaCalculus
import Data.SDRS.Composition (updateRelations)
--import Debug.Trace
--import Data.SDRS.Show()

---------------------------------------------------------------------------
-- | Applies merge to 'SDRS' @s1@ and 'SDRS' @s2@. The latter is attached
-- with its root node to a node @dv1@ that must be on the RF of @s1@, using relation @r@.  
---------------------------------------------------------------------------
sdrsMerge :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
sdrsMerge s1@(SDRS m1 _) s2 edges = SDRS mMergedWithNewRelation (sdrsLast s2_conv) 
  where convMap = buildConvMap s1 s2
        s2_conv = sdrsAlphaConvert s2 convMap
        mMerged = m1 `M.union` (sdrsMap s2_conv)
        attachingNode = root s2_conv
        updatedOutscope = (fst $ M.findMax mMerged) + 1
        mMergedWithNewRelation = updateRelations s1 edges mMerged attachingNode updatedOutscope

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Builds a conversion map for all overlapping 'DisVar' from 
---------------------------------------------------------------------------
buildConvMap :: SDRS -> SDRS -> M.Map DisVar DisVar
buildConvMap (SDRS m1 _) (SDRS m2 _) = build M.empty s1Keys s2Keys
  where build :: M.Map DisVar DisVar -> [DisVar] -> [DisVar] -> M.Map DisVar DisVar
        build cm _ []       = cm
        build cm keys1 (dv:rest)
          | dv `elem` keys1 = build (M.insert dv newMax cm) (insert newMax keys1) rest
          | otherwise       = build cm keys1 rest
          where newMax = maximum keys1 + 1
        s1Keys = M.keys m1
        s2Keys = M.keys m2


---------------------------------------------------------------------------
-- | Strict merge. Preliminary version, depends on implementation of
-- isWellformedSDRS
---------------------------------------------------------------------------
--sdrsMergeStrict :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
--sdrsMergeStrict s1 s2 edges
--  | isWellformedSDRS s1 &&
--    isWellformedSDRS s2 = sdrsMerge s1 s2 edges
--  | otherwise           = error "The merge SDRSs are not wellformed."


