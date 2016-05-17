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
  --sdrsMerge
) where

--import qualified Data.Map as M
--import Data.List (intersect)
----import Debug.Trace
----import Data.SDRS.Show()

--import Data.SDRS.DataType
--import Data.SDRS.Structure
--import Data.SDRS.DiscourseGraph
--import Data.SDRS.LambdaCalculus
--import Data.SDRS.Composition (updateRelations)

--import Data.DRS.Structure (drsUniverse)

---------------------------------------------------------------------------
-- | Applies merge to 'SDRS' @s1@ and 'SDRS' @s2@. The latter is attached
-- with its root node to a node @dv1@ that must be on the RF of @s1@, using relation @r@.  
---------------------------------------------------------------------------
--sdrsMerge :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
--sdrsMerge s1@(SDRS m1 _) s2 edges = SDRS mMergedWithNewRelation (sdrsLast s2DRConv) 
--  where convMap = buildConvMap s1 s2
--        s2DVConv = sdrsAlphaConvert s2 convMap
--        s2DRConv = sdrsDRSRefsAlphaConvert s2DVConv drsRefConvMap
--        mMerged = m1 `M.union` (sdrsMap s2DRConv)
--        attachingNode = root s2DVConv
--        updatedOutscope = (fst $ M.findMax mMerged) + 1
--        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefOverlap
--        drsRefs1 = concat $ map drsUniverse $ drss s1
--        drsRefs2 = concat $ map drsUniverse $ drss s2
--        drsRefOverlap = drsRefs1 `intersect` drsRefs2 -- order?
--        mMergedWithNewRelation = updateRelations s1 edges mMerged attachingNode updatedOutscope

---------------------------------------------------------------------------
-- | Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Strict merge. Preliminary version, depends on implementation of
-- isWellformedSDRS
---------------------------------------------------------------------------
--sdrsMergeStrict :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
--sdrsMergeStrict s1 s2 edges
--  | isWellformedSDRS s1 &&
--    isWellformedSDRS s2 = sdrsMerge s1 s2 edges
--  | otherwise           = error "The merge SDRSs are not wellformed."


