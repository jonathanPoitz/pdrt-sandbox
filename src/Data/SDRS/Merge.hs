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

import qualified Data.Map as M
import Data.List (intersect)
-- import Debug.Trace
-- import Data.SDRS.Show()

import Data.SDRS.DataType
import Data.SDRS.Structure
import Data.SDRS.DiscourseGraph
import Data.SDRS.LambdaCalculus
import Data.SDRS.Composition (updateRelations)

import Data.DRS.Structure (drsUniverse)
import Data.DRS.LambdaCalculus (renameSubDRS)
import Data.DRS.Merge ((<<+>>))

-------------------------------------------------------------------------
-- | Applies merge to 'SDRS' @s1@ and 'SDRS' @s2@. The latter is attached
-- with its root node to a node @dv1@ that must be on the RF of @s1@, using relation @r@.  
-------------------------------------------------------------------------
sdrsMerge :: SDRS -> SDRS -> [(DisVar,SDRSRelation)] -> SDRS
sdrsMerge s1@(SDRS m1 _) s2 edges = sdrsDRSRefAlphaConved
  where convMap = buildConvMap s1 s2 -- 1.
        drsRefs1 = concat $ map drsUniverse $ accDRSs -- 3a.
        drsRefs2 = concat $ map drsUniverse $ drss s2DVConv -- 3b.
        drsRefOverlap = drsRefs1 `intersect` drsRefs2 -- order? 3c.
        drsRefConvMap = buildDRSRefConvMap drsRefs1 drsRefOverlap -- 3d.
        accDRSs = accessibleDRSs sdrsMerged attachingNode
        updatedLast = sdrsLast s2DVConv
        attachingNode = root s2DVConv
        updatedOutscope = (max (fst $ M.findMax m1) (fst $ M.findMax (sdrsMap s2DVConv))) + 1
        -----
        s2DVConv = sdrsAlphaConvert s2 convMap -- 2.
        s1WithNewRelation = updateRelations s1 edges attachingNode updatedOutscope
        sdrsMerged = SDRS ((sdrsMap s1WithNewRelation) `M.union` (sdrsMap s2DVConv)) updatedLast -- merged maps and updated last
        newDRSKeys = map fst $ segments s2DVConv -- 2a. the labels of the drss that are to be added. these later need to be drsRefAlphaConv'ed
        sdrsDRSRefAlphaConved = alphaConvDRSs sdrsMerged newDRSKeys
        alphaConvDRSs :: SDRS -> [DisVar] -> SDRS
        alphaConvDRSs s []                 = s
        alphaConvDRSs (SDRS m l) (dv:rest) = alphaConvDRSs (SDRS m' l) rest
          where m' = M.adjustWithKey alphaConvDRS dv m
                alphaConvDRS :: DisVar -> SDRSFormula -> SDRSFormula
                alphaConvDRS key (Segment d) = Segment $ renameSubDRS d gd' drsRefConvMap
                  where gd' = foldl (<<+>>) (DRS [] []) accDRSs'
                        accDRSs' = accessibleDRSs sdrsMerged key
                alphaConvDRS _ sf          = sf -- other cases don't matter currently
        

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


