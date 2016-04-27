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

---------------------------------------------------------------------------
-- | Applies merge to 'SDRS' @s1@ and 'SDRS' @s2@. The latter is attached
-- with node @dv2@ to @dv1@ of @s1@, using relation @r@.  
-- TODO do wellformedness checks before merge?
---------------------------------------------------------------------------
sdrsMerge :: SDRS -> SDRS -> SDRSRelation -> SDRS
sdrsMerge s1@(SDRS m1 _) s2@(SDRS m2 _) rel = SDRS mergedWithNewRelation (sdrsLast s2_conv) 
  where buildConvMap :: M.Map DisVar DisVar -> [DisVar] -> [DisVar] -> M.Map DisVar DisVar
        buildConvMap cm _ [] = cm
        buildConvMap cm acc (dv:rest)
          | dv `elem` acc = buildConvMap (M.insert dv newMax cm) (insert newMax acc) rest
          | otherwise     = buildConvMap cm acc rest
          where newMax = maximum acc + 1
        convMap = buildConvMap M.empty (M.keys m1) (M.keys m2)
        s2_conv = sdrsAlphaConvert s2 convMap
        merged = m1 `M.union` (sdrsMap s2_conv)
        mergedWithNewRelation = M.insert ((fst $ M.findMax merged) + 1) (Relation rel (root s1 !! 0) (root s2_conv !! 0)) merged

