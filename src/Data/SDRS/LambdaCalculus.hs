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
  sdrsAlphaConvertDRS
, sdrsAlphaConvertDRSs
, buildDRSRefConvMap
, buildDisVarConvMap
) where

import Data.List (union, insert, intersect)
import qualified Data.Map as M

import Data.DRS.LambdaCalculus (renameSubDRS)
import Data.DRS.Variables (increase)
import Data.DRS.Merge ((<<+>>))

import Data.SDRS.DataType
import Data.SDRS.DiscourseStructure

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- ** Alpha Conversion
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | For an 'SDRS' @s@, a 'DisVar' @dv@ and a 'DRSRef' conversion map @cm@,
-- DRS-alpha-converts all 'DRSs' labeled by @dv@.
---------------------------------------------------------------------------
sdrsAlphaConvertDRS :: SDRS -> DisVar -> [(DRSRef,DRSRef)] -> SDRS
sdrsAlphaConvertDRS s@(SDRS m l) dv cm = SDRS m' l
  where m' = M.adjustWithKey conv dv m
        conv :: DisVar -> SDRSFormula -> SDRSFormula
        conv key (EDU d) = EDU $ renameSubDRS d gd' cm
          where gd' = foldl (<<+>>) (DRS [] []) accDRSs'
                accDRSs' = accessibleDRSs s key
        conv _ sf        = sf
        -- ^ the SDRSFormula labeled by @key@ was not a EDU, skip it.

---------------------------------------------------------------------------
-- | For an 'SDRS' @s@, a list of 'DisVar's @dvs@ and a 'DRSRef' conversion map
-- @cm@, DRS-alpha-converts all 'DRSs' labeled by the 'DisVar's in @dvs@.
---------------------------------------------------------------------------
sdrsAlphaConvertDRSs :: SDRS -> [DisVar] -> [(DRSRef,DRSRef)] -> SDRS
sdrsAlphaConvertDRSs s [] _         = s
sdrsAlphaConvertDRSs s (dv:rest) cm = sdrsAlphaConvertDRSs (sdrsAlphaConvertDRS s dv cm) rest cm

---------------------------------------------------------------------------
-- | Given a list of all present 'DRSRef's @rs@ and a list of overlapping
-- 'DRSRef's, builds a conversion list.
---------------------------------------------------------------------------
buildDRSRefConvMap :: [DRSRef] -> [DRSRef] -> [(DRSRef,DRSRef)]
buildDRSRefConvMap rs1 rs2 = build rsUnion rsOverlap
  where rsOverlap = rs1 `intersect` rs2
        rsUnion = rs1 `union` rs2
        build :: [DRSRef] -> [DRSRef] -> [(DRSRef,DRSRef)]
        build _ []          = []
        build rs (ref:rest) = (ref,newRef) : buildDRSRefConvMap (newRef:rs) rest
          where newRef = checkRef $ incrRef ref
                incrRef :: DRSRef -> DRSRef
                incrRef (DRSRef r) = DRSRef $ increase r
                incrRef ldr = ldr
                checkRef :: DRSRef -> DRSRef
                checkRef dr
                  | dr `elem` rs = checkRef $ incrRef dr
                  | otherwise    = dr

---------------------------------------------------------------------------
-- | Builds a conversion map for all overlapping 'DisVar' from two 'SDRS's
-- mapping each duplicate instance to a new variable.
---------------------------------------------------------------------------
buildDisVarConvMap :: SDRS -> SDRS -> [(DisVar,DisVar)]
buildDisVarConvMap (SDRS m1 _) (SDRS m2 _) = M.assocs $ build M.empty s1Keys s2Keys
  where build :: M.Map DisVar DisVar -> [DisVar] -> [DisVar] -> M.Map DisVar DisVar
        build cm _ []       = cm
        build cm keys1 (dv:rest)
          | dv `elem` keys1 = build (M.insert dv newMax cm) (insert newMax keys1) rest
          | otherwise       = build cm keys1 rest
          where newMax = maximum keys1 + 1
        s1Keys = M.keys m1
        s2Keys = M.keys m2
