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
  renameDisVars
, sdrsAlphaConvertDRS
, sdrsAlphaConvertDRSs
--, normalize
, buildDRSRefConvMap
, buildConvMap
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
-- | Renames all embedded 'DisVar's of a given 'SDRS' on the basis of a
-- conversion list.
---------------------------------------------------------------------------
renameDisVars :: SDRS -> [(DisVar,DisVar)] -> SDRS
renameDisVars (SDRS m l) cm = SDRS (M.fromList (convert' (M.assocs m) cmMap)) (M.findWithDefault l l cmMap)
  where cmMap = M.fromList cm
        convert' :: [(DisVar, SDRSFormula)] -> M.Map DisVar DisVar -> [(DisVar, SDRSFormula)]
        convert' [] _         = []
        convert' (t:rest) cm' = (convertTuple t cm') : (convert' rest cm')
        convertTuple :: (DisVar, SDRSFormula) -> M.Map DisVar DisVar -> (DisVar, SDRSFormula)
        convertTuple (dv, sf@(EDU _)) cm' = (M.findWithDefault dv dv cm', sf)
        convertTuple (dv, (CDU cdu)) cm'  = (M.findWithDefault dv dv cm', CDU $ convertCDU cdu cm')
        convertCDU :: CDU -> M.Map DisVar DisVar -> CDU
        convertCDU (Relation rel dv1 dv2) cm' = Relation rel (M.findWithDefault dv1 dv1 cm') (M.findWithDefault dv2 dv2 cm')
        convertCDU (And cdu1 cdu2) cm'        = And (convertCDU cdu1 cm') (convertCDU cdu2 cm')
        convertCDU (Not cdu1) cm'             = Not (convertCDU cdu1 cm')

---------------------------------------------------------------------------
-- | For an 'SDRS' @s@, a 'DisVar' @dv@ and a DRSRef conversion map @cm@,
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
-- | For an 'SDRS' @s@, a list of 'DisVar's @dvs@ and a DRSRef conversion map
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
buildConvMap :: SDRS -> SDRS -> [(DisVar,DisVar)]
buildConvMap (SDRS m1 _) (SDRS m2 _) = M.assocs $ build M.empty s1Keys s2Keys
  where build :: M.Map DisVar DisVar -> [DisVar] -> [DisVar] -> M.Map DisVar DisVar
        build cm _ []       = cm
        build cm keys1 (dv:rest)
          | dv `elem` keys1 = build (M.insert dv newMax cm) (insert newMax keys1) rest
          | otherwise       = build cm keys1 rest
          where newMax = maximum keys1 + 1
        s1Keys = M.keys m1
        s2Keys = M.keys m2

---------------------------------------------------------------------------
-- | Normalizes the nodes in the 'SDRS' @s@, i.e., maps the @s@'s 'DisVar's
-- to a sequence from 0 to n, where n is equal to the number of unique 'DisVar's
-- in the 'SDRS'.
-- Depends on discourseGraph, but not crucial to fix atm
---------------------------------------------------------------------------
--normalize :: SDRS -> SDRS
--normalize s = renameDisVars s normMap
--  where build :: [DisVar] -> DisVar -> M.Map DisVar DisVar -> M.Map DisVar DisVar
--        build [] _ cm'                     = cm'
--        build (cur:rest) index cm' 
--          | ((M.lookup cur g) == Nothing) = M.insert cur index (build rest (index + 1) cm')
--          | otherwise                     = M.insert cur index (build (rest `union` (map fst $ g M.! cur)) (index + 1) cm')
--        normMap = build [root s] 0 M.empty
--        g = discourseGraph s
