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
--, sdrsDRSRefsAlphaConvert
, sdrsAlphaConvertDRS
, sdrsAlphaConvertDRSs
, normalize
, buildDRSRefConvMap
, buildConvMap
) where

import Data.List (union, insert, intersect)
import qualified Data.Map as M

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph

import Data.DRS.LambdaCalculus (renameSubDRS)
import Data.DRS.Variables (increase)
import Data.DRS.Merge ((<<+>>))

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- ** Alpha Conversion
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Applies alpha conversion to an 'SDRS' on the basis of a conversion list
-- for all embedded 'DisVar's.
-- TODO FIX! And needs to take a CDU and not an SDRSFormula
---------------------------------------------------------------------------
sdrsAlphaConvert :: SDRS -> M.Map DisVar DisVar -> SDRS
sdrsAlphaConvert (SDRS m l) cm = SDRS (M.fromList (convert' (M.assocs m) cm)) (M.findWithDefault l l cm)
  where convert' :: [(DisVar, SDRSFormula)] -> M.Map DisVar DisVar -> [(DisVar, SDRSFormula)]
        convert' [] _           = []
        convert' (t:rest) nm    = (convertTuple t nm) : (convert' rest nm)
        convertTuple :: (DisVar, SDRSFormula) -> M.Map DisVar DisVar -> (DisVar, SDRSFormula)
        convertTuple (dv, sf) nm = (M.findWithDefault dv dv nm, convertSF sf nm)
        convertSF :: SDRSFormula -> M.Map DisVar DisVar -> SDRSFormula
        convertSF sf _ = sf
        --convertSF d@(EDU _) _             = d
        --convertSF (CDU (Relation rel dv1 dv2)) nm = CDU $ Relation rel (M.findWithDefault dv1 dv1 nm) (M.findWithDefault dv2 dv2 nm)
        --convertSF (CDU (And sf1 sf2)) nm            = CDU $ And (convertSF (CDU sf1) nm) (convertSF (CDU sf2) nm)
        --convertSF (CDU (Not sf1)) nm                = CDU $ Not (convertSF (CDU sf1) nm)

-----------------------------------------------------------------------------
---- | Applies drt alpha conversion on all embedded 'DRS's of the given 'SDRS'
---- on the basis of a conversion list for 'DRSRef's @rs@.
-----------------------------------------------------------------------------
--sdrsDRSRefsAlphaConvert :: SDRS -> [(DRSRef,DRSRef)] -> SDRS
--sdrsDRSRefsAlphaConvert s@(SDRS m l) rs = SDRS mConv l
--  where mConv = M.mapWithKey convert m
--        convert :: DisVar -> SDRSFormula -> SDRSFormula
--        convert dv (EDU d) = EDU $ renameSubDRS d gd rs
--          where gd = foldl (<<+>>) (DRS [] []) accDRSs
--                accDRSs = accessibleDRSs s dv
--        convert _ r@(Relation {}) = r
--        convert dv (And sf1 sf2) = And (convert dv sf1) (convert dv sf2)
--        convert dv (Not sf1) = Not (convert dv sf1)

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
        conv _ sf            = sf
        -- ^ the SDRSFormula labeled by @key@ was not a EDU, skip it.

---------------------------------------------------------------------------
-- | For an 'SDRS' @s@, a list of 'DisVar's @dvs@ and a DRSRef conversion map
-- @cm@, DRS-alpha-converts all 'DRSs' labeled by the 'DisVar's in @dvs@.
---------------------------------------------------------------------------
sdrsAlphaConvertDRSs :: SDRS -> [DisVar] -> [(DRSRef,DRSRef)] -> SDRS
sdrsAlphaConvertDRSs s [] _                    = s
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
-- | Normalizes the nodes in the 'SDRS' @s@, i.e., maps the @s@'s 'DisVar's
-- to a sequence from 0 to n, where n is equal to the number of unique 'DisVar's
-- in the 'SDRS'.
---------------------------------------------------------------------------
normalize :: SDRS -> SDRS
normalize s = sdrsAlphaConvert s normMap
  where build :: [DisVar] -> DisVar -> M.Map DisVar DisVar -> M.Map DisVar DisVar
        build [] _ nm                     = nm
        build (cur:rest) index nm 
          | ((M.lookup cur g) == Nothing) = M.insert cur index (build rest (index + 1) nm)
          | otherwise                     = M.insert cur index (build (rest `union` (map fst $ g M.! cur)) (index + 1) nm)
        normMap = build [root s] 0 M.empty
        g = discourseGraph s
