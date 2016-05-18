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
, sdrsDRSRefsAlphaConvert
, normalize
, buildDRSRefConvMap
, buildConvMap
) where

import Data.List (union, insert)
import qualified Data.Map as M

import Data.SDRS.DataType
import Data.SDRS.DiscourseGraph

import Data.DRS.LambdaCalculus (drsAlphaConvert)
import Data.DRS.Variables (increase)

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
-- | Applies drt alpha conversion on all embedded 'DRS's of the given 'SDRS'
-- on the basis of a conversion list for 'DRSRef's @rs@.
---------------------------------------------------------------------------
sdrsDRSRefsAlphaConvert :: SDRS -> [(DRSRef,DRSRef)] -> SDRS
sdrsDRSRefsAlphaConvert (SDRS m l) rs = SDRS mConv l
  where mConv = M.map convert m
        convert :: SDRSFormula -> SDRSFormula
        convert (Segment d) = Segment $ drsAlphaConvert d rs
        convert r@(Relation {}) = r
        convert (And sf1 sf2) = And (convert sf1) (convert sf2)
        convert (Not sf1) = Not (convert sf1)

---------------------------------------------------------------------------
-- | Given a list of all present 'DRSRef's @rs@ and a list of overlapping
-- 'DRSRef's, builds a conversion list.
---------------------------------------------------------------------------
buildDRSRefConvMap :: [DRSRef] -> [DRSRef] -> [(DRSRef,DRSRef)]
buildDRSRefConvMap _ [] = []
buildDRSRefConvMap rs (ref:rest) = (ref,newRef) : buildDRSRefConvMap (newRef:rs) rest
  where newRef = checkRef $ incrRef ref
        incrRef :: DRSRef -> DRSRef
        incrRef (DRSRef r) = DRSRef $ increase r
        incrRef ldr = ldr
        checkRef :: DRSRef -> DRSRef
        checkRef dr
          | dr `elem` rs = checkRef $ incrRef dr
          | otherwise    = dr

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
-- | normalizes the nodes in an SDRS.
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


