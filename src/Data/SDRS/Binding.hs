{- |
Module      :  Data.SDRS.Binding
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS binding
-}

module Data.SDRS.Binding
(
  allRelArgsBound
, sdrsFreeRefs
, sdrsBoundRef
, noSelfRefs
, allSegmentsBound
) where

import qualified Data.Map as M
import qualified Data.Set as S -- because of conflict with foldl and union
import Data.List (union)

import Data.DRS.Binding
import Data.DRS.Merge

import Data.SDRS.DataType
import Data.SDRS.Structure
import Data.SDRS.DiscourseGraph

---------------------------------------------------------------------------
-- | Returns whether 'DRSRef' @r@ is bound in the 'SDRS' @s@ relative to its
-- introduction side labeled by 'DisVar' @dv@.
---------------------------------------------------------------------------
sdrsBoundRef :: DRSRef -> DisVar -> SDRS -> Bool
sdrsBoundRef r dv s@(SDRS m _)
  | M.lookup dv m == Nothing = False
  | otherwise                = case (m M.! dv) of
                                (EDU n) -> bound n
                                _       -> bound (DRS [] [])
  where bound :: DRS -> Bool
        bound d = drsBoundRef r d gd
          where gd = foldl (<<+>>) (DRS [] []) accDRSs
                accDRSs = accessibleDRSs s dv

---------------------------------------------------------------------------
-- | Returns the list of free 'DRSRef's in the 'SDRS' @s@.
---------------------------------------------------------------------------
sdrsFreeRefs :: SDRS -> [DRSRef]
sdrsFreeRefs s = free $ segments s
  where free :: [(DisVar, SDRSFormula)] -> [DRSRef]
        free []                    = []
        free ((dv,EDU d):rest) = drsFreeRefs d gd `union` free rest
          where gd = foldl (<<+>>) (DRS [] []) accDRSs
                accDRSs = accessibleDRSs s dv
        free (_:rest)              = free rest

---------------------------------------------------------------------------
-- | Checks if all relations in the 'SDRS' @s@ use labels as arguments
-- that are valid 'DisVar's, i.e. they are mapped to an 'SDRSFormula' in the
-- internal map of @s@.
-- CHECK If we define binding for DVs, then this would basically check whether
-- all DVs are bound 
---------------------------------------------------------------------------
allRelArgsBound :: SDRS -> Bool
allRelArgsBound s@(SDRS m _) = relVars `S.isProperSubsetOf` disVars
  where relVars = S.fromList $ relArgs s
        disVars = S.fromList $ M.keys m

---------------------------------------------------------------------------
-- | Returns 'True' if the given 'SDRS' has no self-referencing relations
-- such as 3:Narration(4,4) or 3:Narration(3,4), otherwise returns 'False'.
---------------------------------------------------------------------------
noSelfRefs :: SDRS -> Bool
noSelfRefs (SDRS m _) = all noSelfRef (M.assocs m)
  where noSelfRef :: (DisVar, SDRSFormula) -> Bool
        noSelfRef (dv0, CDU (Relation _ dv1 dv2)) = dv1 /= dv2 && dv0 /= dv1 && dv0 /= dv2
        noSelfRef (dv0, CDU (And sf1 sf2))        = noSelfRef (dv0,CDU sf1) && noSelfRef (dv0,CDU sf2)
        noSelfRef (dv0, CDU (Not sf1))            = noSelfRef (dv0,CDU sf1)
        noSelfRef _                             = True  

---------------------------------------------------------------------------
-- | Checks for the given 'SDRS' @s@ whether all Segments are bound as an 
-- argument in a relation.
---------------------------------------------------------------------------
allSegmentsBound :: SDRS -> Bool
allSegmentsBound s = allSegmentRelNames `S.isSubsetOf` allRelArgs
  where allSegmentRelNames = S.fromList $ map fst $ segments s
        allRelArgs = S.fromList $ relArgs s
