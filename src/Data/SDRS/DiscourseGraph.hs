{- |
Module      :  Data.SDRS.DiscourseGraph
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS discourse graph
-}

module Data.SDRS.DiscourseGraph
( 
  discourseGraph
, accessibleNodes
, accessibleDRSs
, rf
, immediateOutscopes
, Label
, DGraph
, root
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.List (union, nub, sort, (\\))
import Data.SDRS.Structure (relLabels, dus)

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Shows a discourse graph
---------------------------------------------------------------------------
--showDGraph :: DGraph -> String
--showDGraph (DGraph graph labels) = show graph ++ "\n" ++ show labels

--instance Show DGraph where
  --show dg = '\n' : showDGraph dg



---------------------------------------------------------------------------
-- | DGraph
---------------------------------------------------------------------------
type DGraph = M.Map DisVar [(DisVar, SDRSRelation)]

---------------------------------------------------------------------------
-- | Given an SDRS, build a labeled graph structure, consisting of a tuple
-- of the graph itself and the last node
---------------------------------------------------------------------------
discourseGraph :: SDRS -> DGraph
discourseGraph (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [(DisVar, SDRSRelation)]) -> DisVar -> SDRSFormula -> M.Map DisVar [(DisVar, SDRSRelation)]
        build acc dv0 (Relation rel dv1 dv2) = M.insertWith (++) dv1 [(dv2,rel)] (M.insertWith (union) dv0 [(dv1,Outscopes),(dv2,Outscopes)] acc)
        build acc dv0 (And sf1 sf2)            = build (build acc dv0 sf1) dv0 sf2
        build acc dv0 (Not sf1)                = build acc dv0 sf1
        build acc _ _                          = acc

---------------------------------------------------------------------------
-- | Given an 'SDRS', returns a map from 'DisVar's to 'DisVar's that it outscopes
---------------------------------------------------------------------------
immediateOutscopes :: SDRS -> M.Map DisVar [DisVar]
immediateOutscopes (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [DisVar]) -> DisVar -> SDRSFormula -> M.Map DisVar [DisVar]
        build acc dv0 (Relation _ dv1 dv2) = M.insertWith (union) dv0 [dv1,dv2] acc
        build acc dv0 (And sf1 sf2)            = build (build acc dv0 sf2) dv0 sf1
        build acc dv0 (Not sf1)                = build acc dv0 sf1
        build acc _ _                          = acc

---------------------------------------------------------------------------
-- | Given an 'SDRS' and a discourse variable, lists the accessible
-- variables from this variable in order of proximity to local discourse
-- variable, with the first entry being the farthest and the last the closest
-- TODO might be simplified. currently a lot of steps, maybe rec can be simplified.
-- TODO structural relations? def. 15 book
---------------------------------------------------------------------------
accessibleNodes :: SDRS -> DisVar -> [DisVar]
accessibleNodes s dv1 = walkEdges [dv1]
  where g = discourseGraph s
        walkEdges :: [DisVar] -> [DisVar]
        walkEdges []       = []
        walkEdges (k:rest) = (walkEdges rest) `union` walkEdges (keys k) `union` (keys k)
        keys :: DisVar -> [DisVar]
        keys dv2 = nub (M.keys (M.filter (findKey dv2) g))
        findKey :: DisVar -> [(DisVar, SDRSRelation)] -> Bool
        findKey _ []  = False
        findKey dv ((dv',_):rest)
          | dv' == dv = True
          | otherwise = findKey dv rest

---------------------------------------------------------------------------
-- | returns all accessible 'DRS's from a given 'DisVar' @dv@ in the 'SDRS' @s@. 
---------------------------------------------------------------------------
accessibleDRSs :: SDRS -> DisVar -> [DRS]
accessibleDRSs s@(SDRS m _) dv = accDRSs
  where accDisVars = accessibleNodes s dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [ drs | (Segment drs) <- accDUs]

---------------------------------------------------------------------------
-- | computes the right frontier of an 'SDRS', in order of locality
-- TODO might be simplified. currently a lot of steps, maybe rec can be simplified.
-- TODO debug structural checks -> does this work in every situation?
-- If the last node is wrong or even an unvalid pointer, this function will
-- return a wrong rf. FIX, walk from root instead to avoid that?
---------------------------------------------------------------------------
rf :: SDRS -> [DisVar]
rf s@(SDRS _ l) = walkEdges [l]
  where g = discourseGraph s
        walkEdges :: [DisVar] -> [DisVar]
        walkEdges []       = []
        walkEdges (v:rest) = (walkEdges rest) `union` walkEdges (parents v) `union` (parents v) `union` [v]
        parents :: DisVar -> [DisVar]
        parents dv = nub (M.keys (M.filter (isOnRF dv) g))
        isOnRF :: DisVar -> [(DisVar, SDRSRelation)] -> Bool
        isOnRF _ []                             = False
        isOnRF dv ((dv',Outscopes):rest)
          | dv == dv'                           = True
          | otherwise                           = isOnRF dv rest
        isOnRF dv ((dv',rel@(SDRSRelation {})):rest)
          | dv == dv' && ((isStructured rel) ||                                 -- FIX, here we're only working with a label, how do we check the label's structuredness etc?
                          (relType rel == Sub)) = True
          | otherwise                           = isOnRF dv rest


---------------------------------------------------------------------------
-- | Return the root node of the discourse graph. If the graph has more than
-- one root node, only the first one in the list will be returned. 
---------------------------------------------------------------------------
root :: SDRS -> DisVar
root s = ((sort $ nub (dus s)) \\ (sort $ nub (relLabels s))) !! 0
