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
, rf
, Label
, DGraph
, root
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.List (union, nub, sort, (\\))
import Data.SDRS.Structure (relLabels)

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
-- | Given an SDRS, build a labeled graph structure, consisting of a tuple
-- of the graph itself and the last node
---------------------------------------------------------------------------
discourseGraph :: SDRS -> DGraph
discourseGraph (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [(DisVar, Label)]) -> DisVar -> SDRSFormula -> M.Map DisVar [(DisVar, Label)]
        build acc dv0 (Relation label dv1 dv2) = M.insertWith (++) dv1 [(dv2,label)] (M.insertWith (++) dv0 [(dv1,""),(dv2,"")] acc)
        build acc dv0 (And sf1 sf2)            = build (build acc dv0 sf1) dv0 sf2
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
        findKey :: DisVar -> [(DisVar, Label)] -> Bool
        findKey _ []  = False
        findKey dv ((dv',_):rest)
          | dv' == dv = True
          | otherwise = findKey dv rest

---------------------------------------------------------------------------
-- | computes the right frontier of an 'SDRS', also in order
-- TODO might be simplified. currently a lot of steps, maybe rec can be simplified.
-- TODO debug structural checks -> does this work in every situation?
---------------------------------------------------------------------------
rf :: SDRS -> [DisVar]
rf s@(SDRS _ l) = walkEdges [l]
  where g = discourseGraph s
        walkEdges :: [DisVar] -> [DisVar]
        walkEdges []       = []
        walkEdges (v:rest) = (walkEdges rest) `union` walkEdges (keys v) `union` (keys v) `union` [v]
        keys :: DisVar -> [DisVar]
        keys dv = nub (M.keys (M.filterWithKey (isOnRF dv) g))
        isOnRF :: DisVar -> DisVar -> [(DisVar, Label)] -> Bool
        isOnRF _ _ []                                = False
        isOnRF dv key ((dv',label):rest)
          | dv' == dv && (isStrucRelation label ||
                          not (isCrdRelation label)) = True
          | otherwise                                = isOnRF dv key rest

---------------------------------------------------------------------------
-- | Return the root node of the discourse graph. If the graph doesn't have
-- a single node that dominates all subelements. The return type is list
-- to account for the (infelicitous) case where there is more than one root
-- node or nodes in the graph that are unconnected (thus root nodes of a separate
-- graph).
---------------------------------------------------------------------------
root :: SDRS -> [DisVar]
root (SDRS m _) = (sort $ nub (M.keys m)) \\ (sort $ nub (relLabels $ M.elems m))




