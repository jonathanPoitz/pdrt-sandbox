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
  buildDGraph
, accessibleNodes
) where

import Data.SDRS.DataType
import qualified Data.Map as M
import Data.List (union, nub)

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

type Label = String

type DGraph = M.Map DisVar [(DisVar, Label)]

---------------------------------------------------------------------------
-- | Given an SDRS, build a labeled graph structure
---------------------------------------------------------------------------
buildDGraph :: SDRS -> DGraph
buildDGraph (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [(DisVar, Label)]) -> DisVar -> SDRSFormula -> M.Map DisVar [(DisVar, Label)]
        build acc dv0 (Relation l dv1 dv2) = M.insertWith (union) dv1 [(dv2,l)] (M.insertWith (union) dv0 [(dv1,""),(dv2,"")] acc)
        build acc dv0 (And sf1 sf2)        = build (build acc dv0 sf1) dv0 sf2
        build acc dv0 (Not sf1)            = build acc dv0 sf1
        build acc _ _                      = acc

---------------------------------------------------------------------------
-- | Given a graph structure (as produced by buildDGraph) and a discourse
-- variable, lists the accessible variables from this variable
---------------------------------------------------------------------------
accessibleNodes :: DGraph -> DisVar -> [DisVar]
accessibleNodes m dv1 = walkEdges [dv1]
  where walkEdges :: [DisVar] -> [DisVar]
        walkEdges [] = []
        walkEdges (k:rest) = (keys k) `union` walkEdges (keys k) `union` (walkEdges rest)
        keys :: DisVar -> [DisVar]
        keys dv2 = nub (M.keys (M.filter (findKey dv2) m))
        findKey :: DisVar -> [(DisVar, Label)] -> Bool
        findKey _ [] = False
        -- findKey _ ([]:_) = False -- not so sure about this one
        findKey dv ((dv',_):[]) 
          | dv' == dv = True -- simple relation
        findKey dv ((dv',_):rest)
          | dv' == dv = True
          | otherwise = findKey dv rest
        -- findKey dv (((dv',_):rest'):rest)
          -- | dv' == dv = True
          -- | otherwise = findKey dv (rest':rest)
