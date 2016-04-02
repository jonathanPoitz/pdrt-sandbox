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
( --discourseGraph
  buildDGraph
, accessibleNodes
--, showDGraph
--, printDGraph
) where

import Data.SDRS.DataType
--import Data.Graph (graphFromEdges', Graph, Vertex)
import qualified Data.Map as M
import Data.List (union, nub)
--import qualified Data.Set as Set

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
-- | A discourse graph, consisting of the graph structure and the labels
-- of its edges
---------------------------------------------------------------------------
--data DGraph =
--  DGraph Graph [(Vertex, Vertex, EdgeLabel)]

--type Label = String

--type Key = Int

---------------------------------------------------------------------------
-- | A DNode is represented by its label, its id and a list of successor 
-- nodes with the edge label (if any)
---------------------------------------------------------------------------
--data DNode = 
--  DNode Label Key [(Key, Label)] 

---------------------------------------------------------------------------
-- | Given an SDRS, build a labeled graph structure
---------------------------------------------------------------------------
buildDGraph :: SDRS -> (M.Map DisVar [[(DisVar, String)]])
buildDGraph (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [[(DisVar, String)]]) -> DisVar -> SDRSFormula -> (M.Map DisVar [[(DisVar, String)]])
        build acc dv0 (Relation l dv1 dv2) = M.insertWith (++) dv1 [[(dv2,l)]] (M.insertWith (++) dv0 [[(dv1,""),(dv2,"")]] acc)
        build acc dv0 (And sf1 sf2)        = build (build acc dv0 sf1) dv0 sf2
        build acc dv0 (Not sf1)            = build acc dv0 sf1
        build acc _ _                      = acc

---------------------------------------------------------------------------
-- | Given a graph structure (as produced by buildDGraph) and a discourse
-- variable, lists the accessible variables from this variable
---------------------------------------------------------------------------
accessibleNodes :: (M.Map DisVar [[(DisVar, String)]]) -> DisVar -> [DisVar]
accessibleNodes m dv1 = walkEdges [dv1]
  where walkEdges :: [DisVar] -> [DisVar]
        walkEdges [] = []
        walkEdges (k:rest) = (keys k) `union` walkEdges (keys k) `union` (walkEdges rest)
        keys :: DisVar -> [DisVar]
        keys dv2 = nub (M.keys (M.filter (findKey dv2) m))
        findKey :: DisVar -> [[(DisVar, String)]] -> Bool
        findKey _ [] = False
        findKey _ ([]:_) = False -- not so sure about this one
        findKey dv ([(dv',_)]:[]) 
          | dv' == dv = True -- simple relation
        findKey dv ([(dv',_)]:rest)
          | dv' == dv = True
          | otherwise = findKey dv rest
        findKey dv (((dv',_):rest'):rest)
          | dv' == dv = True
          | otherwise = findKey dv (rest':rest)
