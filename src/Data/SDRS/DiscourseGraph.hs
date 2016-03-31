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
--, buildDGraph'
, accessibleNodes
--, outscopes
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
-- | A graph is represented by a list of DNodes
---------------------------------------------------------------------------
--data DGraph = 
--  DGraph [DNode]

---------------------------------------------------------------------------
-- | A DNode is represented by its label, its id and a list of successor 
-- nodes with the edge label (if any)
---------------------------------------------------------------------------
--data DNode = 
--  DNode Label Key [(Key, Label)] 

--graphFromSDRS :: SDRS -> DGraph
--graphFromSDRS 

---------------------------------------------------------------------------
-- | Returns, given an SDRS, a map that from each discourse variable to 
-- those that it outscopes. If @fullMap@ is set to False, the returned list
-- only contains the discourse variables that outscope others.
---------------------------------------------------------------------------
--buildDGraph :: SDRS -> [(DisVar,[[(DisVar, String)]])]
--buildDGraph (SDRS m _) = M.assocs (M.foldlWithKey build M.empty m)
--  where build :: (M.Map DisVar [[(DisVar, String)]]) -> DisVar -> SDRSFormula -> (M.Map DisVar [[(DisVar, String)]])
--        build acc dv0 (Relation l dv1 dv2) = M.insertWith (++) dv1 [[(dv2,l)]] (M.insertWith (++) dv0 [[(dv1,""),(dv2,"")]] acc)
--        build acc dv0 (And sf1 sf2)        = build (build acc dv0 sf1) dv0 sf2
--        build acc dv0 (Not sf1)            = build acc dv0 sf1
--        build acc _ _                      = acc

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

--buildDGraph :: SDRS -> DGraph
--buildDGraph (SDRS m _) = build $ buildOutscopeMap m
--  where build :: [(DisVar,[[(DisVar, String)]])] -> DGraph
--        build [] = build rest
--        build ((dv,[]):rest) = build rest
--        build ((dv,):rest)

---------------------------------------------------------------------------
-- | Given an SDRS @s@, builds up a simple labeled Graph structure using 
-- the immediate outscopings that are in place
---------------------------------------------------------------------------
--discourseGraph :: SDRS -> DGraph
--discourseGraph s@(SDRS m _) = DGraph g labels
--  where graph_map = map (\(x,y) -> (x,x,y)) (M.assocs (buildOutscopeMap s True))
--        labels = edgeLabels (map snd (M.assocs m))
--        g = fst $ graphFromEdges' graph_map
--        edgeLabels :: [SDRSFormula] -> [(Vertex, Vertex, EdgeLabel)]
--        edgeLabels [] = []
--        edgeLabels (Segment {}:rest) = edgeLabels rest
--        edgeLabels (Relation l dv1 dv2:rest) = (dv1, dv2, l):(edgeLabels rest)
--        edgeLabels (And sf1 sf2:rest) = edgeLabels [sf1] ++ edgeLabels [sf2] ++ edgeLabels rest
--        -- should a negated relation be put on the graph as a label?
--        edgeLabels (Not sf1:rest) = edgeLabels [sf1] ++ edgeLabels rest



---------------------------------------------------------------------------
-- | Lists all DUs (represented by their labels) in an SDRS that (disregarding
-- the RFC) are accessible from the given label
---------------------------------------------------------------------------
--naiveAccessibleDRSs :: SDRS -> DisVar -> [DisVar]
--naiveAccessibleDRSs (SDRS m _) dv = reachable g dv `union` 

-----------------------------------------------------------------------------
---- | Returns, given an 'SDRS' @s@, if discourse variable @dv@ outscopes 
---- another discourse variable @dv'@.
-----------------------------------------------------------------------------
--outscopes :: SDRS -> DisVar -> DisVar -> Bool
--outscopes s dv dv' = dv' `elem` (reachable g dv)
--  where (g,_) = discourseGraph s
