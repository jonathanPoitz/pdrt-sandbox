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
( discourseGraph
, buildOutscopeMap
--, outscopes
--, showDGraph
--, printDGraph
) where

import Data.SDRS.DataType
import Data.SDRS.Structure (expandRecursiveFormula)
import Data.Graph (graphFromEdges', Graph, Vertex)
import qualified Data.List as List 
import qualified Data.Map as Map
--import qualified Data.Set as Set

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

--type DGraph = Graph

---------------------------------------------------------------------------
-- | Shows a projection table
---------------------------------------------------------------------------
--showDGraph :: DGraph -> String
--showDGraph (DGraph dg) = 

---------------------------------------------------------------------------
-- | Prints a projection table
---------------------------------------------------------------------------
--printDGraph :: PTable -> IO ()
--printDGraph pt = putStrLn $ '\n' : showDGraph pt

---------------------------------------------------------------------------
-- | Returns, given an SDRS, a map that from each discourse variable to 
-- those that it outscopes. If @fullMap@ is set to False, the returned list
-- only contains the discourse variables that outscope others.
---------------------------------------------------------------------------
buildOutscopeMap :: SDRS -> Bool -> (Map.Map DisVar [DisVar])
buildOutscopeMap (SDRS m _) buildFullMap = Map.foldlWithKey build initialMap m
  where build :: (Map.Map DisVar [DisVar]) -> DisVar -> SDRSFormula -> (Map.Map DisVar [DisVar])
        build acc dv0 (Relation _ dv1 dv2) = Map.insertWith (List.union) dv0 (List.nub [dv1,dv2]) acc
        build acc dv0 sf@(And _ _) = buildRecursive acc dv0 $ expandRecursiveFormula sf
        build acc dv0 sf@(Not _) = buildRecursive acc dv0 $ expandRecursiveFormula sf
        build acc _ _ = acc
        buildRecursive :: (Map.Map DisVar [DisVar]) -> DisVar -> [SDRSFormula] -> (Map.Map DisVar [DisVar])
        -- only add Relations since only they hold discourse variables as arguments
        buildRecursive acc _ [] = acc
        buildRecursive acc dv0 ((Relation _ dv1 dv2):rest) = Map.insertWith (List.union) dv0 (List.nub [dv1,dv2]) (buildRecursive acc dv0 rest)
        buildRecursive acc dv0 (_:rest) = buildRecursive acc dv0 rest
        initialMap = if buildFullMap then Map.fromList $ zip (Map.keys m) (repeat [])
                                     else Map.empty

---------------------------------------------------------------------------
-- | Given an SDRS @s@, builds up a simple Graph structure
---------------------------------------------------------------------------
discourseGraph :: SDRS -> (Graph, Vertex -> (DisVar, DisVar, [DisVar]))
discourseGraph s = graphFromEdges' graph_map
  where graph_map = map (\(x,y) -> (x,x,y)) (Map.assocs (buildOutscopeMap s True))

-----------------------------------------------------------------------------
---- | Returns, given an 'SDRS' @s@, if discourse variable @dv@ outscopes 
---- another discourse variable @dv'@.
-----------------------------------------------------------------------------
--outscopes :: SDRS -> DisVar -> DisVar -> Bool
--outscopes s dv dv' = dv' `elem` (reachable g dv)
--  where (g,_) = discourseGraph s
