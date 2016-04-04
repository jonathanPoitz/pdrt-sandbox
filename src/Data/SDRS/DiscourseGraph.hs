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
--import Data.Char (toLower)

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

type DGraph = (M.Map DisVar [(DisVar, Label)], DisVar)

---------------------------------------------------------------------------
-- | Given an SDRS, build a labeled graph structure, consisting of a tuple
-- of the graph itself and the last node
---------------------------------------------------------------------------
buildDGraph :: SDRS -> DGraph
buildDGraph (SDRS m l) = (M.foldlWithKey build M.empty m, l)
  where build :: (M.Map DisVar [(DisVar, Label)]) -> DisVar -> SDRSFormula -> M.Map DisVar [(DisVar, Label)]
        build acc dv0 (Relation label dv1 dv2) = M.insertWith (union) dv1 [(dv2,label)] (M.insertWith (union) dv0 [(dv1,""),(dv2,"")] acc)
        build acc dv0 (And sf1 sf2)        = build (build acc dv0 sf1) dv0 sf2
        build acc dv0 (Not sf1)            = build acc dv0 sf1
        build acc _ _                      = acc

---------------------------------------------------------------------------
-- | Given a graph structure (as produced by buildDGraph) and a discourse
-- variable, lists the accessible variables from this variable
-- TODO to be simplified!
---------------------------------------------------------------------------
accessibleNodes :: DGraph -> DisVar -> [DisVar]
accessibleNodes g dv1 = walkEdges [dv1]
  where m = fst g
        walkEdges :: [DisVar] -> [DisVar]
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

---------------------------------------------------------------------------
-- | computes the right frontier of a graph. TO BE CONTINUED AND SIMPLIFIED!!!
---------------------------------------------------------------------------
--rf :: DGraph -> [DisVar]
--rf g = walkEdges [l]
--  where l = snd g
--        m = fst g
--        walkEdges :: [DisVar] -> [DisVar]
--        walkEdges [] = []
--        walkEdges (v:rest) = [v] `union` (rfKeys v) `union` walkEdges (rfKeys v) `union` (walkEdges rest) -- v = value
--        keys :: DisVar -> [DisVar]
--        keys dv2 = nub (M.keys (M.filter (findKey dv2) m))
--        rfKeys :: [DisVar] -> [DisVar]
--        rfKeys v = notCrdRel (keys v)
--        notCrdRel :: DisVar -> [DisVar]
--        notCrdRel v k = filter (\(v', label) -> v' == v && not isCrdRelation' label) (m M.! k)
--        findKey :: DisVar -> [(DisVar, Label)] -> Bool
--        findKey _ [] = False
--        -- findKey _ ([]:_) = False -- not so sure about this one
--        findKey dv ((dv',_):[]) 
--          | dv' == dv = True -- simple relation
--        findKey dv ((dv',_):rest)
--          | dv' == dv = True
--          | otherwise = findKey dv rest

---------------------------------------------------------------------------
-- * Private
---------------------------------------------------------------------------

-----------------------------------------------------------------------------
---- | simple version working on strings instead of SDRSFormulae
-----------------------------------------------------------------------------
--isSubRelation' :: String -> Bool
--isSubRelation' label = (filter (/=' ') (map toLower label)) `elem` relations
--  where relations = ["elaboration",
--                    "entityElaboration",
--                    "comment",
--                    "flashback",
--                    "background",
--                    "goal",
--                    "explanation",
--                    "attribution"]

---------------------------------------------------------------------------
-- | simple version working on strings instead of SDRSFormulae
---------------------------------------------------------------------------
--isCrdRelation' :: String -> Bool
--isCrdRelation' label = (filter (/=' ') (map toLower label)) `elem` relations
--  where relations = ["narration",
--                    "contrast",
--                    "result",
--                    "parallel",
--                    "continuation",
--                    "alternation",
--                    "conditional"]
