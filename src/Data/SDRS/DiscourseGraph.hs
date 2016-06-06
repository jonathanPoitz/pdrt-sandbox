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
, inferLast
, isOnRF
, immediateOutscopes
, Label
, DGraph
, root
, isRoot
, hasParents
) where

import qualified Data.Map as M
import Data.List (union, nub, sort, (\\))

import Data.SDRS.DataType
import Data.SDRS.Relation
import Data.SDRS.Structure

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
        build acc dv0 (CDU (Relation rel dv1 dv2)) = M.insertWith (++) dv1 [(dv2,rel)] (M.insertWith (union) dv0 [(dv1,Outscopes),(dv2,Outscopes)] acc)
        build acc dv0 (CDU (And sf1 sf2))            = build (build acc dv0 (CDU sf1)) dv0 (CDU sf2)
        build acc dv0 (CDU (Not sf1))                = build acc dv0 (CDU sf1)
        build acc _ _                              = acc

---------------------------------------------------------------------------
-- | Given an 'SDRS', returns a map from 'DisVar's to 'DisVar's that it outscopes
---------------------------------------------------------------------------
immediateOutscopes :: SDRS -> M.Map DisVar [DisVar]
immediateOutscopes (SDRS m _) = M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [DisVar]) -> DisVar -> SDRSFormula -> M.Map DisVar [DisVar]
        build acc dv0 (CDU (Relation _ dv1 dv2)) = M.insertWith (union) dv0 [dv1,dv2] acc
        build acc dv0 (CDU (And sf1 sf2))            = build (build acc dv0 (CDU sf2)) dv0 (CDU sf1)
        build acc dv0 (CDU (Not sf1))                = build acc dv0 (CDU sf1)
        build acc _ _                          = acc

---------------------------------------------------------------------------
-- | Given the 'SDRS' @s@ and the 'DisVar' @dv1@, lists all accessible 'DisVar's
-- using the discourse graph of @s@. The output list's elements are ordered
-- with the first element being the farthest and the last the closest to @dv1@
-- in the discourse graph.
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
-- | Returns all 'DRS's accessible from a given 'DisVar' @dv@ in the 'SDRS' @s@.
---------------------------------------------------------------------------
accessibleDRSs :: SDRS -> DisVar -> [DRS]
accessibleDRSs s@(SDRS m _) dv = accDRSs
  where accDisVars = accessibleNodes s dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [drs | (EDU drs) <- accDUs]

---------------------------------------------------------------------------
-- | computes the right frontier of an 'SDRS', in order of locality
-- TODO might be simplified. currently a lot of steps, maybe rec can be simplified.
-- TODO debug structural checks -> does this work in every situation?
-- If the last node is wrong or even an unvalid pointer, this function will
-- return a wrong rf. FIX, walk from root instead to avoid that?
-- TODO de-uglify. possibly implement function to rewind an sdrs by one step, which could get arbitrarily complex
---------------------------------------------------------------------------
rf :: SDRS -> [DisVar]
rf s@(SDRS _ l) = case M.member ((parents l) !! 0) outscopes &&
                       (all (\(Relation rel _ _) -> isSub rel) lastRels) of True -> walkEdges [l] `union` rf rewindedSDRSWithNewLast
                                                                            False -> walkEdges [l]
  where rewindedSDRSWithNewLast = updateLast rewindedSDRS rewindedLast 
        rewindedLast = inferLast rewindedSDRS
        rewindedSDRS = removeRels s lastRels
        lastRels = [rel | (rel@(Relation {})) <- calcRightArgRels s l]
        g = discourseGraph s
        outscopes = immediateOutscopes s
        walkEdges :: [DisVar] -> [DisVar]
        walkEdges []       = []
        walkEdges (v:rest) =  (walkEdges rest) `union` walkEdges (parents v) `union` (parents v) `union` [v]
        parents :: DisVar -> [DisVar]
        parents dv = nub (M.keys (M.filter (onRF dv) g))
        onRF :: DisVar -> [(DisVar, SDRSRelation)] -> Bool
        onRF _ []                               = False
        onRF dv ((dv',Outscopes):rest)
          | dv == dv'                           = True
          | otherwise                           = onRF dv rest
        onRF dv ((dv',rel@(SDRSRelation {})):rest)
          | dv == dv' && ((isStructured rel) ||                                 -- FIX, here we're only working with a label, how do we check the label's structuredness etc?
                          (relType rel == Sub)) = True
          | otherwise                           = onRF dv rest

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, infers the last node from the discourse structure
---------------------------------------------------------------------------
inferLast :: SDRS -> DisVar
inferLast s@(SDRS m _) = walk (root s)
  where walk :: DisVar -> DisVar
        walk dv = case (m M.! dv) of (EDU _)   -> dv
                                     (CDU cdu) -> walk $ getNext cdu
          where getNext :: CDU -> DisVar
                getNext (Relation _ _ next) = next
                getNext (And _ cdu2) = getNext cdu2
                getNext (Not cdu1) = getNext cdu1

---------------------------------------------------------------------------
-- | checks whether a given 'DisVar' @dv@ is on the right frontier of 
-- 'SDRS' @s@.
---------------------------------------------------------------------------
isOnRF :: SDRS -> DisVar -> Bool
isOnRF s dv = dv `elem` rf s

---------------------------------------------------------------------------
-- | Return the root node of the discourse graph. If the graph has more than
-- one root node, only the first one in the list will be returned. 
---------------------------------------------------------------------------
root :: SDRS -> DisVar
root s = ((sort $ nub (dus s)) \\ (sort $ nub (relArgs s))) !! 0

---------------------------------------------------------------------------
-- | checks whether 'DisVar' @dv@ in 'SDRS' @s@ is the root node.
---------------------------------------------------------------------------
isRoot :: SDRS -> DisVar -> Bool
isRoot s dv = dv == root s

---------------------------------------------------------------------------
-- | checks whether 'DisVar' @dv@ in 'SDRS' @s@ has any incoming edges.
---------------------------------------------------------------------------
hasParents :: SDRS -> DisVar -> Bool
hasParents (SDRS m _) dv = any incoming $ M.elems m
  where incoming :: SDRSFormula -> Bool
        incoming (CDU (Relation _ _ dv2)) = dv == dv2
        incoming (CDU (And sf1 sf2))        = incoming (CDU sf1) || incoming (CDU sf2)
        incoming (CDU (Not sf1))            = incoming (CDU sf1)
        incoming (EDU _)          = False
