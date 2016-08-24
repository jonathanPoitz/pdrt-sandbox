{- |
Module      :  Data.SDRS.DiscourseStructure
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS discourse structure
-}

module Data.SDRS.DiscourseStructure
( 
  accessibleDRSs
, rf
, rfSimple
, inferLast
, isOnRF
, iOutscopesMap
, iOutscopesFrom
, outscopesFrom
, RelName
, root
, isRoot
, hasParents
) where

import qualified Data.Map as M
import Data.List
--import Debug.Trace

import Data.SDRS.DataType
import Data.SDRS.Relation
import Data.SDRS.Structure

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns all 'DRS's accessible from a given 'DisVar' @dv@ in the 'SDRS' @s@.
---------------------------------------------------------------------------
accessibleDRSs :: SDRS -> DisVar -> [DRS]
accessibleDRSs s@(SDRS m _) dv = accDRSs
  where accDisVars = accessibleDRSDVs s dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [drs | (EDU drs) <- accDUs]

---------------------------------------------------------------------------
-- | helper function for accessibleDRSs
-- TODO make more effective. currently i don't know how to avoid all the recursion
-- TODO does "nub" always preserve order?
---------------------------------------------------------------------------
accessibleDRSDVs :: SDRS -> DisVar -> [DisVar]
accessibleDRSDVs s dv = nub $ walkEdges [dv]
  where walkEdges :: [DisVar] -> [DisVar]
        walkEdges []         = []
        walkEdges (dv':rest) = ss ++ cs ++ walkEdges ss ++ walkEdges rest ++ walkEdges ios
          where ss = subordLefts dv' s -- all sub relations with dv' as end node
                cs = coordLefts dv' s
                ios = case ioutscope of
                        Nothing -> []
                        Just n  -> [n]
                ioutscope = iOutscopesFrom dv' s

---------------------------------------------------------------------------
-- | Computes the right frontier of an 'SDRS'. The order of the output list
-- is from LAST to root node.
-- TODO deuglify
---------------------------------------------------------------------------
rf :: SDRS -> [DisVar]
rf s@(SDRS _ l) 
  | length lastRels == 1 &&
    all (\(Relation r _ _) -> isSub r) lastRels && -- other way to do this that doesn't rely on lists?
    M.member lastRelStart ioutscopes = walkEdges (Just l) `union` rf rewindedSDRSWithNewLast
  | otherwise                        = walkEdges (Just l)
  where ioutscopes = M.fromList $ iOutscopesMap s
        rewindedSDRSWithNewLast = updateLast rewindedSDRS rewindedLast 
        rewindedLast = inferLast rewindedSDRS
        rewindedSDRS = removeRels s lastRels
        lastRels = [rel | (rel@(Relation {})) <- calcRightArgRels s l]
        lastRelStart = head $ subordLefts l s
        walkEdges :: Maybe DisVar -> [DisVar]
        walkEdges Nothing = []
        walkEdges (Just dv) = dv : (superords [dv]) ++ walkEdges ioutscope
          where ioutscope = iOutscopesFrom dv s
        superords :: [DisVar] -> [DisVar]
        superords [] = []
        superords (dv:rest) = (case (subordLefts dv s) of
                                [] -> []
                                n  -> n ++ superords n) ++ superords rest

---------------------------------------------------------------------------
-- | Simple version of rf, used in the thesis.
---------------------------------------------------------------------------
rfSimple :: SDRS -> [DisVar]
rfSimple s@(SDRS _ last) = walkEdges (Just last)
  where walkEdges :: Maybe DisVar -> [DisVar]
        walkEdges Nothing = []
        walkEdges (Just dv) = dv : (superords [dv]) ++ walkEdges ioutscope
          where ioutscope = iOutscopesFrom dv s
        superords :: [DisVar] -> [DisVar]
        superords [] = []
        superords (dv:rest) = (case (subordLefts dv s) of
                                [] -> []
                                n  -> n ++ superords n) ++ superords rest

---------------------------------------------------------------------------
-- | Given an 'SDRS', returns a map from 'DisVar's to 'DisVar's that it outscopes.
---------------------------------------------------------------------------
iOutscopesMap :: SDRS -> [(DisVar,[DisVar])]
iOutscopesMap (SDRS m _) = M.assocs $ M.foldlWithKey build M.empty m
  where build :: (M.Map DisVar [DisVar]) -> DisVar -> SDRSFormula -> M.Map DisVar [DisVar]
        build acc dv0 (CDU (Relation _ dv1 dv2)) = M.insertWith (union) dv0 [dv1,dv2] acc
        build acc dv0 (CDU (And sf1 sf2))        = build (build acc dv0 (CDU sf2)) dv0 (CDU sf1)
        build acc dv0 (CDU (Not sf1))            = build acc dv0 (CDU sf1)
        build acc _ _                            = acc

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, returns the 'DisVar' that i-outscopes a given
-- 'DisVar' @dv@. If there is none, @dv@ represents the root node, and the
-- function returns Nothing.
---------------------------------------------------------------------------
iOutscopesFrom :: DisVar -> SDRS -> Maybe DisVar
iOutscopesFrom dv s = case (M.null ps) of
                        True  -> Nothing
                        False -> Just $ head $ M.keys ps
  where ioutscopeMap = M.fromList $ iOutscopesMap s
        ps = M.filter (elem dv) ioutscopeMap

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@ and a 'DisVar' @dv@, returns the transitive closure
-- of iOutscopesFrom, i.e., all variables that outscope @dv@.
---------------------------------------------------------------------------
outscopesFrom :: DisVar -> SDRS -> [DisVar]
outscopesFrom dv s = outscope dv
  where outscope :: DisVar -> [DisVar]
        outscope dv' = case newOutscope of
                          Nothing   -> []
                          Just dv'' -> dv'' : outscope dv''
          where newOutscope = iOutscopesFrom dv' s

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
                getNext (And _ cdu2)        = getNext cdu2
                getNext (Not cdu1)          = getNext cdu1

---------------------------------------------------------------------------
-- | Checks whether a given 'DisVar' @dv@ is on the right frontier of 
-- 'SDRS' @s@.
---------------------------------------------------------------------------
isOnRF :: SDRS -> DisVar -> Bool
isOnRF s dv  = dv `elem` rf s

---------------------------------------------------------------------------
-- | Return the root node of the 'SDRS' @s@. If @s@ has more than one root,
-- only the first in the list will be returned. 
---------------------------------------------------------------------------
root :: SDRS -> DisVar
root s 
  | null cduDVs = head $ M.keys $ sdrsMap s -- if the SDRS is a singleton, then there are no outscopes, thus get the only label
  | otherwise   = head $ [ d | d <- cduDVs, iOutscopesFrom d s == Nothing]
  where cduDVs = M.keys $ M.fromList $ iOutscopesMap s

---------------------------------------------------------------------------
-- | Checks whether 'DisVar' @dv@ in 'SDRS' @s@ is the root node.
---------------------------------------------------------------------------
isRoot :: SDRS -> DisVar -> Bool
isRoot s dv = dv == root s

---------------------------------------------------------------------------
-- | Checks whether 'DisVar' @dv@ in 'SDRS' @s@ has any incoming edges.
---------------------------------------------------------------------------
hasParents :: SDRS -> DisVar -> Bool
hasParents (SDRS m _) dv = any incoming $ M.elems m
  where incoming :: SDRSFormula -> Bool
        incoming (CDU (Relation _ _ dv2)) = dv == dv2
        incoming (CDU (And sf1 sf2))        = incoming (CDU sf1) || incoming (CDU sf2)
        incoming (CDU (Not sf1))            = incoming (CDU sf1)
        incoming (EDU _)          = False
