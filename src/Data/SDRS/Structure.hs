{- |
Module      :  Data.SDRS.Structure
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

Structural operations on SDRSs
-}

module Data.SDRS.Structure
(
  listDUs
, lookupDU
, buildOutscopeMap
, expandRecursiveFormula
) where

import Data.SDRS.DataType
import qualified Data.List as List 
import qualified Data.Map as Map
-- import qualified Data.Set as Set

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns the set of discourse units
---------------------------------------------------------------------------
listDUs :: SDRS -> [DisVar]
listDUs (SDRS m _)     = Map.keys m

---------------------------------------------------------------------------
-- | Returns a certain discourse unit if present
---------------------------------------------------------------------------
lookupDU :: SDRS -> DisVar -> Maybe SDRSFormula
lookupDU (SDRS m _) i     = Map.lookup i m

---------------------------------------------------------------------------
-- | Returns, given an 'SDRS' @s@, in what dominance relation two 'DisVar'
-- @dv@ and @dv'@ stand. This relation is represented by an 'Ordering'.
-- @dv@ 'GT' @dv'@ if the former outscopes the latter (vice versa for
-- 'LT'). 'EQ' if no dominance relation is present.
---------------------------------------------------------------------------
-- checkOutscopes :: SDRS -> DisVar -> DisVar -> Maybe Bool
-- checkOutscopes s@(SDRS a m l) dv dv'
--   | checkNoUnboundVars s = getRelsOutscopes (Map.assocs $ buildOutscopeMap s) dv dv'
--   | otherwise = Nothing
--     where getRelsOutscopes :: [(DisVar, [DisVar]])] -> DisVar -> DisVar -> Bool
--           getRelsOutscopes [] _ _ = Nothing
--           getRelsOutscopes allrels@((dv0, dv1s):_) dv dv'
--             | dv `elem` dv1s && dv' `elem` dv1s = Nothing -- dv and dv' are args of a rel. no outscoping
--             | dv == dv0 && dv' `elem` dv1s = Just True -- dv immediately outscopes dv'
--             | dv' == dv0 && dv `elem` dv1s = Just False -- dv' immediately outscopes dv
--             | dv `elem` (map fst allrels) = findDVFromKeys [dv] allrels True -- dv is labeling a relation, look along its path
--             | dv' `elem` (map fst allrels) = findDVFromKeys [dv'] allrels False -- dv' is labeling a relation, look along its path
--             | otherwise = findPathWithoutKey allrels -- neither dv nor dv'
--               where findDVFromKeys :: [DisVar] -> [(DisVar, [DisVar])] -> DisVar -> Bool -> Maybe Bool
--                     findDVFromKeys [] _ _ _ = Nothing
--                     findDVFromKeys dvKey:[] allrels1 dvFind outBool
--                       -- I want to call the findDVFromKeys method 
--                       | dvFind `notElem` (Map.lookup dvKey allrels1) = findDVFromKeys (Map.lookup dvKey allrels1) allrels1 dvFind
--                       | otherwise = Just outBool
--                     findDVFromKeys dvKey:rest allrels1 dvFind outBool





-- checkOutscopes :: SDRS -> DisVar -> DisVar -> Maybe Bool
-- checkOutscopes s@(SDRS a m l) dv dv'
--   | checkNoUnboundVars s = getRelsOutscopes (buildOutscopeMap s) dv dv'
--   | otherwise = Nothing
--     where getRelsOutscopes :: (Map.Map DisVar [DisVar]) -> DisVar -> DisVar -> Bool
--           getRelsOutscopes Map.empty _ _ = Nothing
--           getRelsOutscopes map dv dv'
--             | dv `elem` (snd(Map.elemAt 0 map)) && dv' `elem` (snd(Map.elemAt 0 map)) = Nothing -- dv and dv' are args of a rel. no outscoping
--             | -- TODO continue here
--             | dv == fst(Map.elemAt 0 map) = findDVFromKeys [dv] allrels True -- dv is labeling a relation, look along its path
--             | dv' `elem` (map fst allrels) = findDVFromKeys [dv'] allrels False -- dv' is labeling a relation, look along its path
--             | otherwise = findPathWithoutKey allrels -- neither dv nor dv'
--               where findDVFromKeys :: [DisVar] -> [(DisVar, [DisVar])] -> DisVar -> Bool -> Maybe Bool
--                     findDVFromKeys [] _ _ _ = Nothing
--                     findDVFromKeys dvKey:[] allrels1 dvFind outBool
--                       -- I want to call the findDVFromKeys method 
--                       | dvFind `notElem` (Map.lookup dvKey allrels1) = findDVFromKeys (Map.lookup dvKey allrels1) allrels1 dvFind
--                       | otherwise = Just outBool
--                     findDVFromKeys dvKey:rest allrels1 dvFind outBool

            

-- buildOutscopeMap :: SDRS -> (Map.Map DisVar (Set.Set DisVar))
-- buildOutscopeMap s@(SDRS _ m _)
--   | checkNoUnboundVars s = Map.foldlWithKey buildOutscopeMapAux Map.empty m
--   | otherwise = Map.empty
--     where buildOutscopeMapAux :: (Map.Map DisVar (Set.Set DisVar)) -> DisVar -> SDRSFormula -> (Map.Map DisVar (Set.Set DisVar))
--           buildOutscopeMapAux acc dv0 (Relation _ dv1 dv2) = Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2]) acc
--           buildOutscopeMapAux acc dv0 (And (Relation _ dv1 dv2) (Relation _ dv3 dv4)) = (Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2])) ((Map.insertWith (Set.union) dv0 (Set.fromList [dv3,dv4])) acc)
--           buildOutscopeMapAux acc dv0 (Not (Relation _ dv1 dv2)) = (Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2]) acc)
--           buildOutscopeMapAux acc _ _ = acc

---------------------------------------------------------------------------
-- | Returns, given an SDRS, a map that represents which of its discourse 
-- 
---------------------------------------------------------------------------
buildOutscopeMap :: SDRS -> (Map.Map DisVar [DisVar])
buildOutscopeMap (SDRS m _) = Map.foldlWithKey build Map.empty m
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

---------------------------------------------------------------------------
-- | given an SDRSFormula, returns a list of all of its embedded SDRSFormulae 
---------------------------------------------------------------------------
expandRecursiveFormula :: SDRSFormula -> [SDRSFormula]
expandRecursiveFormula (Not sf1) = expandRecursiveFormula sf1
expandRecursiveFormula (And sf1 sf2) = expandRecursiveFormula sf1 ++ expandRecursiveFormula sf2
expandRecursiveFormula sf = [sf]


-- how two labels can outscope eachother. Pseudo code using Ordering to show outscoping. EQ indicates that they're on the same level
-- outscoping 1 2:
-- 1 2 arguments of same relation -> EQ
-- 1:rel(2,x) -> GT
-- 2:rel(1,x) -> LT
-- ^ immediate outscoping
-- if exists path from 1 to 2 along relations, then 1 GT 2
