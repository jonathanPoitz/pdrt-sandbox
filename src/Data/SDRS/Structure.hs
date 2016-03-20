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
, getDu
, buildOutscopeMap
) where

import Data.SDRS.DataType
import Data.SDRS.Binding
import qualified Data.Map as Map
import qualified Data.Set as Set

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns the set of discourse units
---------------------------------------------------------------------------
listDUs :: SDRS -> [DisVar]
listDUs (SDRS _ m _)     = Map.keys m

---------------------------------------------------------------------------
-- | Returns a certain discourse unit if present
---------------------------------------------------------------------------
getDu :: SDRS -> DisVar -> Maybe SDRSFormula
getDu (SDRS _ m _) i     = Map.lookup i m

---------------------------------------------------------------------------
-- | Returns, given an 'SDRS' @s@, in what dominance relation two 'DisVar'
-- @dv@ and @dv'@ stand. This relation is represented by an 'Ordering'.
-- @dv@ 'GT' @dv'@ if the former outscopes the latter (vice versa for
-- 'LT'). 'EQ' if no dominance relation is present.
---------------------------------------------------------------------------
-- checkOutscopes :: SDRS -> DisVar -> DisVar -> Bool
-- checkOutscopes s@(SDRS a m l) dv dv'
--   | checkNoUnboundVars s = getRelsOutscopes (Map.assocs m) dv dv'
--   | otherwise = Nothing
--     where getRelsOutscopes :: [(DisVar, SDRSFormula)] -> DisVar -> DisVar -> Bool
--           getRelsOutscopes [] _ _ = False
--           getRelsOutscopes (dv0, And sf1 sf2):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getOneRelOutscopes dv0 sf2 dv dv' || getRelsOutscopes rest dv dv'
--           getRelsOutscopes (dv0, Not sf1):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getRelsOutscopes rest dv dv'
--           getRelsOutscopes (dv0, sf1@(Relation _ _)):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getRelsOutscopes rest dv dv'
--           -- TODO do i have to pass on dv and dv' here or can i leave it and it's implicit?
--           getRelsOutscopes _:rest = getRelsOutscopes rest
--             where getOneRelOutscopes :: DisVar -> SDRSFormula -> DisVar -> DisVar -> Bool
--                   getOneRelOutscopes dv0 (Relation _ dv1 dv2) dv dv' -- in the following the immediate outscopes cases
--                    | List.sort [dv, dv'] == List.sort [dv1, dv2] = False -- dv and dv' are args of a rel. no outscoping
--                    | dv == dv0 && ( dv' == dv1 || dv' == dv2 ) = True -- dv outscopes dv'
--                    | dv' == dv0 && ( dv == dv1 || dv == dv2 ) = False -- dv' outscopes dv
--                    -- here it gets tricky because I have to find a path along which the two labels lie.
--                    -- TODO

buildOutscopeMap :: SDRS -> (Map.Map DisVar (Set.Set DisVar))
buildOutscopeMap s@(SDRS _ m _)
  | checkNoUnboundVars s = Map.foldlWithKey buildOutscopeMapAux Map.empty m
  | otherwise = Map.empty
    where buildOutscopeMapAux :: (Map.Map DisVar (Set.Set DisVar)) -> DisVar -> SDRSFormula -> (Map.Map DisVar (Set.Set DisVar))
          buildOutscopeMapAux acc dv0 (Relation _ dv1 dv2) = Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2]) acc
          buildOutscopeMapAux acc dv0 (And (Relation _ dv1 dv2) (Relation _ dv3 dv4)) = (Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2])) ((Map.insertWith (Set.union) dv0 (Set.fromList [dv3,dv4])) acc)
          buildOutscopeMapAux acc dv0 (Not (Relation _ dv1 dv2)) = (Map.insertWith (Set.union) dv0 (Set.fromList [dv1,dv2]) acc)
          buildOutscopeMapAux acc _ _ = acc

-- how two labels can outscope eachother. Pseudo code using Ordering to show outscoping. EQ indicates that they're on the same level
-- outscoping 1 2:
-- 1 2 arguments of same relation -> EQ
-- 1:rel(2,x) -> GT
-- 2:rel(1,x) -> LT
-- ^ immediate outscoping
-- if exists path from 1 to 2 along relations, then 1 GT 2
