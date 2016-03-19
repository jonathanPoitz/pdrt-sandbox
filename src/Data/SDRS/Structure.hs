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
, checkOutscopes
) where

import Data.SDRS.DataType
import Data.SDRS.Binding
import qualified Data.Map as Map
import qualified Data.List as List

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
checkOutscopes :: SDRS -> DisVar -> DisVar -> Bool
checkOutscopes s@(SDRS a m l) dv dv'
  | checkNoUnboundVars s = getRelsOutscopes (Map.assocs m) dv dv'
    where getRelsOutscopes :: [(DisVar, SDRSFormula)] -> DisVar -> DisVar -> Bool
          getRelsOutscopes [] _ _ = False
          getRelsOutscopes (dv0, And sf1 sf2):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getOneRelOutscopes dv0 sf2 dv dv' || getRelsOutscopes rest dv dv'
          getRelsOutscopes (dv0, Not sf1):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getRelsOutscopes rest dv dv'
          getRelsOutscopes (dv0, sf1@(Relation _ _)):rest dv dv' = getOneRelOutscopes dv0 sf1 dv dv' || getRelsOutscopes rest dv dv'
          -- TODO do i have to pass on dv and dv' here or can i leave it and it's implicit?
          getRelsOutscopes _:rest = getRelsOutscopes rest
            where getOneRelOutscopes :: DisVar -> SDRSFormula -> DisVar -> DisVar -> Bool
                  getOneRelOutscopes dv0 (Relation _ dv1 dv2) dv dv' -- in the following the immediate outscopes cases
                   | List.sort [dv, dv'] == List.sort [dv1, dv2] = False -- dv and dv' are args of a rel. no outscoping
                   | dv == dv0 && ( dv' == dv1 || dv' == dv2 ) = True -- dv outscopes dv'
                   | dv' == dv0 && ( dv == dv1 || dv == dv2 ) = False -- dv' outscopes dv
                   -- here it gets tricky because I have to find a path along which the two labels lie.
                   -- TODO
  | otherwise = Nothing

-- buildOutscopesStruc :: SDRS -> [[Int]]
-- buildOutscopesStruc s@(SDRS a m l)
--   | checkNoUnboundVars s = buildStruc (Map.assocs m)
--     where buildStruc :: [(DisVar, SDRSFormula)] -> [[Int]]
--           buildStruc [] = []
--           buildStruc (dv0, And sf1 sf2):rest = Map.foldlWithKey f [] m 
--           -- TODO
--   | otherwise = [[]]

-- f :: [[DisVar]] -> DisVar -> SDRSFormula -> [[DisVar]]
-- f acc dv0 v@(Relation _ dv1 dv2)
--   | acc == [] = [[dv0],[dv1, dv2]]
--   | otherwise = acc.insert

-- how two labels can outscope eachother. Pseudo code using Ordering to show outscoping. EQ indicates that they're on the same level
-- outscoping 1 2:
-- 1 2 arguments of same relation -> EQ
-- 1:rel(2,x) -> GT
-- 2:rel(1,x) -> LT
-- ^ immediate outscoping
-- if exists path from 1 to 2 along relations, then 1 GT 2
