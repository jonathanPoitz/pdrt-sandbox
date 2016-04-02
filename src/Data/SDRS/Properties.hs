{- |
Module      :  Data.SDRS.Properties
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com me@hbrouwer.eu, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS properties
-}

module Data.SDRS.Properties
(
  properDRS
, sdrsProperDRSs
, pureDRS
, sdrsPureDRSs
, isPureSDRS
) where

import Data.SDRS.DataType
import Data.DRS.Properties
import Data.DRS.Merge
import Data.DRS.Structure
import qualified Data.Map as M
import Data.List (nub)
import Data.SDRS.Structure (segments, drss)
import Data.SDRS.DiscourseGraph

---------------------------------------------------------------------------
-- | Checks, given the global SDRS, if an embedded 'DRS' @d@ is /proper/,
-- where: ['DRS' @d@ is proper /iff/]
--
--  * @d@ does not contain any free variables
---------------------------------------------------------------------------
properDRS :: SDRS -> DisVar -> DRS -> Bool
properDRS s@(SDRS m _) dv d = isProperDRS (d <<+>> (foldl (<<+>>) (DRS [] []) accDRSs)) -- is merging with empty DRS the only way for this?
  where graph = buildDGraph s
        accDisVars = accessibleNodes graph dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [ drs | (Segment drs) <- accDUs]

-- ---------------------------------------------------------------------------
-- -- | Checks, given an 'SDRS', whether all embedded DRSs are proper
-- ---------------------------------------------------------------------------
-- sdrsProperDRSs :: SDRS -> Bool
-- sdrsProperDRSs s = proper $ segments s
--   where proper :: [(DisVar, SDRSFormula)] -> Bool
--         proper []                    = True
--         proper ((dv,Segment d):rest) = (properDRS s dv d) && proper rest
--         proper ((_,_):rest)          = proper rest

---------------------------------------------------------------------------
-- | Debug version printing out all booleans
---------------------------------------------------------------------------
sdrsProperDRSs :: SDRS -> [Bool]
sdrsProperDRSs s = proper $ segments s
  where proper :: [(DisVar, SDRSFormula)] -> [Bool]
        proper []                    = []
        proper ((dv,Segment d):rest) = (properDRS s dv d) : proper rest
        proper ((_,_):rest)          = proper rest

---------------------------------------------------------------------------
-- | Checks, given the global SDRS, if an embedded 'DRS' @d@ is /pure/,
-- where: ['DRS' @d@ is pure /iff/]
--
--  * @d@ does not contain any otiose declarations of discourse referents
--    (i.e., @d@ does not contain any unbound, duplicate uses of referents).
---------------------------------------------------------------------------
pureDRS :: SDRS -> DisVar -> DRS -> Bool
pureDRS s@(SDRS m _) dv d = isPureDRS (d <<+>> (foldl (<<+>>) (DRS [] []) accDRSs)) -- is merging with empty DRS the only way for this?
  where graph = buildDGraph s
        accDisVars = accessibleNodes graph dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [ drs | (Segment drs) <- accDUs]

---------------------------------------------------------------------------
-- | Checks, given an 'SDRS', whether all embedded DRSs are pure
---------------------------------------------------------------------------
sdrsPureDRSs :: SDRS -> Bool
sdrsPureDRSs s = pure' $ segments s
  where pure' :: [(DisVar, SDRSFormula)] -> Bool
        pure' []                    = True
        pure' ((dv,Segment d):rest) = (pureDRS s dv d) && pure' rest
        pure' ((_,_):rest)          = pure' rest

-----------------------------------------------------------------------------
---- | Debug version of normal function
-----------------------------------------------------------------------------
--sdrsPureDRSs :: SDRS -> [Bool]
--sdrsPureDRSs s = pure' $ segments s
--  where pure' :: [(DisVar, SDRSFormula)] -> [Bool]
--        pure' []                    = []
--        pure' ((dv,Segment d):rest) = (pureDRS s dv d) : pure' rest
--        pure' ((_,_):rest)          = pure' rest

---------------------------------------------------------------------------
-- | Checks if the 'SDRS' @s@ is /pure/, where:
-- ['SDRS' @s@ is pure /iff/]
--
--  * no embedded 'DRS' declares 'DRSRef's that are declared in any other
-- embedded 'DRS' of @s@. 
---------------------------------------------------------------------------
isPureSDRS :: SDRS -> Bool
isPureSDRS s = universes == nub universes
  where universes = concat $ map drsUniverse (drss s)







