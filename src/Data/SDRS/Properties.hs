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
) where

import Data.SDRS.DataType
import Data.DRS.Properties
import Data.DRS.Merge
import qualified Data.Map as M
import Data.SDRS.Structure (segments)
import Data.SDRS.DiscourseGraph

---------------------------------------------------------------------------
-- | Checks, given the global SDRS, if an embedded 'DRS' @d@ is /proper/,
-- where: ['DRS' @d@ is proper /iff/]
--
--  * @d@ does not contain any free variables
---------------------------------------------------------------------------
properDRS :: SDRS -> DisVar -> DRS -> Bool
properDRS s@(SDRS m _) dv d = isProperDRS (d <<+>> mergedAccDRSs)
  where graph = buildDGraph s
        accDisVars = accessibleNodes graph dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [ drs | (Segment drs) <- accDUs]
        recMerge :: [DRS] -> DRS
        recMerge []        = DRS [] [] -- ugly. but ghc says I have to be exhaustive in my pattern matches. workaround?
        recMerge (d':[])   = d'
        recMerge (d':rest) = d' <<+>> recMerge rest
        mergedAccDRSs = recMerge accDRSs

---------------------------------------------------------------------------
-- | Checks, given an 'SDRS', whether all embedded DRSs are proper
---------------------------------------------------------------------------
sdrsProperDRSs :: SDRS -> Bool
sdrsProperDRSs s = proper $ segments s
  where proper :: [(DisVar, SDRSFormula)] -> Bool
        proper []                    = True
        proper ((dv,Segment d):rest) = (properDRS s dv d) && proper rest
        proper ((_,_):rest)          = proper rest

-- ---------------------------------------------------------------------------
-- -- | Debug version printing out all booleans
-- ---------------------------------------------------------------------------
-- sdrsProperDRSs :: SDRS -> [Bool]
-- sdrsProperDRSs s = proper $ segments s
--   where proper :: [(DisVar, SDRSFormula)] -> [Bool]
--         proper []                    = []
--         proper ((dv,Segment d):rest) = (properDRS s dv d) : proper rest
--         proper ((_,_):rest)          = proper rest

---------------------------------------------------------------------------
-- | Checks, given the global SDRS, if an embedded 'DRS' @d@ is /pure/,
-- where: ['DRS' @d@ is pure /iff/]
--
--  * @d@ does not contain any otiose declarations of discourse referents
--    (i.e., @d@ does not contain any unbound, duplicate uses of referents).
---------------------------------------------------------------------------
pureDRS :: SDRS -> DisVar -> DRS -> Bool
pureDRS s@(SDRS m _) dv d = isPureDRS (d <<+>> mergedAccDRSs)
  where graph = buildDGraph s
        accDisVars = accessibleNodes graph dv
        accDUs = map (\i -> m M.! i) accDisVars
        accDRSs = [ drs | (Segment drs) <- accDUs]
        recMerge :: [DRS] -> DRS
        recMerge []        = DRS [] [] -- ugly. but ghc says I have to be exhaustive in my pattern matches. workaround?
        recMerge (d':[])   = d'
        recMerge (d':rest) = d' <<+>> recMerge rest
        mergedAccDRSs = recMerge accDRSs

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
