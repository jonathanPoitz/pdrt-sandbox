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
  sdrsProperDRSs
, sdrsPureDRSs
, sdrsAllDRSRefUnique
, allRelationsValid
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
-- | Checks, given an 'SDRS', whether all embedded DRSs are proper
---------------------------------------------------------------------------
sdrsProperDRSs :: SDRS -> Bool
sdrsProperDRSs s@(SDRS m _) = proper $ segments s
 where proper :: [(DisVar, SDRSFormula)] -> Bool
       proper []                    = True
       proper ((dv,Segment d):rest) = (properDRS dv d) && proper rest
       proper ((_,_):rest)          = proper rest
       properDRS :: DisVar -> DRS -> Bool
       properDRS dv d = isProperDRS ((foldl (<<+>>) (DRS [] []) accDRSs) <<+>> d) -- is merging with empty DRS the only way for this?
         where accDisVars = accessibleNodes s dv
               accDUs = map (\i -> m M.! i) accDisVars
               accDRSs = [ drs | (Segment drs) <- accDUs]
       

---------------------------------------------------------------------------
-- | Checks, given an 'SDRS', whether all embedded 'DRS's are pure
---------------------------------------------------------------------------
sdrsPureDRSs :: SDRS -> Bool
sdrsPureDRSs s@(SDRS m _) = pure' $ segments s
  where pure' :: [(DisVar, SDRSFormula)] -> Bool
        pure' []                    = True
        pure' ((dv,Segment d):rest) = (pureDRS dv d) && pure' rest
        pure' ((_,_):rest)          = pure' rest
        pureDRS :: DisVar -> DRS -> Bool
        pureDRS dv d = isPureDRS ((foldl (<<+>>) (DRS [] []) accDRSs) <<+>> d) -- is merging with empty DRS the only way for this?
          where accDisVars = accessibleNodes s dv
                accDUs = map (\i -> m M.! i) accDisVars
                accDRSs = [ drs | (Segment drs) <- accDUs]

---------------------------------------------------------------------------
-- | Checks if the 'SDRS' @s@ is /pure/, where:
-- ['SDRS' @s@ is pure /iff/]
--
--  * no embedded 'DRS' declares 'DRSRef's that are declared in any other
-- embedded 'DRS' of @s@. 
---------------------------------------------------------------------------
sdrsAllDRSRefUnique :: SDRS -> Bool
sdrsAllDRSRefUnique s = universes == nub universes
  where universes = concat $ map drsUniverse (drss s)

---------------------------------------------------------------------------
-- | Checks, given an 'SDRS', whether all embedded relations are valid in SDRT
---------------------------------------------------------------------------
allRelationsValid :: SDRS -> Bool
allRelationsValid (SDRS m _) = all isRelation allRelationLabels
  where allSDRSFormulae = map snd $ M.assocs m
        allRelationLabels = [ l | Relation l _ _ <- allSDRSFormulae]


