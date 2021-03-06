{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      :  Data.SDRS.DataType
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, njvenhuizen@gmail.com
Stability   :  provisional
Portability :  portable

SDRS data type
-}

module Data.SDRS.DataType
(
  SDRS (..)
, SDRSFormula (..)
, SDRSRelation (..)
, CDU (..)
, DisVar
, RelName
, RelType (..)
, extractCDU
, relationFromRelName
, module Data.DRS.DataType
) where

import Data.Char (toLower)
import qualified Data.Map as M

import Data.DRS.DataType

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Segmented Discourse Representation Structure.
---------------------------------------------------------------------------
data SDRS =
  SDRS (M.Map DisVar SDRSFormula) DisVar
  -- ^ An SDRS (a map assigning SDRS-formulas to discourse variables and the
  -- pointer to the last added element to the discourse)
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | Discourse variable denoting a simple or complex speech act discourse  
-- referent.
---------------------------------------------------------------------------
type DisVar = Int

---------------------------------------------------------------------------
-- | An SDRS-Formula
---------------------------------------------------------------------------
data SDRSFormula = 
  EDU DRS
  | CDU CDU
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | A complex discourse unit, which can be recursively combined using
-- conjunction or negation.
---------------------------------------------------------------------------
data CDU =
  Relation SDRSRelation DisVar DisVar
  -- ^ A rhetorical relation between two speech act discourse referents
  | And CDU CDU
  | Not CDU
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | A relation in SDRS carries its name, the relation type (Coordinating
-- vs. Subordinating), a boolean indicating whether it is a structured 
-- relation, and a boolean indicating whether it imposes a topic constraint
-- on the discourse
---------------------------------------------------------------------------
data SDRSRelation =
  SDRSRelation { relName :: RelName
               , relType :: RelType
               , isStructured :: Bool
               , isTopic :: Bool
               } deriving (Eq, Read)

---------------------------------------------------------------------------
-- | The type of relation (Crd = Coordinating, Sub = Subordinating or None for outscoping relation)
---------------------------------------------------------------------------
data RelType = Crd | Sub
  deriving (Eq, Read)

---------------------------------------------------------------------------
-- | Relation name
---------------------------------------------------------------------------
type RelName = String

---------------------------------------------------------------------------
-- | Derive an instance of the 'Show' typeclass for 'SDRSRelation'.
---------------------------------------------------------------------------
instance Show SDRSRelation where
  show r@(SDRSRelation {}) = relName r
  --show Outscopes = opOutsc

---------------------------------------------------------------------------
-- | Extracts the 'CDU' from a given 'SDRSFormula' @sf@ or throws an error
-- if @sf@ is not created from a 'CDU'.
---------------------------------------------------------------------------
extractCDU :: SDRSFormula -> CDU
extractCDU (CDU cdu) = cdu
extractCDU _ = error ("Cannot extract non-CDU SDRSFormula")

---------------------------------------------------------------------------
-- | Given a relation name, returns the 'SDRSRelation' with that name.
---------------------------------------------------------------------------
relationFromRelName :: RelName -> SDRSRelation
relationFromRelName l = if (length outRels > 1) || (length outRels == 0)
                      then error "Error finding relation"
                      else outRels !! 0
  where outRels = filter (\r -> show r == (filter (/=' ') (map toLower l))) relations

---------------------------------------------------------------------------
-- * Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | relation types
---------------------------------------------------------------------------
relations :: [SDRSRelation]
--                         label reltype isStruc isTopic          
relations = [
             SDRSRelation "explanation" Sub False False,
             SDRSRelation "background" Sub False True,
             SDRSRelation "elaboration" Sub False False,
             SDRSRelation "entity_elaboration" Sub False False,
             SDRSRelation "comment" Sub False False,
             SDRSRelation "flashback" Sub False False,
             SDRSRelation "goal" Sub False False,
             SDRSRelation "attribution" Sub False False,
             SDRSRelation "consequence" Crd False False,
             SDRSRelation "alternation" Crd False False,
             SDRSRelation "result" Crd False False,
             SDRSRelation "parallel" Crd True False,
             SDRSRelation "contrast" Crd True False,
             SDRSRelation "narration" Crd False True,
             SDRSRelation "continuation" Crd False True
            ]
