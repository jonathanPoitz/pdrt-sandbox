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
, Label
, RelType (..)
, relationFromLabel
, extractCDU
, module Data.DRS.DataType
) where

import Data.DRS.DataType
import Data.Char (toLower)
import qualified Data.Map as M

---------------------------------------------------------------------------
-- | Show a 'DRS' in 'Debug' notation.
--
-- Note: This is a workaround for bootstrapping the SDRT notations. We will
-- take care of this in a proper way in the future.
---------------------------------------------------------------------------

--instance Show DRS where show d = showDRSDebug d

--showDRSDebug :: DRS -> String
--showDRSDebug (LambdaDRS l) = "LambdaPDRS" ++ " "  ++ show l
--showDRSDebug (Merge d1 d2) = "Merge"      ++ " (" ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
--showDRSDebug (DRS u c)     = "DRS"        ++ " "  ++ show u ++ " [" ++ intercalate "," (map showCon c) ++ "]"
--  where showCon :: DRSCon -> String
--        showCon (Rel r d)    = "Rel ("     ++ show r          ++ ")" ++ " " ++ show d
--        showCon (Neg d1)     = "Neg ("     ++ showDRSDebug d1 ++ ")"
--        showCon (Imp d1 d2)  = "Imp ("     ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
--        showCon (Or d1 d2)   = "Or ("      ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
--        showCon (Box d1)     = "Box ("     ++ showDRSDebug d1 ++ ")"
--        showCon (Diamond d1) = "Diamond (" ++ showDRSDebug d1 ++ ")"
--        showCon (Prop r d1)  = "Prop ("    ++ show r          ++ ") (" ++ showDRSDebug d1 ++ ")"

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | A relation in SDRS carries its name, the relation type (Coordinating
-- vs. Subordinating), a boolean indicating whether it is a structured 
-- relation, and a boolean indicating whether it imposes a topic constraint
-- on the discourse
---------------------------------------------------------------------------
data SDRSRelation =
  SDRSRelation { label :: Label
               , relType :: RelType
               , isStructured :: Bool
               , isTopic :: Bool
               }
  | Outscopes deriving (Eq, Read)

-- | Outscopes symbol
opOutsc :: String
opOutsc = "\8827"

---------------------------------------------------------------------------
-- | Derive an instance of the 'Show' typeclass for 'SDRSRelation'.
---------------------------------------------------------------------------
instance Show SDRSRelation where
  show r@(SDRSRelation {}) = label r
  show Outscopes = opOutsc

---------------------------------------------------------------------------
-- | The type of relation (Crd = Coordinating, Sub = Subordinating or None for outscoping relation)
---------------------------------------------------------------------------
data RelType = Crd | Sub
  deriving (Eq, Read)

---------------------------------------------------------------------------
-- | given a relation name, returns the SDRSRelation with that name
---------------------------------------------------------------------------
relationFromLabel :: Label -> SDRSRelation
relationFromLabel l = if (length outRels > 1) || (length outRels == 0)
                      then error "Error finding relation"
                      else outRels !! 0
  where outRels = filter (\r -> show r == (filter (/=' ') (map toLower l))) relations

---------------------------------------------------------------------------
-- | Discourse variable denoting a simple or complex speech act discourse  
-- referent (a label or pointer). 
---------------------------------------------------------------------------
type DisVar = Int

---------------------------------------------------------------------------
-- | An SDRS formula
---------------------------------------------------------------------------
data SDRSFormula = 
  EDU DRS
  | CDU CDU
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | Extracts the 'CDU' from a given 'SDRSFormula' @sf@ or throws an error
-- if @sf@ is not created from a 'CDU'.
---------------------------------------------------------------------------
extractCDU :: SDRSFormula -> CDU
extractCDU (CDU cdu) = cdu
extractCDU _ = error ("Cannot extract non-CDU SDRSFormula")

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
-- | Segmented Discourse Representation Structure.
---------------------------------------------------------------------------
data SDRS =
  SDRS (M.Map DisVar SDRSFormula) DisVar
  -- ^ An SDRS (a map assigning SDRS-formulas to discourse variables and the
  -- pointer to the last added element to the discourse)
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | relation label
---------------------------------------------------------------------------
type Label = String

---------------------------------------------------------------------------
-- | relation types
---------------------------------------------------------------------------
relations :: [SDRSRelation]
relations = [SDRSRelation "elaboration" Sub False False,
             SDRSRelation "entity_elaboration" Sub False False,
             SDRSRelation "comment" Sub False False,
             SDRSRelation "flashback" Sub False False,
             SDRSRelation "background" Sub False False,
             SDRSRelation "goal" Sub False False,
             SDRSRelation "explanation" Sub False False,
             SDRSRelation "attribution" Sub False False,
             SDRSRelation "narration" Crd False True,
             SDRSRelation "contrast" Crd True False,
             SDRSRelation "result" Crd False False,
             SDRSRelation "parallel" Crd True False,
             SDRSRelation "continuation" Crd False True,
             SDRSRelation "alternation" Crd False False,
             SDRSRelation "consequence" Crd False False,
             Outscopes]
