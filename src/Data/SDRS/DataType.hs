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
, DisVar
, RelName
, module Data.DRS.DataType
) where

import Data.DRS.DataType

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
-- | Discourse variable denoting a simple or complex speech act discourse  
-- referent (a label or pointer). 
---------------------------------------------------------------------------
type DisVar = Int

---------------------------------------------------------------------------
-- | Name of a rhetorical relation 
---------------------------------------------------------------------------
type RelName = String

---------------------------------------------------------------------------
-- | A rhetorical relation, which can be either coordinating or subordinating
---------------------------------------------------------------------------
--data RelLabel =
--  CrdRel RelName
--  | SubRel RelName
--  deriving (Show, Read Eq)

---------------------------------------------------------------------------
-- | An SDRS formula
---------------------------------------------------------------------------
data SDRSFormula =
  Segment DRS
-- ^ A DRS
  | Relation RelName DisVar DisVar
-- ^ A rhetorical relation between two speech act discourse referents
  | And SDRSFormula SDRSFormula
  | Not SDRSFormula
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | Segmented Discourse Representation Structure.
---------------------------------------------------------------------------

data SDRS =
  SDRS (M.Map DisVar SDRSFormula) DisVar
  -- ^ A SDRS (a set of speech act discourse referents, a map assigning
  -- SDRS formulas to referents and the referent last added to the discourse)
  deriving (Read, Eq)

---------------------------------------------------------------------------
-- | Segmented Discourse Representation Structure
-- alternative version using association lists instead of Data.Map (saves
-- the fromList() call, but lacks some convenience methods)
---------------------------------------------------------------------------

--data SDRS =
--  SDRS [(DisVar,SDRSFormula)] DisVar
---- ^ A SDRS (a set of speech act discourse referents, a function assigning
---- SDRS formulas to referents and the referent last added to the discourse)
--  deriving (Show, Read, Eq)
