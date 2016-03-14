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
, SDRSForm (..)
, SDRSCon (..)
, DisVar
-- , CoRel (..)
-- , SubRel (..)
, RelLabel (..)
-- TODO should I export everything by default or be strict about what to export?
--, sdrsFormulaMapFunction
, module Data.DRS.DataType
) where

import Data.DRS.DataType

import Data.List (intercalate)
import qualified Data.Map as Map

---------------------------------------------------------------------------
-- | Show a 'DRS' in 'Debug' notation.
--
-- Note: This is a workaround for bootstrapping the SDRT notations. We will
-- take care of this in a proper way in the future.
---------------------------------------------------------------------------

instance Show DRS where show d = showDRSDebug d

showDRSDebug :: DRS -> String
showDRSDebug (LambdaDRS l) = "LambdaPDRS" ++ " "  ++ show l
showDRSDebug (Merge d1 d2) = "Merge"      ++ " (" ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
showDRSDebug (DRS u c)     = "DRS"        ++ " "  ++ show u ++ " [" ++ intercalate "," (map showCon c) ++ "]"
  where showCon :: DRSCon -> String
        showCon (Rel r d)    = "Rel ("     ++ show r          ++ ")" ++ " " ++ show d
        showCon (Neg d1)     = "Neg ("     ++ showDRSDebug d1 ++ ")"
        showCon (Imp d1 d2)  = "Imp ("     ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
        showCon (Or d1 d2)   = "Or ("      ++ showDRSDebug d1 ++ ") (" ++ showDRSDebug d2 ++ ")"
        showCon (Box d1)     = "Box ("     ++ showDRSDebug d1 ++ ")"
        showCon (Diamond d1) = "Diamond (" ++ showDRSDebug d1 ++ ")"
        showCon (Prop r d1)  = "Prop ("    ++ show r          ++ ") (" ++ showDRSDebug d1 ++ ")"

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Discourse variable denoting a simple or complex speech act discourse  
-- referent (a label or pointer). 
---------------------------------------------------------------------------
type DisVar = Int

-- ---------------------------------------------------------------------------
-- -- | Coordinating Relations
-- ---------------------------------------------------------------------------
-- data CoRel = Narration | Contrast | Result | Parallel | Continuation | Alternation | Conditional
--   deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- -- | Subordinating Relations
-- ---------------------------------------------------------------------------
-- data SubRel = Elaboration | EntityElaboration | Comment | Flashback | Background | Goal | Explanation | Attribution
--   deriving (Show, Eq)

-- data RelLabel = CoRel | SubRel
--   deriving (Show, Eq)

-- I wanted to do it as above (separate the type RelLabel into two sub types SubRel and CoRel, but I'm not sure it's possible in Haskell)
data RelLabel = Narration | Contrast | Result | Parallel | Continuation | Alternation
  | Conditional | Elaboration | EntityElaboration | Comment | Flashback | Background
  | Goal | Explanation | Attribution
  deriving (Show, Eq)


---------------------------------------------------------------------------
-- | Convenience type to represent the mapping function
---------------------------------------------------------------------------
--type SDRSMapper = DisVar -> [SDRSForm] -> Maybe SDRSForm

--instance Show SDRSMapper where show s = showSDRSMap s
--showSDRSMap :: SDRSMapper -> String
--showSDRSMap fromList [] = ""
--showSDRSMap f _ m = "SDRSMap" ++ " " ++ show m

---------------------------------------------------------------------------
-- | A SDRS formula
-- TODO how do I check that the disvars of rrel are (within the set A of discourse variables)?
---------------------------------------------------------------------------
data SDRSForm =
  DRSForm DRS
-- ^ A DRS
  | RRel RelLabel DisVar DisVar
-- ^ A rhetorical relation between two speech act discourse referents
  | ComplexForm SDRSCon
  deriving (Show, Eq)

---------------------------------------------------------------------------
-- | Segmented Discourse Representation Structure.
---------------------------------------------------------------------------
--data SDRS =
--	SDRS SDRSMapper DisVar
--	-- ^ A SDRS (a set of speech act discourse referents, a function assigning
--	-- SDRS formulas to referents and the referent last added to the discourse)
--	deriving (Show)

data SDRS =
  SDRS (Map.Map DisVar SDRSForm) DisVar
  -- ^ A SDRS (a set of speech act discourse referents, a function assigning
  -- SDRS formulas to referents and the referent last added to the discourse)
  deriving (Show, Eq)

---------------------------------------------------------------------------
-- | A SDRS condition
-- If I write Neg (and And) instead of something else like SDRSNeg here,
-- I'm getting into trouble with the DRS module
---------------------------------------------------------------------------
data SDRSCon = 
  SDRSNeg SDRSForm                  -- ^ A negated SDRSForm
  | SDRSAnd SDRSForm SDRSForm       -- ^ An conjunction of two SDRSForms
  deriving (Show, Eq)



--sdrsFormulaMapFunction :: DisVar -> [SDRSForm] -> Maybe SDRSForm
--sdrsFormulaMapFunction _ [] = Nothing
--sdrsFormulaMapFunction index dus
--	| index < 0 = Nothing
--	| index >= length dus = Nothing
--	| otherwise = Just (dus!!index)
