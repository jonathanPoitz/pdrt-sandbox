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
, module Data.DRS.DataType
) where

import Data.DRS.DataType

import Data.List (intercalate)

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
-- | Segmented Discourse Representation Structure (DRS)
---------------------------------------------------------------------------

-- [your code goes here]

data SDRS = SDRS DRS
  deriving (Show)
