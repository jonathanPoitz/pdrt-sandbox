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
  sdrsDUs
, sdrsGetDu
) where

import Data.SDRS.DataType
import Data.Map as Map

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns the set of discourse units
---------------------------------------------------------------------------
sdrsDUs :: SDRS -> [DisVar]
sdrsDUs (SDRS _ m _)     = Map.keys m

---------------------------------------------------------------------------
-- | Returns a certain discourse unit if present
---------------------------------------------------------------------------
sdrsGetDu :: SDRS -> DisVar -> Maybe SDRSFormula
sdrsGetDu (SDRS _ m _) i     = Map.lookup i m
