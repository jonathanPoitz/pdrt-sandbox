{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Module      :  Data.SDRS.Show
Copyright   :  (c) Harm Brouwer, Jonathan Poitz and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  me@hbrouwer.eu, jonathanpoitz@gmail.com, n.j.venhuizen@rug.nl
Stability   :  provisional
Portability :  portable

SDRS pretty printing
-}

module Data.SDRS.Show
(
-- * Show SDRS (pretty printing)
  SDRSNotation (..)
, showSDRS
, printSDRS
-- ** SDRS Operator symbols
, opAnd
, opNot
) where

import Data.DRS.Show hiding (DRSNotation (..))
import qualified Data.DRS.Show as DRS
import Data.SDRS.DataType

import Data.List (intercalate, union)
import qualified Data.Map as Map

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- ** Show DRS
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Derive an instance of the 'Show' typeclass for 'SDRS'.
---------------------------------------------------------------------------
instance Show SDRS where
  show s = '\n' : showSDRS (Boxes s)

-- DEBUG // DEBUG // DEBUG --
-- XXX: This is for DEBUGGING only (for now)
instance {-# OVERLAPPING #-} (Show a) => Show [(a,SDRSFormula)] where
  show [] = ""
  show ((l,f):ts)
    | mpos == 0 = '\n' : showConcat (showModifier ("( " ++ show l ++ " ,") mpos form) ")\n" ++ show ts
    | otherwise = '\n' : showConcat (showModifier ("( " ++ show l ++ " ,") mpos form) (DRS.showPadding ")\n") ++ show ts
    where form = showFormula f
          mpos = modifierPos form
instance {-# OVERLAPPING #-} Show [SDRSFormula] where
  show fs = '\n' : unlines (map showFormula fs)
-- DEBUG // DEBUG // DEBUG --
--
---------------------------------------------------------------------------
-- | Typeclass for 'showableSDRS's, that are unresolved.
---------------------------------------------------------------------------
class ShowableSDRS s where
  resolve :: s -> SDRS

-- | Derive appropriate instances of 'ShowableSDRS'.
-- instance ShowableSDRS SDRS where
--   resolve s = s
-- instance (ShowableSDRS s) => ShowableSDRS (DRS -> s) where
--   resolve s

-- | Derive appropriate instances of 'Show' for 'ShowableSDRS's.

-- TODO: add instance declarations

---------------------------------------------------------------------------
-- | 'SDRS' notations.
---------------------------------------------------------------------------
data SDRSNotation s =
  Set s      -- ^ Set notation
  | Linear s -- ^ Linear notation
  | Boxes s  -- ^ Box notation
  | Debug s  -- ^ Debug notation

-- | Derive an instance of Show for 'SDRSNotation'.
instance (ShowableSDRS s) => Show (SDRSNotation s) where
  show (Boxes s)  = '\n' : showSDRS (Boxes  (resolve s))
  show (Linear s) = '\n' : showSDRS (Linear (resolve s))
  show (Set s)    = '\n' : showSDRS (Set    (resolve s))
  show (Debug s)  = '\n' : showSDRS (Debug  (resolve s))

---------------------------------------------------------------------------
-- | Shows a 'SDRS'.
---------------------------------------------------------------------------
showSDRS :: SDRSNotation SDRS -> String
showSDRS n = 
  case n of
    (Boxes s)  -> showSDRSBox s
    (Linear s) -> showSDRSBox s -- showSDRSLinear s ++ "\n"
    (Set s)    -> showSDRSBox s -- showSDRSSet s ++ "\n"
    (Debug s)  -> showSDRSBox s -- showSDRSDebug s ++ "\n"

---------------------------------------------------------------------------
-- | Prints a 'SDRS'.
---------------------------------------------------------------------------
printSDRS :: SDRS -> IO ()
printSDRS s = putStrLn $ '\n' : showSDRS (Boxes s)

---------------------------------------------------------------------------
-- ** Operators
---------------------------------------------------------------------------

-- | Conjunction symbol
opAnd :: String
opAnd = "\x2227"

-- | Negation symbol
opNot :: String
opNot = "\x0021"

---------------------------------------------------------------------------
-- * Private
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- **  Notations for showing DRSs
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Shows an 'SDRS' in 'Boxes' notation.
---------------------------------------------------------------------------

showSDRSBox :: SDRS -> String
showSDRSBox (SDRS f ll) = showHorizontalLine l boxTopLeft boxTopRight
  ++ showContent l vl ++ showHorizontalLine l boxMiddleLeft boxMiddleRight
  ++ showContent l fl ++ showHorizontalLine l boxBottomLeft boxBottomRight
  where vl
          | not(null dvs) = (showDisVars dvs "  ") ++ "   last:" ++ show ll
          | otherwise     = " "
          where dvs = Map.keys f
        fl = showFunction (Map.toList f)
        l = 4 + maximum (map length (lines vl) `union` map length (lines fl))

---------------------------------------------------------------------------
-- **  Showing the subparts of an SDRS
---------------------------------------------------------------------------

showFunction :: [(DisVar,SDRSFormula)] -> String
showFunction f = foldr ((++) . showFunc) "" f
  where showFunc :: (DisVar,SDRSFormula) -> String
        showFunc (dv,sf@(Segment _))      = showModifier (show dv ++ ":") (modifierPos form) form
          where form = showFormula sf
        showFunc (dv,sf@(Relation _ _ _)) = showModifier (show dv ++ ":") (modifierPos form) form
          where form = showFormula sf
        showFunc (dv,(And f1 f2))         = showModifier (show dv ++ ":") (modifierPos form) form
          where form = showConjunction f1 f2
        showFunc (dv,(Not f1))            = showModifier (show dv ++ ":") (modifierPos form) form
          where form = showNegation f1

showFormula :: SDRSFormula -> String
showFormula (Segment d)          = showDRS (DRS.Boxes d)
showFormula (Relation r dv1 dv2) = label r ++ "(" ++ show dv1 ++ "," ++ show dv2 ++")\n"
showFormula (And f1 f2)          = showConjunction f1 f2 
showFormula (Not f1)             = showNegation f1

modifierPos :: String -> Int
modifierPos s
  | (length (lines s)) > 1 = 2
  | otherwise              = 0

showConjunction :: SDRSFormula -> SDRSFormula -> String
showConjunction f1 f2
  | lf1 > 1  && lf2 == 1 = showConcat form1 (showModifier opAnd (modifierPos form1) (showPadding form2))
  | lf1 == 1  && lf2 > 1 = showConcat (showPadding form1) (showModifier opAnd (modifierPos form2) form2)
  | otherwise            = showConcat form1 (showModifier opAnd (modifierPos form2) form2)
  where form1 = showFormula f1
        form2 = showFormula f2
        lf1 = length (lines form1)
        lf2 = length (lines form2)

showNegation :: SDRSFormula -> String
showNegation f
  | mpos == 0 = showModifier opNot mpos (showConcat (showModifier "(" mpos form) ")\n")
  | otherwise = showModifier opNot mpos (showConcat (showModifier "(" mpos form) (DRS.showPadding ")\n"))
  where form = showFormula f
        mpos = modifierPos form

---------------------------------------------------------------------------
-- | Shows the discourse variables @dvs$ of an 'SDRS', using 'String' @d@ 
-- as a delimiter between referents.
---------------------------------------------------------------------------
showDisVars :: [DisVar] -> String -> String
showDisVars dvs d = intercalate d (map show dvs)
