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
import Data.SDRS.DiscourseStructure

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
          mpos = modifierPos' form

instance {-# OVERLAPPING #-} Show [SDRSFormula] where
  show fs = '\n' : unlines (map showFormula fs)

instance {-# OVERLAPPING #-} Show SDRSFormula where
  show sf = showFormula sf

instance {-# OVERLAPPING #-} Show CDU where
  show cdu = showCDU cdu
-- DEBUG // DEBUG // DEBUG --


---------------------------------------------------------------------------
-- | Typeclass for 'showableSDRS's, that are unresolved.
---------------------------------------------------------------------------
class ShowableSDRS s where
  resolve :: s -> SDRS

-- | Derive appropriate instances of 'ShowableSDRS'.
instance ShowableSDRS SDRS where
  resolve s = s
--instance (ShowableSDRS s) => ShowableSDRS (DRS -> s) where
--  resolve s 

-- | Derive appropriate instances of 'Show' for 'ShowableSDRS's.

-- TODO: add instance declarations

---------------------------------------------------------------------------
-- | 'SDRS' notations.
---------------------------------------------------------------------------
data SDRSNotation s =
  Set s      -- ^ Set notation
  | Linear s -- ^ Linear notation
  | Boxes s  -- ^ embedded Box notation
  | Underspec s  -- ^ underspecified Box notation
  | Debug s  -- ^ Debug notation

-- | Derive an instance of Show for 'SDRSNotation'.
instance (ShowableSDRS s) => Show (SDRSNotation s) where
  show (Boxes s)  = '\n' : showSDRS (Boxes           (resolve s))
  show (Underspec s)  = '\n' : showSDRS (Underspec   (resolve s))
  show (Linear s) = '\n' : showSDRS (Linear          (resolve s))
  show (Set s)    = '\n' : showSDRS (Set             (resolve s))
  show (Debug s)  = '\n' : showSDRS (Debug           (resolve s))

---------------------------------------------------------------------------
-- | Shows an 'SDRS'.
---------------------------------------------------------------------------
showSDRS :: SDRSNotation SDRS -> String
showSDRS n = 
  case n of
    (Boxes s)  -> showSDRSBox s
    (Underspec s)  -> showSDRSUnderspec s
    (Linear s) -> showSDRSLinear s -- showSDRSLinear s ++ "\n"
    (Set s)    -> showSDRSBox s -- showSDRSSet s ++ "\n"
    (Debug s)  -> showSDRSDebug s -- showSDRSDebug s ++ "\n"

---------------------------------------------------------------------------
-- | Prints an 'SDRS'.
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
-- | Show a 'SDRS' in 'Debug' notation.
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | TODO get rid of line breaks after CDUs, make underspec Debug?
---------------------------------------------------------------------------
showSDRSDebug :: SDRS -> String
showSDRSDebug (SDRS m l) = "SDRS" ++ " " ++ "[" ++ intercalate "," (map showTuple $ Map.assocs m) ++ "]" ++ " " ++ show l
  where showTuple :: (DisVar,SDRSFormula) -> String
        showTuple (dv,EDU d)      = "(" ++ show dv ++ "," ++ showDRSDebug d ++ ")"
        showTuple (dv,sf@(CDU _)) = "(" ++ show dv ++ "," ++ showFormula sf ++ ")"

---------------------------------------------------------------------------
-- | Shows an 'SDRS' in 'Boxes' notation.
---------------------------------------------------------------------------

--showSDRSBox :: SDRS -> String
--showSDRSBox (SDRS f ll) = showHorizontalLine l boxTopLeft boxTopRight
--  ++ showContent l vl ++ showHorizontalLine l boxMiddleLeft boxMiddleRight
--  ++ showContent l fl ++ showHorizontalLine l boxBottomLeft boxBottomRight
--  where vl
--          | not(null dvs) = showDisVars dvs "  "
--          | otherwise     = " "
--          where dvs = Map.keys f
--        fl = showFunction (Map.toList f) ll
--        l = 4 + maximum (map length (lines vl) `union` map length (lines fl))

---------------------------------------------------------------------------
-- | Shows an 'SDRS' in 'EmbedBoxes' notation.
-- TODO needs to be decomposed to be more readable
---------------------------------------------------------------------------
showSDRSBox :: SDRS -> String
showSDRSBox s@(SDRS f ll) = showSubBox outscopedDVs (-1) -- not a good dummy value and isn't there another way of writing (-1)
  where outscopedDVs = [root s]
        -- ^ the root node as the first box to draw
        -- v the recursive function that draws a box for a given set of disVars, the second DisVar is the parent label (needed for putting the relation in the box itself)
        showSubBox :: [DisVar] -> DisVar -> String
        showSubBox dvs fromDV = showHorizontalLine l boxTopLeft boxTopRight ++
                           showContent l disVarsString ++
                           showHorizontalLine l boxMiddleLeft boxMiddleRight ++
                           showContent l contentString ++
                           showHorizontalLine l boxBottomLeft boxBottomRight
          where disVarsString = case (null dvs) of
                                 True -> " "
                                 False -> showDisVars dvs "  "
                -- ^ the "universe" of this box
                contentString = showSubFunction (Map.toList (Map.filterWithKey (\k _ -> k `elem` dvs) f)) ++
                                case (Map.lookup fromDV f) of -- takes care of relation that needs to be embedded outside of the box
                                  Just sf -> showFunc' (fromDV,sf) ll
                                  Nothing -> ""
                -- ^ the content of this box which recursively calls showSubBox
                showSubFunction :: [(DisVar,SDRSFormula)] -> String
                showSubFunction f' = foldr ((++) . (showSubFunc)) "" f'
                showSubFunc :: (DisVar,SDRSFormula) -> String
                showSubFunc t@(dv,_)
                  | dv `Map.member` outscopesMap = showModifier (show dv ++ ":") (modifierPos' form) form
                  | otherwise                    = showFunc' t ll
                  where form = showSubBox (outscopesMap Map.! dv) dv
                -- v for calculating the max length of the outer box
                l = 4 + maximum (map length (lines disVarsString) `union` map length (lines contentString))
        outscopesMap = Map.fromList $ iOutscopesMap s
        -- ^ the outscopes map that governs the embedding structure of the boxes

---------------------------------------------------------------------------
-- | Shows an 'SDRS' in 'EmbedGraph' notation.
-- TODO needs to be decomposed to be more readable
---------------------------------------------------------------------------
showSDRSUnderspec :: SDRS -> String
showSDRSUnderspec s@(SDRS f ll) = showSubBox outscopedDVs (-1) -- not a good dummy value and isn't there another way of writing (-1)
  where outscopedDVs = [root s]
        -- ^ the root node as the first box to draw
        -- v the recursive function that draws a box for a given set of disVars 
        showSubBox :: [DisVar] -> DisVar -> String
        showSubBox dvs fromDV = showHorizontalLine l boxTopLeft boxTopRight ++
                           showContent l disVarsString ++
                           showHorizontalLine l boxMiddleLeft boxMiddleRight ++
                           showContent l contentString ++
                           showHorizontalLine l boxBottomLeft boxBottomRight
          where disVarsString = case (null dvs) of
                                 True -> " "
                                 False -> showDisVars dvs "  "
                -- ^ the "universe" of this box
                contentString = showSubUnspecFunction (Map.toList (Map.filterWithKey (\k _ -> k `elem` dvs) f)) ++
                                case (Map.lookup fromDV f) of -- takes care of relation that needs to be embedded outside of the box
                                  Just sf -> showFunc' (fromDV,sf) ll
                                  Nothing -> ""
                -- ^ the content of this box which recursively calls showSubBox
                showSubUnspecFunction :: [(DisVar,SDRSFormula)] -> String
                showSubUnspecFunction f' = foldr ((++) . (showSubUnspecFunc)) "" f'
                showSubUnspecFunc :: (DisVar,SDRSFormula) -> String
                showSubUnspecFunc t@(dv,_)
                  | dv `Map.member` outscopesMap = showModifier (show dv ++ ":") (modifierPos' form) form
                  | otherwise                    = showUnspecFunc t ll
                  where form = showSubBox (outscopesMap Map.! dv) dv
                -- v for calculating the max length of the outer box
                l = 4 + maximum (map length (lines disVarsString) `union` map length (lines contentString))
        outscopesMap = Map.fromList $ iOutscopesMap s
        -- ^ the outscopes map that governs the embedding structure of the boxes

---------------------------------------------------------------------------
-- | Shows an 'SDRS' in linear, underspecified Boxes notation.
-- TODO needs to be decomposed to be more readable
---------------------------------------------------------------------------
showSDRSLinear :: SDRS -> String
showSDRSLinear s@(SDRS f ll) = showSubBox outscopedDVs (-1) -- not a good dummy value and isn't there another way of writing (-1)
  where outscopedDVs = [root s]
        -- ^ the root node as the first box to draw
        -- v the recursive function that draws a box for a given set of disVars 
        showSubBox :: [DisVar] -> DisVar -> String
        showSubBox dvs fromDV = showHorizontalLine l boxTopLeft boxTopRight ++
                           showContent l disVarsString ++
                           showHorizontalLine l boxMiddleLeft boxMiddleRight ++
                           showContent l contentString ++
                           showHorizontalLine l boxBottomLeft boxBottomRight
          where disVarsString = case (null dvs) of
                                 True -> " "
                                 False -> showDisVars dvs "  "
                -- ^ the "universe" of this box
                contentString = showSubUnspecFunction (Map.toList (Map.filterWithKey (\k _ -> k `elem` dvs) f)) ++
                                case (Map.lookup fromDV f) of -- takes care of relation that needs to be embedded outside of the box
                                  Just sf -> showFunc' (fromDV,sf) ll
                                  Nothing -> ""
                -- ^ the content of this box which recursively calls showSubBox
                showSubUnspecFunction :: [(DisVar,SDRSFormula)] -> String
                showSubUnspecFunction f' = foldr ((++) . (showSubUnspecFunc)) "" f'
                showSubUnspecFunc :: (DisVar,SDRSFormula) -> String
                showSubUnspecFunc t@(dv,_)
                  | dv `Map.member` outscopesMap = showModifier (show dv ++ ":") (modifierPos' form) form
                  | otherwise                    = showLinearFunc t ll
                  where form = showSubBox (outscopesMap Map.! dv) dv
                -- v for calculating the max length of the outer box
                l = 4 + maximum (map length (lines disVarsString) `union` map length (lines contentString))
        outscopesMap = Map.fromList $ iOutscopesMap s
        -- ^ the outscopes map that governs the embedding structure of the boxes

---------------------------------------------------------------------------
-- **  Showing the subparts of an SDRS
---------------------------------------------------------------------------

--showFunction :: [(DisVar,SDRSFormula)] -> DisVar -> String
--showFunction f ll = foldr ((++) . ((flip showFunc) ll)) "" f

---------------------------------------------------------------------------
-- **  Showing the subparts of an 'SDRS' in underspecified form, i.e., with
-- the 'DRS's replaced by placeholders.
---------------------------------------------------------------------------
--showUnspecFunction :: [(DisVar,SDRSFormula)] -> DisVar -> String
--showUnspecFunction f ll = foldr ((++) . ((flip showUnspecFunc) ll)) "" f

---------------------------------------------------------------------------
-- | Helper function for showUnspecFunction
---------------------------------------------------------------------------
showUnspecFunc :: (DisVar,SDRSFormula) -> DisVar -> String
showUnspecFunc (dv,sf@(EDU _)) ll
  | ll == dv      = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise     = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showUnspecFormula sf dv
showUnspecFunc (dv,sf) ll = showFunc (dv,sf) ll

---------------------------------------------------------------------------
-- | Helper function
---------------------------------------------------------------------------
showLinearFunc :: (DisVar,SDRSFormula) -> DisVar -> String
showLinearFunc (dv,sf@(EDU _)) ll
  | ll == dv      = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise     = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showLinearFormula sf dv
showLinearFunc (dv,sf) ll = showFunc (dv,sf) ll

---------------------------------------------------------------------------
-- | Helper function for showFunction and showUnspecFunction
---------------------------------------------------------------------------
showFunc :: (DisVar,SDRSFormula) -> DisVar -> String
showFunc (dv,sf@(EDU _)) ll
  | ll == dv  = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showFormula sf
showFunc (dv,sf@(CDU (Relation _ _ _))) ll
  | ll == dv  = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showFormula sf
showFunc (dv,(CDU (And f1 f2))) ll
  | ll == dv  = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showConjunction (CDU f1) (CDU f2)
showFunc (dv,(CDU (Not f1))) ll
  | ll == dv  = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showNegation (CDU f1)

---------------------------------------------------------------------------
-- | Helper function for showFunction' and showUnspecFunction' as part of
-- embedded box notation, i.e., omitting the modifier before a relation
---------------------------------------------------------------------------
showFunc' :: (DisVar,SDRSFormula) -> DisVar -> String
showFunc' (dv,sf@(EDU _)) ll
  | ll == dv  = showModifier ("*" ++ show dv ++ "*" ++ ":") (modifierPos' form) form
  | otherwise = showModifier (show dv ++ ":") (modifierPos' form) form
  where form = showFormula sf
showFunc' (_,sf@(CDU (Relation _ _ _))) _ = form
  where form = showFormula sf
showFunc' (_,(CDU (And f1 f2))) _ = form
  where form = showConjunction (CDU f1) (CDU f2)
showFunc' (_,(CDU (Not f1))) _ = form
  where form = showNegation (CDU f1)

-- DEBUG / DEBUG / DEBUG
showCDU :: CDU -> String
showCDU (Relation r dv1 dv2) = relName r ++ "(" ++ show dv1 ++ "," ++ show dv2 ++")\n"
showCDU (And f1 f2)          = showConjunction (CDU f1) (CDU f2) 
showCDU (Not f1)             = showNegation (CDU f1)

showFormula :: SDRSFormula -> String
showFormula (EDU d)          = showDRS (DRS.Boxes d)
showFormula (CDU (Relation r dv1 dv2)) = relName r ++ "(" ++ show dv1 ++ "," ++ show dv2 ++")\n"
showFormula (CDU (And f1 f2))          = showConjunction (CDU f1) (CDU f2) 
showFormula (CDU (Not f1))             = showNegation (CDU f1)

showLinearFormula :: SDRSFormula -> DisVar -> String
showLinearFormula (EDU d) _ = (showDRS $ DRS.Linear d) ++ "\n"
showLinearFormula sf _      = showFormula sf

showUnspecFormula :: SDRSFormula -> DisVar -> String
showUnspecFormula (EDU _) dv = "K" ++ show dv -- ++ "\n" -- why does this do anything?
showUnspecFormula sf _       = showFormula sf

--modifierPos :: String -> Int
--modifierPos' s
--  | (length (lines s)) > 1 = 2
--  | otherwise              = 0

modifierPos' :: String -> Int
modifierPos' s
  | (length (lines s)) > 2 = (length (lines s)) `div` 2
  | otherwise              = 0

showConjunction :: SDRSFormula -> SDRSFormula -> String
showConjunction f1 f2
  | lf1 > 1  && lf2 == 1 = showConcat form1 (showModifier opAnd (modifierPos' form1) (showPadding form2))
  | lf1 == 1  && lf2 > 1 = showConcat (showPadding form1) (showModifier opAnd (modifierPos' form2) form2)
  | otherwise            = showConcat form1 (showModifier opAnd (modifierPos' form2) form2)
  where form1 = showFormula f1
        form2 = showFormula f2
        lf1 = length (lines form1)
        lf2 = length (lines form2)

showNegation :: SDRSFormula -> String
showNegation f
  | mpos == 0 = showModifier opNot mpos (showConcat (showModifier "(" mpos form) ")\n")
  | otherwise = showModifier opNot mpos (showConcat (showModifier "(" mpos form) (DRS.showPadding ")\n"))
  where form = showFormula f
        mpos = modifierPos' form

---------------------------------------------------------------------------
-- | Shows the discourse variables @dvs$ of an 'SDRS', using 'String' @d@ 
-- as a delimiter between referents.
---------------------------------------------------------------------------
showDisVars :: [DisVar] -> String -> String
showDisVars dvs d = intercalate d (map show dvs)
