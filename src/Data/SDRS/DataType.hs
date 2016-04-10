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
, isCrdRelation
, isSubRelation
, isRelation
, isTopicRelation
, isStrucRelation
, labelEq
, DGraph
, Label
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
-- | Discourse variable denoting a simple or complex speech act discourse  
-- referent (a label or pointer). 
---------------------------------------------------------------------------
type DisVar = Int

---------------------------------------------------------------------------
-- | Name of a rhetorical relation 
---------------------------------------------------------------------------
type RelName = String

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
-- | DGraph
---------------------------------------------------------------------------

type DGraph = M.Map DisVar [(DisVar, Label)]

---------------------------------------------------------------------------
-- | relation label
---------------------------------------------------------------------------

type Label = String

---------------------------------------------------------------------------
-- | relation types
---------------------------------------------------------------------------

--relations :: [Label]
--relations = ["elaboration",
--             "entity_elaboration",
--             "comment",
--             "flashback",
--             "background",
--             "goal",
--             "explanation",
--             "attribution",
--             "narration",
--             "contrast",
--             "result",
--             "parallel",
--             "continuation",
--             "alternation",
--             "conditional"]

---------------------------------------------------------------------------
-- | checks if a relation label is a subordinating relation
---------------------------------------------------------------------------
isSubRelation :: String -> Bool
isSubRelation label = (filter (/=' ') (map toLower label)) `elem` relations
 where relations = ["elaboration",
                   "entity_elaboration",
                   "comment",
                   "flashback",
                   "background",
                   "goal",
                   "explanation",
                   "attribution"]

---------------------------------------------------------------------------
-- | checks if a relation label is a subordinating relation
---------------------------------------------------------------------------
isCrdRelation :: String -> Bool
isCrdRelation label = (filter (/=' ') (map toLower label)) `elem` relations
  where relations = ["narration",
                     "contrast",
                     "result",
                     "parallel",
                     "continuation",
                     "alternation",
                     "conditional"]

---------------------------------------------------------------------------
-- | checks if a relation label is a relation
---------------------------------------------------------------------------
isRelation :: String -> Bool
isRelation label = (filter (/=' ') (map toLower label)) `elem` relations
  where relations = ["narration",
                     "contrast",
                     "result",
                     "parallel",
                     "continuation",
                     "alternation",
                     "conditional",
                     "elaboration",
                     "entity_elaboration",
                     "comment",
                     "flashback",
                     "background",
                     "goal",
                     "explanation",
                     "attribution"]

---------------------------------------------------------------------------
-- | checks if a relation label is a structural relation, otherwise it is
-- a non-structural relation
---------------------------------------------------------------------------
isStrucRelation :: String -> Bool
isStrucRelation label = (filter (/=' ') (map toLower label)) `elem` relations
  where relations = ["contrast",
                     "parallel"]

---------------------------------------------------------------------------
-- | checks if a relation label imposes a topic constraint, otherwise it does
-- not
---------------------------------------------------------------------------
isTopicRelation :: String -> Bool
isTopicRelation label = (filter (/=' ') (map toLower label)) `elem` relations
  where relations = ["narration",
                     "continuation"]

---------------------------------------------------------------------------
-- | a convenience method for equality of labels, including preprocessing
-- (trimming and lowercasing)
---------------------------------------------------------------------------
labelEq :: String -> String -> Bool
labelEq label1 label2 = (filter (/=' ') (map toLower label1)) == (filter (/=' ') (map toLower label2))



