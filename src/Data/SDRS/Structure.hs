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
  listDUs
, lookupDU
, listRelations
, listSegments
, relLabels
, listDRSs
, expandRecursiveFormula
) where

import Data.SDRS.DataType
import qualified Data.Map as M

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns the set of discourse units
---------------------------------------------------------------------------
listDUs :: SDRS -> [DisVar]
listDUs (SDRS m _)     = M.keys m

---------------------------------------------------------------------------
-- | Returns a certain discourse unit if present
---------------------------------------------------------------------------
lookupDU :: SDRS -> DisVar -> Maybe SDRSFormula
lookupDU (SDRS m _) i     = M.lookup i m

---------------------------------------------------------------------------
-- | Returns all labels that are Arguments of Relations
---------------------------------------------------------------------------
relLabels :: [SDRSFormula] -> [DisVar]
relLabels []                           = []
relLabels ((Relation _ dv1 dv2):rest)  = dv1:dv2:relLabels rest 
relLabels ((And sf1 sf2):rest)         = relLabels [sf1] ++ relLabels [sf2] ++ relLabels rest
relLabels ((Not sf1):rest)             = relLabels [sf1] ++ relLabels rest
relLabels ((Segment _):rest)              = relLabels rest

---------------------------------------------------------------------------
-- | Lists, given an SDRS, its relations along with each respective label
-- TODO could I make this generic? Passing along the name of a constructor
-- and only list the SDRSFormulas using this constructor?
-- TODO can this be done in an easier way with some sort of filter?
---------------------------------------------------------------------------
listRelations :: SDRS -> [(DisVar, SDRSFormula)]
listRelations (SDRS m _) = formulas (M.assocs m)
  where formulas :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        formulas [] = []
        formulas (t@(_, Relation _ _ _):rest) = t:(formulas rest)
        formulas ((dv, sf@(And _ _)):rest) = (formulas $ zip (repeat dv) (expandRecursiveFormula sf)) ++ formulas rest
        formulas ((dv, sf@(Not _)):rest) = (formulas $ zip (repeat dv) (expandRecursiveFormula sf)) ++ formulas rest
        formulas (_:rest) = formulas rest

---------------------------------------------------------------------------
-- | Similar to listRelations, lists Segments of an SDRS, along with their
-- discourse variables.
---------------------------------------------------------------------------
listSegments :: SDRS -> [(DisVar, SDRSFormula)]
listSegments (SDRS m _) = formulas (M.assocs m)
  where formulas :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        formulas [] = []
        formulas (t@(_, Segment _):rest) = t:(formulas rest)
        formulas ((dv, sf@(And _ _)):rest) = (formulas $ zip (repeat dv) (expandRecursiveFormula sf)) ++ formulas rest
        formulas ((dv, sf@(Not _)):rest) = (formulas $ zip (repeat dv) (expandRecursiveFormula sf)) ++ formulas rest
        formulas (_:rest) = formulas rest

---------------------------------------------------------------------------
-- | Lists all embedded DRSs of an SDRS
---------------------------------------------------------------------------
listDRSs :: SDRS -> [DRS]
listDRSs (SDRS m _) = drss (M.assocs m)
  where drss :: [(DisVar, SDRSFormula)] -> [DRS]
        drss [] = []
        drss ((_, Segment d):rest) = d:(drss rest)
        drss ((dv, sf@(And _ _)):rest) = (drss $ zip (repeat dv) (expandRecursiveFormula sf)) ++ drss rest
        drss ((dv, sf@(Not _)):rest) = (drss $ zip (repeat dv) (expandRecursiveFormula sf)) ++ drss rest
        drss (_:rest) = drss rest

---------------------------------------------------------------------------
-- | given an SDRSFormula, returns a list of all of its embedded SDRSFormulae 
---------------------------------------------------------------------------
expandRecursiveFormula :: SDRSFormula -> [SDRSFormula]
expandRecursiveFormula (Not sf1) = expandRecursiveFormula sf1
expandRecursiveFormula (And sf1 sf2) = expandRecursiveFormula sf1 ++ expandRecursiveFormula sf2
expandRecursiveFormula sf = [sf]
