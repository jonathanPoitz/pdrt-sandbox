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
, relations
, segments
, relLabels
, drss
, lookupKey
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
-- | Lookup the DisVar that labels the relation where the given DisVar is 
-- an argument of
---------------------------------------------------------------------------
lookupKey :: SDRS -> DisVar -> DisVar
lookupKey s dv = findKey $ relations s
  where findKey :: [(DisVar, SDRSFormula)] -> DisVar
        findKey []                 = error $ "Discourse variable " ++ show dv ++ " was not found."
        findKey ((dv0, Relation _ dv1 dv2):rest)
          | dv == dv1 || dv == dv2 = dv0
          | otherwise              = findKey rest
        findKey ((_,_):rest)       = findKey rest -- should not happen b/c of "relations s" call

---------------------------------------------------------------------------
-- | Returns all labels that are Arguments of Relations
---------------------------------------------------------------------------
relLabels :: [SDRSFormula] -> [DisVar]
relLabels []                          = []
relLabels ((Relation _ dv1 dv2):rest) = dv1:dv2:relLabels rest 
relLabels ((And sf1 sf2):rest)        = relLabels [sf1] ++ relLabels [sf2] ++ relLabels rest
relLabels ((Not sf1):rest)            = relLabels [sf1] ++ relLabels rest
relLabels ((Segment _):rest)          = relLabels rest

---------------------------------------------------------------------------
-- | Lists, given an SDRS, its relations along with each respective label
---------------------------------------------------------------------------
relations :: SDRS -> [(DisVar, SDRSFormula)]
relations (SDRS m _) = relations' (M.assocs m)
  where relations' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        relations' []                        = []
        relations' (t@(_, Relation {}):rest) = t:(relations' rest)
        relations' ((dv, And sf1 sf2):rest)  = relations' [(dv, sf1)] ++ relations' [(dv, sf2)] ++ relations' rest
        relations' ((dv, Not sf1):rest)      = relations' [(dv, sf1)] ++ relations' rest
        relations' (_:rest)                  = relations' rest

---------------------------------------------------------------------------
-- | Similar to listRelations, lists Segments of an SDRS, along with their
-- discourse variables.
---------------------------------------------------------------------------
segments :: SDRS -> [(DisVar, SDRSFormula)]
segments (SDRS m _) = segments' (M.assocs m)
  where segments' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        segments' []                       = []
        segments' (t@(_, Segment _):rest)  = t:(segments' rest)
        segments' ((dv, And sf1 sf2):rest) = segments' [(dv, sf1)] ++ segments' [(dv, sf2)] ++ segments' rest
        segments' ((dv, Not sf1):rest)     = segments' [(dv, sf1)] ++ segments' rest
        segments' (_:rest)                 = segments' rest

---------------------------------------------------------------------------
-- | Lists all embedded DRSs of an SDRS
---------------------------------------------------------------------------
drss :: SDRS -> [DRS]
drss (SDRS m _) = drss' (map snd $ M.assocs m)
  where drss' :: [SDRSFormula] -> [DRS]
        drss' []                 = []
        drss' (Segment d:rest)   = d:(drss' rest)
        drss' (And sf1 sf2:rest) = drss' [sf1] ++ drss' [sf2] ++ drss' rest
        drss' (Not sf1:rest)     = drss' [sf1] ++ drss' rest
        drss' (_:rest)           = drss' rest
