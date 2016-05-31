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
  dus
, lookupDU
, relations
, segments
, relArgs
, drss
, lookupKey
, sdrsMap
, sdrsLast
) where

import Data.SDRS.DataType
import qualified Data.Map as M

---------------------------------------------------------------------------
-- * Exported
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- | Returns the map of the 'SDRS'.
---------------------------------------------------------------------------
sdrsMap :: SDRS -> M.Map DisVar SDRSFormula
sdrsMap (SDRS m _) = m

---------------------------------------------------------------------------
-- | Returns the last pointer of the 'SDRS'.
---------------------------------------------------------------------------
sdrsLast :: SDRS -> DisVar
sdrsLast (SDRS _ l) = l

---------------------------------------------------------------------------
-- | Returns the set of discourse units
---------------------------------------------------------------------------
dus :: SDRS -> [DisVar]
dus (SDRS m _) = M.keys m

---------------------------------------------------------------------------
-- | Returns a certain discourse unit if present
---------------------------------------------------------------------------
lookupDU :: SDRS -> DisVar -> Maybe SDRSFormula
lookupDU (SDRS m _) i = M.lookup i m

---------------------------------------------------------------------------
-- | Get the DisVar that labels the relation where the given DisVar is 
-- an argument of.
-- TODO rename!
---------------------------------------------------------------------------
lookupKey :: SDRS -> DisVar -> DisVar
lookupKey s dv = findKey $ relations s
  where findKey :: [(DisVar, SDRSFormula)] -> DisVar
        findKey []                 = error $ "Discourse variable " ++ show dv ++ " was not found."
        findKey ((dv0, CDU (Relation _ dv1 dv2)):rest)
          | dv == dv1 || dv == dv2 = dv0
          | otherwise              = findKey rest
        findKey ((_,_):rest)       = findKey rest -- should not happen b/c of "relations s" call

---------------------------------------------------------------------------
-- | Returns all 'DisVar's that are arguments of relations
---------------------------------------------------------------------------
relArgs :: SDRS -> [DisVar]
relArgs (SDRS m _) = relArgs' $ M.elems m
  where relArgs' :: [SDRSFormula] -> [DisVar]
        relArgs' []                          = []
        relArgs' ((CDU (Relation _ dv1 dv2)):rest) = dv1:dv2:relArgs' rest 
        relArgs' ((CDU (And sf1 sf2)):rest)        = relArgs' [CDU sf1] ++ relArgs' [CDU sf2] ++ relArgs' rest
        relArgs' ((CDU (Not sf1)):rest)            = relArgs' [CDU sf1] ++ relArgs' rest
        relArgs' ((EDU _):rest)          = relArgs' rest

---------------------------------------------------------------------------
-- | Lists, given an SDRS, its relations along with each respective label
---------------------------------------------------------------------------
relations :: SDRS -> [(DisVar, SDRSFormula)]
relations (SDRS m _) = relations' (M.assocs m)
  where relations' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        relations' []                        = []
        relations' (t@(_, CDU (Relation {})):rest) = t:(relations' rest)
        relations' ((dv, CDU (And sf1 sf2)):rest)  = relations' [(dv, CDU sf1)] ++ relations' [(dv, CDU sf2)] ++ relations' rest
        relations' ((dv, CDU (Not sf1)):rest)      = relations' [(dv, CDU sf1)] ++ relations' rest
        relations' (_:rest)                  = relations' rest

---------------------------------------------------------------------------
-- | Similar to listRelations, lists Segments of an SDRS, along with their
-- discourse variables.
---------------------------------------------------------------------------
segments :: SDRS -> [(DisVar, SDRSFormula)]
segments (SDRS m _) = segments' (M.assocs m)
  where segments' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        segments' []                       = []
        segments' (t@(_, EDU _):rest)  = t:(segments' rest)
        segments' (_:rest)                 = segments' rest

---------------------------------------------------------------------------
-- | Lists all embedded DRSs of an SDRS
---------------------------------------------------------------------------
drss :: SDRS -> [DRS]
drss (SDRS m _) = drss' (map snd $ M.assocs m)
  where drss' :: [SDRSFormula] -> [DRS]
        drss' []                 = []
        drss' (EDU d:rest)   = d:(drss' rest)
        drss' (_:rest)           = drss' rest
