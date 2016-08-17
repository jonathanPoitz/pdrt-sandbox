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
, updateLast
, removeRel
, removeRels
, updateRightArgs
, calcLeftArgRels
, calcRightArgRels
, addEDU
, addCDUs
, addCDU
, negateRelation
, renameDisVars
--, normalize
) where

import qualified Data.Map as M

import Data.SDRS.DataType

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
-- | Returns True if the given 'SDRSFormula' is a 'CDU', False if it is an 'EDU'
---------------------------------------------------------------------------
--isCDU :: SDRSFormula -> Bool
--isCDU (CDU _) = True
--isCDU (EDU _) = False

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, returns all labels that directly label the 'SDRSFormula'
-- @sf@.
---------------------------------------------------------------------------
getDisVars :: SDRSFormula -> SDRS -> [DisVar]
getDisVars sf (SDRS m _) = M.keys $ M.filter (\sf' -> sf == sf') m 

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
        relArgs' []                                = []
        relArgs' ((CDU (Relation _ dv1 dv2)):rest) = dv1:dv2:relArgs' rest 
        relArgs' ((CDU (And sf1 sf2)):rest)        = relArgs' [CDU sf1] ++ relArgs' [CDU sf2] ++ relArgs' rest
        relArgs' ((CDU (Not sf1)):rest)            = relArgs' [CDU sf1] ++ relArgs' rest
        relArgs' ((EDU _):rest)                    = relArgs' rest

---------------------------------------------------------------------------
-- | Lists, given an SDRS, its relations along with each respective label
---------------------------------------------------------------------------
relations :: SDRS -> [(DisVar, SDRSFormula)]
relations (SDRS m _) = relations' (M.assocs m)
  where relations' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        relations' []                              = []
        relations' (t@(_, CDU (Relation {})):rest) = t:(relations' rest)
        relations' ((dv, CDU (And sf1 sf2)):rest)  = relations' [(dv, CDU sf1)] ++ relations' [(dv, CDU sf2)] ++ relations' rest
        relations' ((dv, CDU (Not sf1)):rest)      = relations' [(dv, CDU sf1)] ++ relations' rest
        relations' (_:rest)                        = relations' rest

---------------------------------------------------------------------------
-- | Similar to listRelations, lists Segments of an SDRS, along with their
-- discourse variables.
---------------------------------------------------------------------------
segments :: SDRS -> [(DisVar, SDRSFormula)]
segments (SDRS m _) = segments' (M.assocs m)
  where segments' :: [(DisVar, SDRSFormula)] -> [(DisVar, SDRSFormula)]
        segments' []                       = []
        segments' (t@(_, EDU _):rest)      = t:(segments' rest)
        segments' (_:rest)                 = segments' rest

---------------------------------------------------------------------------
-- | Lists all embedded DRSs of an SDRS
---------------------------------------------------------------------------
drss :: SDRS -> [DRS]
drss (SDRS m _) = drss' (map snd $ M.assocs m)
  where drss' :: [SDRSFormula] -> [DRS]
        drss' []                 = []
        drss' (EDU d:rest)       = d:(drss' rest)
        drss' (_:rest)           = drss' rest

---------------------------------------------------------------------------
-- | updates the last pointer of an 'SDRS' @s@ to the new 'DisVar' @l@.
---------------------------------------------------------------------------
updateLast :: SDRS -> DisVar -> SDRS
updateLast (SDRS m _) l' = SDRS m l'

---------------------------------------------------------------------------
-- | removes all of the given 'CDU's from the map of 'SDRSFormula'e
-- in an 'SDRS'.
---------------------------------------------------------------------------
removeRels :: SDRS -> [CDU] -> SDRS
removeRels s []        = s
removeRels s (cdu:rest) = removeRels (removeRel s cdu) rest

---------------------------------------------------------------------------
-- | removes a given 'CDU' @r@ from a given SDRS @s@ iff @r@ is a
-- Relation. If @r@ is part of a conjunction, remove it from that conjunction.
-- If it is directly labeled in @s@, remove the label along with @r@.
---------------------------------------------------------------------------
removeRel :: SDRS -> CDU -> SDRS
removeRel s@(SDRS m l) r@(Relation {}) = case elem (CDU r) (M.elems m) of
                                          True  -> SDRS mapWithDeletedDUs l -- <- this l right or should we infer it?
                                          False -> SDRS (M.map (removeRelFromSF r) m) l
  where delDVs = getDisVars (CDU r) s
        mapWithDeletedDUs = foldl (flip M.delete) m delDVs 
removeRel s _                        = s

---------------------------------------------------------------------------
-- | Given a conjunction of 'SDRSFormula'e @sf@, removes the subrelation @r@
-- from the conjunction.
---------------------------------------------------------------------------
removeRelFromSF :: CDU -> SDRSFormula -> SDRSFormula
removeRelFromSF r@(Relation {}) (CDU a@(And {})) = CDU $ recurse a
  where recurse :: CDU -> CDU
        recurse (And sf1@(And {}) sf2@(And {})) = And (recurse sf1) (recurse sf2)
        recurse (And sf1@(And {}) sf2@(Relation {}))
          | r == sf2                            = recurse sf1
          | otherwise                           = And (recurse sf1) sf2
        recurse (And sf1@(Relation {}) sf2@(And {}))
          | r == sf1                            = recurse sf2
          | otherwise                           = And sf1 (recurse sf2)
        recurse a'@(And sf1@(Relation {}) sf2@(Relation {}))
          | r == sf1                            = sf2
          | r == sf2                            = sf1
          | otherwise                           = a'
        recurse sf'                             = sf'
removeRelFromSF _ sf = sf

---------------------------------------------------------------------------
-- | Calculates all relations within an 'SDRS' @s@ that have the 'DisVar' @old@
-- as their left argument.
-- TODO use relation traversal functions here?
---------------------------------------------------------------------------
calcLeftArgRels :: SDRS -> DisVar -> [CDU]
calcLeftArgRels (SDRS m _) old = reverse $ M.foldl putSwapRel [] m -- needs to be reversed in order to get right ordering in conjunction later
  where putSwapRel :: [CDU] -> SDRSFormula -> [CDU]
        putSwapRel acc (EDU {})  = acc
        putSwapRel acc (CDU r@(Relation _ dv1 _))
          | dv1 == old               = r:acc
          | otherwise                = acc
        putSwapRel acc (CDU (And sf1 sf2)) = putSwapRel (putSwapRel acc (CDU sf1)) (CDU sf2)
        putSwapRel acc (CDU (Not sf1))     = putSwapRel acc (CDU sf1)

---------------------------------------------------------------------------
-- | Calculates all relations within an 'SDRS' @s@ that have the 'DisVar' @old@
-- as their right argument. (needed for extended RF?)
---------------------------------------------------------------------------
calcRightArgRels :: SDRS -> DisVar -> [CDU]
calcRightArgRels (SDRS m _) old = reverse $ M.foldl putSwapRel [] m -- needs to be reversed in order to get right ordering in conjunction later
  where putSwapRel :: [CDU] -> SDRSFormula -> [CDU]
        putSwapRel acc (EDU {})  = acc
        putSwapRel acc (CDU r@(Relation _ _ dv2))
          | dv2 == old               = r:acc
          | otherwise                = acc
        putSwapRel acc (CDU (And sf1 sf2)) = putSwapRel (putSwapRel acc (CDU sf1)) (CDU sf2)
        putSwapRel acc (CDU (Not sf1))     = putSwapRel acc (CDU sf1)

---------------------------------------------------------------------------
-- | Conjuncts a given list of 'CDU's to a given 'DisVar' @new@ in an
-- 'SDRS' @s@. If @new@ does not yet exist, it is created first.
---------------------------------------------------------------------------
addCDUs :: SDRS -> DisVar -> [CDU] -> SDRS
addCDUs s _ []           = s
addCDUs s new (cdu:rest) = addCDUs (addCDU s new cdu) new rest

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, adds a CDU @cdu@ as a new conjunct to an
-- existing 'CDU' which is embedded in an 'SDRSFormula' that is labeled by
-- the 'DisVar' @new@. If @new@ is not yet part of the 'SDRS', create it
-- and have it label a new 'SDRSFormula' containing @cdu@.
---------------------------------------------------------------------------
addCDU :: SDRS -> DisVar -> CDU -> SDRS
addCDU (SDRS m l) new cdu 
  | new `M.member` m = SDRS (M.insert new newSF m) l
  | otherwise        = SDRS (M.insert new (CDU cdu) m) l
  where newSF = CDU $ And (extractCDU $ m M.! new) cdu

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@, a 'DisVar' @new@ and an 'SDRSFormula' @edu@, adds
-- @edu@ to the 'SDRS' labeled by @new@. If @new@ already is a label in the map,
-- replace its value by @edu@.
---------------------------------------------------------------------------
addEDU :: SDRS -> DisVar -> SDRSFormula -> SDRS
addEDU (SDRS m l) new edu = SDRS (M.insert new edu m) l

---------------------------------------------------------------------------
-- | Within an 'SDRS' @s@ replaces all references of a given 'DisVar'
-- @old@ with @new@ iff @old@ occurs as a right argument of a relation.
---------------------------------------------------------------------------
updateRightArgs :: SDRS -> DisVar -> DisVar -> SDRS
updateRightArgs (SDRS m l) old new = SDRS (M.map updateR m) l
  where updateR :: SDRSFormula -> SDRSFormula
        updateR edu@(EDU {}) = edu
        updateR (CDU cdu)    = CDU $ updateCDU cdu
        updateCDU :: CDU -> CDU
        updateCDU r@(Relation rel dv1 dv2)
          | dv2 == old = Relation rel dv1 new
          | otherwise = r
        updateCDU (And sf1 sf2) = And (updateCDU sf1) (updateCDU sf2)
        updateCDU (Not sf1)     = Not (updateCDU sf1)

---------------------------------------------------------------------------
-- | Given an 'SDRS' @s@ and the unique description of one of its embedded 
-- relations, i.e., an 'SDRSRelation' @r@ and its start and end 'DisVar'
-- @dv1@ and @dv2@, negates the 'CDU' that embeds this relation.
---------------------------------------------------------------------------
negateRelation :: SDRS -> SDRSRelation -> DisVar -> DisVar -> SDRS
negateRelation (SDRS m l) r dv1 dv2 = SDRS m' l
  where cdu = Relation r dv1 dv2
        m' = M.map (neg cdu) m
        neg :: CDU -> SDRSFormula -> SDRSFormula
        neg _ sf@(EDU _) = sf
        neg cdu' (CDU c@_) = CDU $ neg' cdu' c
        neg' :: CDU -> CDU -> CDU
        neg' cdu' c@(Relation {})
          | cdu' == c = Not c
          | otherwise = c
        neg' cdu' (Not c)
                      = Not $ neg' cdu' c
        neg' cdu' (And c1 c2)
                      = And (neg' cdu' c1) (neg' cdu' c2)

---------------------------------------------------------------------------
-- | Renames all embedded 'DisVar's of 'SDRS' @s@ on the basis of the
-- conversion list @cm@.
---------------------------------------------------------------------------
renameDisVars :: SDRS -> [(DisVar,DisVar)] -> SDRS
renameDisVars (SDRS m l) cm = SDRS (M.fromList (convert' (M.assocs m) cmMap)) (M.findWithDefault l l cmMap)
  where cmMap = M.fromList cm
        convert' :: [(DisVar, SDRSFormula)] -> M.Map DisVar DisVar -> [(DisVar, SDRSFormula)]
        convert' [] _         = []
        convert' (t:rest) cm' = (convertTuple t cm') : (convert' rest cm')
        convertTuple :: (DisVar, SDRSFormula) -> M.Map DisVar DisVar -> (DisVar, SDRSFormula)
        convertTuple (dv, sf@(EDU _)) cm' = (M.findWithDefault dv dv cm', sf)
        convertTuple (dv, (CDU cdu)) cm'  = (M.findWithDefault dv dv cm', CDU $ convertCDU cdu cm')
        convertCDU :: CDU -> M.Map DisVar DisVar -> CDU
        convertCDU (Relation rel dv1 dv2) cm' = Relation rel (M.findWithDefault dv1 dv1 cm') (M.findWithDefault dv2 dv2 cm')
        convertCDU (And cdu1 cdu2) cm'        = And (convertCDU cdu1 cm') (convertCDU cdu2 cm')
        convertCDU (Not cdu1) cm'             = Not (convertCDU cdu1 cm')

---------------------------------------------------------------------------
-- | Normalizes the nodes in the 'SDRS' @s@, i.e., maps the @s@'s 'DisVar's
-- to a sequence from 0 to n, where n is equal to the number of unique 'DisVar's
-- in the 'SDRS'.
-- FIX, make independent from discourseGraph
---------------------------------------------------------------------------
--normalize :: SDRS -> SDRS
--normalize s = renameDisVars s normMap
--  where build :: [DisVar] -> DisVar -> M.Map DisVar DisVar -> M.Map DisVar DisVar
--        build [] _ cm'                     = cm'
--        build (cur:rest) index cm' 
--          | ((M.lookup cur g) == Nothing) = M.insert cur index (build rest (index + 1) cm')
--          | otherwise                     = M.insert cur index (build (rest `union` (map fst $ g M.! cur)) (index + 1) cm')
--        normMap = build [root s] 0 M.empty





