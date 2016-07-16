{- |
Module      :  Data.SDRS.Relation
Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
License     :  Apache-2.0

Maintainer  :  jonathanpoitz@gmail.com, me@hbrouwer.eu, njvenhuizen@gmail.com
Stability   :  provisional
Portability :  portable

SDRS relation
-}

module Data.SDRS.Relation
(
  isCrd
, isSub
, subordLefts
, subordRights
, coordLefts
, coordRights
) where

import Data.SDRS.DataType
import Data.SDRS.Structure

---------------------------------------------------------------------------
-- | checks whether the given 'SDRSRelation' @rel@ is of relation type
-- Coordinating 
---------------------------------------------------------------------------
isCrd :: SDRSRelation -> Bool
isCrd rel = relType rel == Crd

---------------------------------------------------------------------------
-- | checks whether the given 'SDRSRelation' @rel@ is of relation type
-- Subordinating 
---------------------------------------------------------------------------
isSub :: SDRSRelation -> Bool
isSub rel = relType rel == Sub

---------------------------------------------------------------------------
-- | Given an 'SDRS' and a 'DisVar' @dv@, returns all 'DisVar's that stand in a
-- subordinating relation with @dv@ as its right node.
---------------------------------------------------------------------------
subordLefts :: DisVar -> SDRS -> [DisVar]
subordLefts dv s = ss
  where rs = map snd $ relations s
        ss = [ dv1 | CDU (Relation rel dv1 dv2) <- rs, isSub rel, dv2 == dv]

---------------------------------------------------------------------------
-- | Given an 'SDRS' and a 'DisVar' @dv@, returns all 'DisVar's that stand in a
-- subordinating relation with @dv@ as its left node.
---------------------------------------------------------------------------
subordRights :: DisVar -> SDRS -> [DisVar]
subordRights dv s = ss
  where rs = map snd $ relations s
        ss = [ dv2 | CDU (Relation rel dv1 dv2) <- rs, isSub rel, dv1 == dv]

---------------------------------------------------------------------------
-- | Given an 'SDRS' and a 'DisVar' @dv@, returns all 'DisVar's that stand in a
-- coordinating relation with @dv@ as its right node.
---------------------------------------------------------------------------
coordLefts :: DisVar -> SDRS -> [DisVar]
coordLefts dv s = ss
  where rs = map snd $ relations s
        ss = [ dv1 | CDU (Relation rel dv1 dv2) <- rs, isCrd rel, dv2 == dv]

---------------------------------------------------------------------------
-- | Given an 'SDRS' and a 'DisVar' @dv@, returns all 'DisVar's that stand in a
-- coordinating relation with @dv@ as its left node.
---------------------------------------------------------------------------
coordRights :: DisVar -> SDRS -> [DisVar]
coordRights dv s = ss
  where rs = map snd $ relations s
        ss = [ dv2 | CDU (Relation rel dv1 dv2) <- rs, isCrd rel, dv1 == dv]
