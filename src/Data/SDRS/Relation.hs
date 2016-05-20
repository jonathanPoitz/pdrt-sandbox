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
) where

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
