  {- |
  Module      :  Data.SDRS.DataType
  Copyright   :  (c) Jonathan Poitz, Harm Brouwer and Noortje Venhuizen
  License     :  Apache-2.0

  Maintainer  :  jpoitz@coli.uni-saarland.de, me@hbrouwer.eu, n.j.venhuizen@rug.nl
  Stability   :  provisional
  Portability :  portable

  SDRS data type
  -}

  module Data.SDRS.DataType
  (
    SDRS (..)
    , DisVar  
    , RelLabel
    , SDRSForm (..)
    , SDRSCon (..)
    ) where

  --import Data.DRS.DataType

  ---------------------------------------------------------------------------
  -- * Exported
  ---------------------------------------------------------------------------

  ---------------------------------------------------------------------------
  -- | Segmented Discourse Representation Structure.
  ---------------------------------------------------------------------------
  data SDRS =
    SDRS [DisVar] [SDRSForm] DisVar
  -- ^ A SDRS (a set of speech act discourse referents, a function assigning
  -- SDRS formulas to referents and the referent last added to the discourse)
    deriving (Eq,Read)
  
  ---------------------------------------------------------------------------
  -- | Discourse variable denoting a simple or complex speech act discourse  
  -- referent (a label or pointer). 
  ---------------------------------------------------------------------------
  type DisVar = Int

  ---------------------------------------------------------------------------
  -- | Label of rhetorical relation of two speech act discourse referents
  ---------------------------------------------------------------------------
  type RelLabel = String

  ---------------------------------------------------------------------------
  -- | A SDRS formula
  ---------------------------------------------------------------------------
  data SDRSForm =
    DRS
  -- ^ A DRS
    | RRel RelLabel DisVar DisVar
  -- ^ A rhetorical relation between two speech act discourse referents 
    deriving (Eq,Read,Show)


  ---------------------------------------------------------------------------
  -- | A SDRS condition
  ---------------------------------------------------------------------------
  data SDRSCon = 
    Neg SDRSForm           -- ^ A negated SDRSForm
    | Imp SDRSForm SDRSForm       -- ^ An implication between two SDRSForms
    | Or SDRSForm SDRSForm        -- ^ A disjunction between two SDRSForms
  -- | Prop DRSRef SDRSForm   -- ^ A proposition SDRSForm
    | Diamond SDRSForm       -- ^ A possible SDRSForm
    | Box SDRSForm           -- ^ A necessary SDRSForm
    deriving (Eq,Read,Show)
    
