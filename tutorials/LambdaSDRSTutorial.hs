-- unresolved structures

import qualified Data.Map as M
import Data.SDRS
import Data.DRS

-- without DRSRef
sdrs_drsref x = SDRS (M.fromList [(0, Relation (relationFromLabel "Result") 1 2),
                                  (1, EDU $ DRS [] [Rel (DRSRel "healthy") [x]]),
                                  (2, EDU $ DRS [] [Rel (DRSRel "happy") [x]])]) 2

-- if one wanted to plug in _a man(x)_ into the hole (λP.Ǝx(man(x) ∧ P(x)), how would we do that?

sdrs_drsref_2 x y = SDRS (M.fromList [(0, Relation (relationFromLabel "Result") 1 2),
                                  (1, EDU $ DRS [] [Rel (DRSRel "healthy") [x]]),
                                  (2, EDU $ DRS [] [Rel (DRSRel "happy") [y]])]) 2

sdrs_drs d = SDRS (M.fromList [(0, Relation (relationFromLabel "Result") 1 2),
                               (1, EDU d),
                               (2, EDU $ DRS [DRSRef "x"] [Rel (DRSRel "happy") [DRSRef "x"]])]) 2

sdrs_rel r = SDRS (M.fromList [(0, Relation r 1 2),
                               (1, EDU $ DRS [DRSRef "x"] [Rel (DRSRel "healthy") [DRSRef "x"]])
                               (2, EDU $ DRS [DRSRef "x"] [Rel (DRSRel "happy") [DRSRef "x"]])])

-- SDRS lacking SDRSFormula? Use case?
-- SDRS lacking DisVar (LAST)? Use case?

