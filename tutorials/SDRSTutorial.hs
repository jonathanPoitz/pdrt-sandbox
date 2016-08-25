-- This is a demo tutorial used for creating and manipulating SDRSs with
-- SDRT-SANDBOX as part of the thesis "An Implementation of Segmented
-- Discourse Representation Theory" by Jonathan Poitz.

import qualified Data.Map as M
import Data.SDRS
import Data.DRS.Show( DRSNotation( Linear ) )

--------------------------------------------------------------------------
-- * Building up an SDRS using addDRS
-- "John has dinner. He eats fish. Then somebody rings the bell."
--------------------------------------------------------------------------
d1 = DRS [DRSRef "x"] [Rel (DRSRel "John") [DRSRef "x"], Rel (DRSRel "have_dinner") [DRSRef "x"]]
s1 = drsToSDRS d1
d2 = DRS [] [Rel (DRSRel "eat_fish") [DRSRef "x"]]
s2 = addDRS s1 d2 [(0, relationFromRelName "Elaboration")]
-- rf s2 = [1,0,2] 
-- ^ In s2, all discourse units are on the right frontier, because attachment was made with a subordinating relation
d3 = DRS [DRSRef "x"] [Rel (DRSRel "person") [DRSRef "x"], Rel (DRSRel "ring_bell") [DRSRef "x"]]
s3 = addDRS s2 d3 [(0, relationFromRelName "Narration")]
-- ^ Therefore, d3 can successfully attach to node 0
-- rf s3 = [3,2]
-- ^ This shows that in s3, 0 and 1 are not available for attachment anymore
-- map Linear $ accessibleDRSs s3 3 = [[x: John(x),have_dinner(x)]]
-- ^ This shows that only d1, but not d2 is accessible from 3 since d2 is embedded underneath the left side of a coordinating relation

--------------------------------------------------------------------------
-- * Checking the conditions of felicity in the SDRS
--------------------------------------------------------------------------
-- sdrsBoundRef (DRSRef "x") 1 s3 = True
-- ^ DRSRef "x" is accessible and bound in DRS labeled by 1 in SDRS s3
-- sdrsFreeRefs s3 = []
-- ^ There are no free referents
-- sdrsProperDRSs s3 = True
-- sdrsPureDRSs s3 = True
-- ^ The embedded DRSs are proper and pure

--------------------------------------------------------------------------
-- * Building up an SDRS using addDRS and addSDRS
-- "John has dinner. He eats fish. The next day he either feels good or is sick"
--------------------------------------------------------------------------
sm1 = s2
-- ^ We reuse the SDRS which we built with addDRS above
d2' = DRS [] [Rel (DRSRel "feel_good") [DRSRef "x"]]
sm2 = drsToSDRS d2'
d3' = DRS [] [Rel (DRSRel "sick") [DRSRef "x"]]
sm3 = addDRS sm2 d3' [(0, relationFromRelName "Alternation")]
-- ^ In parallel, we build up the subdiscourse consisting of d2' and d3'
sm4 = addSDRS sm1 sm3 [(1, relationFromRelName "Result")]
-- ^ Finally, we attach the new SDRS to the existing SDRS

--------------------------------------------------------------------------
-- * Building up an SDRS containing a negated relation
-- "John isn't sick because he ate pasta but because he ate fish." or
-- "John is sick not because he ate pasta but because he ate fish."
--------------------------------------------------------------------------
d3'' = DRS [DRSRef "x"] [Rel (DRSRel "John") [DRSRef "x"],Rel (DRSRel "sick") [DRSRef "x"]]
d4 = DRS [] [Rel (DRSRel "eat_pasta") [DRSRef "x"]]
sn1 = drsToSDRS d3''
sn2 = addDRS sn1 d4 [(0,relationFromRelName "Explanation")]
-- ^ First the DRS, which is to be negated, is added normally
sn3 = negateRelation sn2 (relationFromRelName "Explanation") 0 1
-- ^ Then it is negated using its start and end nodes as identifiers
sn4 = addDRS sn3 d2 [(0,relationFromRelName "Explanation")]
