import qualified Data.Map as M
import Data.SDRS

drs0 = DRS [] []

-- relation update:
-- If I want to update the references of an sdrs because something changed with the outscopings, there is different possible cases:
-- case 1. the reference to be manipulated is the right arg of a relation, for example when adding a drs to sdrsfullal07_to3 with addDRS sdrsfullal07_to3 drs0 [(3,relationFromRelName "Narration")].

sdrsfullal07_to3 = SDRS (M.fromList [(0, And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Elaboration") 2 3)),
                (1, EDU drs0),
                (2, EDU drs0),
                (3, EDU drs0)]) 3

-- this then correctly yields:

sdrsfullal07_to4 = SDRS (M.fromList [(0, And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Elaboration") 2 5)),
                                       (1, EDU drs0),
                                       (2, EDU drs0),
                                       (3, EDU drs0),
                                       (4, EDU drs0),
                                       (5, Relation (relationFromRelName "Narration") 3 4)]) 4

-- only leaf nodes have to be modified, thus no other relation's outscoping label changes

-- case 2. The reference to be updated is on the left of a relation, but doesn't have any incoming relations, e.g.

sdrs_updateRefCase2_to2 = SDRS (M.fromList [(0, Relation (relationFromRelName "Elaboration") 1 2),
                                          (1, EDU drs0),
                                          (2, EDU drs0)]) 2

-- if one adds a DRS to 2 with Narration (addDRS sdrs_updateRefCase2_to2 drs0 [(2, relationFromRelName "Narration")]), then one should get:

sdrs_updateRefCase2_to3 = SDRS (M.fromList [(0, And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Narration") 1 3)),
                                          (1, EDU drs0),
                                          (2, EDU drs0),
                                          (3, EDU drs0)]) 3

-- but now, the tricky case: case 3. The reference to be updated is on the left side of a relation and has incoming edges, then it would look as follows:
-- addDRS sdrsfullal07_to4 drs0 [(2, relationFromRelName "Narration")]

sdrsfullal07_to4_2 = SDRS (M.fromList [(0, And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Elaboration") 2 5)),
                                       (1, EDU drs0),
                                       (2, EDU drs0),
                                       (3, EDU drs0),
                                       (4, EDU drs0),
                                       (5, Relation (relationFromRelName "Narration") 3 4)]) 4

-- should yield:

sdrsfullal07 = SDRS (M.fromList [(0, And (Relation (relationFromRelName "Elaboration") 1 7) (Relation (relationFromRelName "Elaboration") 2 5)),
                                 (1, EDU drs0),
                                 (2, EDU drs0),
                                 (3, EDU drs0),
                                 (4, EDU drs0),
                                 (5, Relation (relationFromRelName "Narration") 3 4),
                                 (6, EDU drs0),
                                 (7, And (Relation (relationFromRelName "Narration") 2 6)
                                         (Relation (relationFromRelName "Explanation") 2 5))]) 6

-- thus, as one can see, this update involves the removal of the relation Elab(2,5) from 0 and its addition to 7.
-- I don't know yet how to do that programmatically. Because currently, I iterate over the map and for each relation
-- (or recursive combination thereof), I replace the old label with the new (here this would be replacing 2 with 7).
-- But since I'm looking at each SDRSFormula one at a time, I don't know how to take Elab(2,5) out, store it and put
-- it back in the new place (under label 7) 
