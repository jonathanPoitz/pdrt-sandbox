-- This is a tutorial for creating and manipulating SDRSs with SDRT-SANDBOX.

import qualified Data.Map as M
import Data.SDRS

-- | "John kisses Jane."
exampleDRS1 = DRS [DRSRef "x", DRSRef "y"] 
                  [Rel (DRSRel "John") [DRSRef "x"]
                  ,Rel (DRSRel "Jane") [DRSRef "y"]
                  ,Rel (DRSRel "kiss") [DRSRef "x", DRSRef "y"]]

-- | Embed exampleDRS1 into an SDRS
exampleSDRS1 = drsToSDRS exampleDRS1

-- | "Because of that, she is happy."
exampleDRS2 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "=") [DRSRef "x", DRSRef "y"]
                  ,Rel (DRSRel "happy") [DRSRef "x"]]

-- addDRS
exampleSDRS2 = addDRS exampleSDRS1 exampleDRS2 [(0,relationFromRelName "result")]

-- addSDRS

-- rf
rf2 = rf exampleSDRS2

-- | "So, she laughs."
exampleDRS3 = DRS [DRSRef "y"] 
                  [Rel (DRSRel "=") [DRSRef "y", DRSRef "x1"]
                  ,Rel (DRSRel "laugh") [DRSRef "y"]]

-- | "So, he laughs."
exampleDRS3b = DRS [DRSRef "y"] 
                  [Rel (DRSRel "=") [DRSRef "y", DRSRef "x"]
                  ,Rel (DRSRel "laugh") [DRSRef "y"]]

exampleSDRS3 = addDRS exampleSDRS2 exampleDRS3 [(1,relationFromRelName "result")]
exampleSDRS3b = addDRS exampleSDRS2 exampleDRS3b [(0,relationFromRelName "result")]

-- sdrsBoundRef/sdrsFreeRefs

-- sdrsProperDRSs/sdrsPureDRSs



-- discourseGraph

-- accessible DRSs

------ for printing:

-- embedded drss that are part of sdrsal07
_al07_1 = DRS [DRSRef "x", DRSRef "y"]
                [Rel (DRSRel "John") [DRSRef "x"]
                ,Rel (DRSRel "evening") [DRSRef "y"]
                ,Rel (DRSRel "great") [DRSRef "y"]
                ,Rel (DRSRel "have") [DRSRef "x", DRSRef "y"]]

_al07_2 = DRS [DRSRef "x1", DRSRef "z"]
                [Rel (DRSRel "=") [DRSRef "x1", DRSRef "x"]
                ,Rel (DRSRel "meal") [DRSRef "z"]
                ,Rel (DRSRel "great") [DRSRef "z"]
                ,Rel (DRSRel "have") [DRSRef "x1", DRSRef "z"]]

_al07_3 = DRS [DRSRef "x2", DRSRef "r"]
                [Rel (DRSRel "=") [DRSRef "x2", DRSRef "x"]
                ,Rel (DRSRel "salmon") [DRSRef "r"]
                ,Rel (DRSRel "eat") [DRSRef "x2", DRSRef "r"]]

_al07_4 = DRS [DRSRef "x3", DRSRef "s"]
                [Rel (DRSRel "=") [DRSRef "x3", DRSRef "x"]
                ,Rel (DRSRel "cheese") [DRSRef "s"]
                ,Rel (DRSRel "devour") [DRSRef "x3", DRSRef "s"]]

_al07_5 = DRS [DRSRef "x4", DRSRef "t"]
                [Rel (DRSRel "=") [DRSRef "x4", DRSRef "x"]
                ,Rel (DRSRel "dancing_competition") [DRSRef "t"]
                ,Rel (DRSRel "win") [DRSRef "x4", DRSRef "t"]]

_al07_6 = DRS [DRSRef "x5"]
                [Rel (DRSRel "=") [DRSRef "x5", DRSRef "x"]
                ,Rel (DRSRel "dance_well") [DRSRef "x5"]]


sdrsfullal07_to2 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 2)),
                                (1, EDU _al07_1),
                                (2, EDU _al07_2)]) 2


sdrsfullal07_to3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Elaboration") 2 3))),
                                (1, EDU _al07_1),
                                (2, EDU _al07_2),
                                (3, EDU _al07_3)]) 3

sdrsfullal07_to4 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Elaboration") 1 2)
                                             (Relation (relationFromRelName "Elaboration") 2 7))),
                                     (1, EDU _al07_1),
                                     (2, EDU _al07_2),
                                     (3, EDU _al07_3),
                                     (4, EDU _al07_4),
                                     (7, CDU (Relation (relationFromRelName "Narration") 3 4))]) 4

sdrsfullal07 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (5, EDU _al07_5),
                                 (6, CDU (And (Relation (relationFromRelName "Elaboration") 2 7)
                                          (Relation (relationFromRelName "Narration ") 2 5))),
                                 (7, CDU (Relation (relationFromRelName "Narration") 3 4))]) 5

