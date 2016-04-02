-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

--map (\(x,y) -> (x, hasOnlyFOLDRSs y)) testSDRSs

import qualified Data.Map as Map
import Data.SDRS

---------------------------------------------------------------------------
-- | DRSs
---------------------------------------------------------------------------

drs0 = DRS [] []

-- | "A man is happy."
drs1 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "man") [DRSRef "x"]
                  ,Rel (DRSRel "happy") [DRSRef "x"]]

-- | "A woman is sad."
drs2 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "woman") [DRSRef "x"]
                  ,Rel (DRSRel "sad") [DRSRef "x"]]

drs3 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [(DRSRef "x")],
                                     Rel (DRSRel "glass") [(DRSRef "y")],
                                     Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

drs4 = DRS [DRSRef "z"] [Rel (DRSRel "=") [(DRSRef "z"), (DRSRef "y")], 
                         Rel (DRSRel "break") [(DRSRef "z")]]

drs5 = DRS [DRSRef "x"] [Rel (DRSRel "Fred") [(DRSRef "x")], 
                         Rel (DRSRel "has_bad_mood") [(DRSRef "x")]]

-- | empty DRS
exampleDRS1 = DRS [] []

-- | "A man is happy."
exampleDRS2 = DRS [DRSRef "x"] 
                  [Rel (DRSRel "man") [DRSRef "x"]
                  ,Rel (DRSRel "happy") [DRSRef "x"]]

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

---------------------------------------------------------------------------
-- | misc structures
---------------------------------------------------------------------------

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

mapal07 = Map.fromList [(0, Relation "Elaboration" 1 6),
                         (1, Segment _al07_1),
                         (2, Segment _al07_2),
                         (3, Segment _al07_3),
                         (4, Segment _al07_4),
                         (5, Segment _al07_5),
                         (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                         (7, Relation "Narration" 3 4)] :: Map.Map DisVar SDRSFormula

outscopesmap = Map.fromList [(0,[1,6]),
                              (6,[2,7,5]),
                              (7,[3,4])]

--graphmap = Map.fromList [(0,[1,6]),
--                          (1,[]),
--                          (6,[2,7,5]),
--                          (2,[]),
--                          (5,[]),
--                          (7,[3,4]),
--                          (3,[]),
--                          (4,[])]

recsf1 = And (Segment drs0) (Not (Segment drs0))
recsf2 = And (Relation "sljs" 2 4)
                (And 
                  (Not
                    (And 
                      (Segment drs0) 
                      (Relation "bla" 3 6)))
                  (Segment drs0))

recsf2b = And (Segment drs0) (And (Not (And (Segment drs0) (Segment drs0))) (Segment drs0))

recsf3 = Not (Not (Not $ Segment drs0))

recsf4 = Not (Not (Not $ Relation "Explanation" 2 4))

testSDRSs = [("sdrsrec1", sdrsrec1),
             ("sdrsal07", sdrsal07),
             ("sdrsdanlos4", sdrsdanlos4),
             ("sdrsdanlos3", sdrsdanlos3),
             ("sdrsdanlos2", sdrsdanlos2),
             ("sdrsdanlos1", sdrsdanlos1),
             ("sdrs1", sdrs1),
             ("sdrs2", sdrs2)]

---------------------------------------------------------------------------
-- | SDRSs
---------------------------------------------------------------------------

sdrs1 = SDRS (Map.fromList [(0, Relation "Result" 1 2),
                                    (1, Segment $ drs3),
                                    (2, Segment $ drs4)]) 0

sdrs2 = SDRS (Map.fromList [(0, Segment $ DRS [] [])]) 0

sdrsdanlos1 = SDRS (Map.fromList [(0, Relation "Explanation" 1 2),
                                            (1, Segment drs0),
                                            (2, Relation "Continuation" 3 4),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrsdanlos2 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrsdanlos3 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Narration" 1 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrsdanlos4 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrsal07 = SDRS (Map.fromList [(0, Relation "Elaboration" 1 6),
                                (1, Segment drs0),
                                (2, Segment drs0),
                                (3, Segment drs0),
                                (4, Segment drs0),
                                (5, Segment drs0),
                                (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                                (7, Relation "Narration" 3 4)]) 5

sdrsfullal07 = SDRS (Map.fromList [(0, Relation "Elaboration" 1 6),
                                (1, Segment _al07_1),
                                (2, Segment _al07_2),
                                (3, Segment _al07_3),
                                (4, Segment _al07_4),
                                (5, Segment _al07_5),
                                (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                                (7, Relation "Narration" 3 4)]) 5

-- for recursion testing only, not felicitous
sdrsrec1 = SDRS (Map.fromList [(0, recsf1),
                                 (1, recsf2),
                                 (2, recsf3),
                                 (3, recsf4)]) 0

---------------------------------------------------------------------------
-- | malformed SDRSs
---------------------------------------------------------------------------

-- has self referencing relation
probselfref1 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 1) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
probselfref2 = SDRS (Map.fromList [(0, And (Relation "Explanation" 0 1) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
probselfref3 = SDRS (Map.fromList [(0, Relation "Explanation" 1 1),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- This entry is problematic since it is deemed felicitous (although it isn't, since not all entries are in relations with eachother)
probnotenoughrels = SDRS (Map.fromList [(0, Relation "Explanation" 1 2),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- the discourse graph is not well formed. TODO find out what rules exactly are broken here
probgraphnotwellformed = SDRS (Map.fromList [(0, Relation "Elaboration" 1 5),
                                                (1, Segment drs0),
                                                (2, Segment drs0),
                                                (3, Segment drs0),
                                                (4, Segment drs0),
                                                (5, Segment drs0),
                                                (6, And (Relation "Elaboration" 2 4) (Relation "Narration" 3 7)),
                                                (7, Relation "Narration" 4 7)]) 5

-- last is not a valid discourse variable
problast1 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 7

-- last is not a segment
problast2 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 1
