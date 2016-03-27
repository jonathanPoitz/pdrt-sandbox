-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

--map (\(x,y) -> (x, hasOnlyFOLDRSs y)) testSDRSs

import qualified Data.Map as Map
import Data.SDRS

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

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

sdrs1 = SDRS (Map.fromList [(0, Relation "Result" 1 2),
                                    (1, Segment $ drs3),
                                    (2, Segment $ drs4)]) 0

sdrs2 = SDRS (Map.fromList [(0, Segment $ DRS [] [])]) 0

sdrs_danlos_1 = SDRS (Map.fromList [(0, Relation "Explanation" 1 2),
                                            (1, Segment drs0),
                                            (2, Relation "Continuation" 3 4),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrs_danlos_2 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrs_danlos_3 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Narration" 1 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrs_danlos_4 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrs_al07 = SDRS (Map.fromList [(0, Relation "Elaboration" 1 6),
                                (1, Segment drs0),
                                (2, Segment drs0),
                                (3, Segment drs0),
                                (4, Segment drs0),
                                (5, Segment drs0),
                                (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                                (7, Relation "Narration" 3 4)]) 5

map_al07 = Map.fromList [(0, Relation "Elaboration" 1 6),
                         (1, Segment drs0),
                         (2, Segment drs0),
                         (3, Segment drs0),
                         (4, Segment drs0),
                         (5, Segment drs0),
                         (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                         (7, Relation "Narration" 3 4)] :: Map.Map DisVar SDRSFormula

outscopes_map = Map.fromList [(0,[1,6]),
                              (6,[2,7,5]),
                              (7,[3,4])]

--graph_map = Map.fromList [(0,[1,6]),
--                          (1,[]),
--                          (6,[2,7,5]),
--                          (2,[]),
--                          (5,[]),
--                          (7,[3,4]),
--                          (3,[]),
--                          (4,[])]

rec_sdrs1 = And (Segment drs0) (Not (Segment drs0))
rec_sdrs2 = And (Segment drs0)
                (And 
                  (Not
                    (And 
                      (Segment drs0) 
                      (Segment drs0)))
                  (Segment drs0))

rec_sdrs2b = And (Segment drs0) (And (Not (And (Segment drs0) (Segment drs0))) (Segment drs0))

rec_sdrs3 = Not (Not (Not $ Segment drs0))

-- for recursion testing only, not felicitous
sdrs_rec_1 = SDRS (Map.fromList [(0, rec_sdrs1),
                                 (1, rec_sdrs2),
                                 (2, rec_sdrs3)]) 0

testSDRSs = [("sdrs_rec_1", sdrs_rec_1),
             ("sdrs_al07", sdrs_al07),
             ("sdrs_danlos_4", sdrs_danlos_4),
             ("sdrs_danlos_3", sdrs_danlos_3),
             ("sdrs_danlos_2", sdrs_danlos_2),
             ("sdrs_danlos_1", sdrs_danlos_1),
             ("sdrs1", sdrs1),
             ("sdrs2", sdrs2)]

-- ==============================================================
-- Malformed SDRSs
-- ==============================================================

-- has self referencing relation
prob_selfref1 = SDRS (Map.fromList [(0, And (Relation "Explanation" 1 1) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
prob_selfref2 = SDRS (Map.fromList [(0, And (Relation "Explanation" 0 1) (Relation "Explanation" 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
prob_selfref3 = SDRS (Map.fromList [(0, Relation "Explanation" 1 1),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- This entry is problematic since it is deemed felicitous (although it isn't, since not all entries are in relations with eachother)
prob_not_enough_rels = SDRS (Map.fromList [(0, Relation "Explanation" 1 2),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- the discourse graph is not well formed. TODO find out what rules exactly are broken here
prob_graph_not_wellformed = SDRS (Map.fromList [(0, Relation "Elaboration" 1 5),
                                                (1, Segment drs0),
                                                (2, Segment drs0),
                                                (3, Segment drs0),
                                                (4, Segment drs0),
                                                (5, Segment drs0),
                                                (6, And (Relation "Elaboration" 2 4) (Relation "Narration" 3 7)),
                                                (7, Relation "Narration" 4 7)]) 5

-- last is not a valid discourse variable
prob_last1 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 7

-- last is not a segment
prob_last2 = SDRS (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 1
