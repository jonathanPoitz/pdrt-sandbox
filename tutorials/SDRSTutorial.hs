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

_al07_6 = DRS [DRSRef "x5"]
                [Rel (DRSRel "=") [DRSRef "x5", DRSRef "x"]
                ,Rel (DRSRel "dance_well") [DRSRef "x5"]]


------

_neg_1 = DRS [DRSRef "x"]
                [Rel (DRSRel "Mary") [DRSRef "x"]
                ,Rel (DRSRel "quit") [DRSRef "x"]]

_neg_2 = DRS [DRSRef "y"]
                [Rel (DRSRel "=") [DRSRef "y", DRSRef "x"]
                ,Neg(DRS [] [Rel (DRSRel "like_it") [DRSRef "y"]])]

_neg_3 = DRS [DRSRef "z"]
                [Rel (DRSRel "=") [DRSRef "z", DRSRef "x"]
                ,Rel (DRSRel "pregnant") [DRSRef "z"]]


---------------------------------------------------------------------------
-- | malformed DRSs
---------------------------------------------------------------------------
-- double DRSRef declaration
_al07_2b = DRS [DRSRef "x1", DRSRef "z", DRSRef "x1"]
                [Rel (DRSRel "=") [DRSRef "x1", DRSRef "x"]
                ,Rel (DRSRel "meal") [DRSRef "z"]
                ,Rel (DRSRel "great") [DRSRef "z"]
                ,Rel (DRSRel "have") [DRSRef "x1", DRSRef "z"]]

-- unnecessary declaration of y
_al07_2c = DRS [DRSRef "x1", DRSRef "z", DRSRef "y"]
                [Rel (DRSRel "=") [DRSRef "x1", DRSRef "x"]
                ,Rel (DRSRel "meal") [DRSRef "z"]
                ,Rel (DRSRel "great") [DRSRef "z"]
                ,Rel (DRSRel "have") [DRSRef "x1", DRSRef "z"]]



---------------------------------------------------------------------------
-- | misc structures
---------------------------------------------------------------------------

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

mapal07 = Map.fromList [(0, Relation (relationFromLabel "Explanation") 1 6),
                         (1, Segment _al07_1),
                         (2, Segment _al07_2),
                         (3, Segment _al07_3),
                         (4, Segment _al07_4),
                         (5, Segment _al07_5),
                         (6, And (Relation (relationFromLabel "Elaboration") 2 7) (Relation (relationFromLabel "Narration") 2 5)),
                         (7, Relation (relationFromLabel "Narration") 3 4)] :: Map.Map DisVar SDRSFormula

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
--recsf2 = And (Relation "sljs" 2 4)
--                (And 
--                  (Not
--                    (And 
--                      (Segment drs0) 
--                      (Relation "bla" 3 6)))
--                  (Segment drs0))

recsf2b = And (Segment drs0) (And (Not (And (Segment drs0) (Segment drs0))) (Segment drs0))

recsf3 = Not (Not (Not $ Segment drs0))

recsf4 = Not (Not (Not $ Relation (relationFromLabel "Explanation") 2 4))

--testSDRSs = [("sdrsrec1", sdrsrec1),
--             ("sdrsal07", sdrsal07),
--             ("sdrsdanlos4", sdrsdanlos4),
--             ("sdrsdanlos3", sdrsdanlos3),
--             ("sdrsdanlos2", sdrsdanlos2),
--             ("sdrsdanlos1", sdrsdanlos1),
--             ("sdrs1", sdrs1),
--             ("sdrs2", sdrs2)]

discourseGraph1 = Map.fromList [(0,[(1,Outscopes),(6,Outscopes)]),
                           (1,[(6,relationFromLabel "Elaboration")]),
                           (2,[(5,relationFromLabel "Narration"),(7,relationFromLabel "Elaboration")]),
                           (3,[(4,relationFromLabel "Narration")]),
                           (6,[(2,Outscopes),(5,Outscopes),(7,Outscopes)]),
                           (7,[(3,Outscopes),(4,Outscopes)])] :: Map.Map DisVar [(DisVar, SDRSRelation)]

---------------------------------------------------------------------------
-- | SDRSs
---------------------------------------------------------------------------

sdrs1 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Result") 1 2),
                                    (1, Segment drs3),
                                    (2, Segment drs4)]) 3

-- drs3 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [(DRSRef "x")],
--                                      Rel (DRSRel "glass") [(DRSRef "y")],
--                                      Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

-- drs4 = DRS [DRSRef "z"] [Rel (DRSRel "=") [(DRSRef "z"), (DRSRef "y")], 
--                          Rel (DRSRel "break") [(DRSRef "z")]]

-- exampleDRS6 = stringToDRS "<{x },  {man(x), happy(x), not <{},{sad(x)}> }>"

-- sdrs1_string = stringToSDRS "<{0:(Result,1,2); 1: <{x,y},{man(x), glass(y), drop(x,y)}>; 2: <{z},{=(z,y), break(z)}>}, 2>"

sdrs2 = SDRS (Map.fromList [(0, Segment $ DRS [] [])]) 0

sdrsdanlos1 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Explanation") 1 2),
                                            (1, Segment drs0),
                                            (2, Relation (relationFromLabel "Continuation") 3 4),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrsdanlos2 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Explanation") 1 2) (Relation (relationFromLabel "Explanation") 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrsdanlos3 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Explanation") 1 2) (Relation (relationFromLabel "Narration") 1 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

sdrsdanlos4 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Commentary") 1 2),
                                            (1, Relation (relationFromLabel "Explanation") 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 4

sdrsal07 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Elaboration") 1 6),
                                (1, Segment drs0),
                                (2, Segment drs0),
                                (3, Segment drs0),
                                (4, Segment drs0),
                                (5, Segment drs0),
                                (6, And (Relation (relationFromLabel "Elaboration") 2 7) (Relation (relationFromLabel "Narration") 2 5)),
                                (7, Relation (relationFromLabel "Narration") 3 4)]) 5

sdrsfullal07_to3 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Elaboration") 1 2) (Relation (relationFromLabel "Elaboration") 2 3)),
                                (1, Segment _al07_1),
                                (2, Segment _al07_2),
                                (3, Segment _al07_3)]) 3

sdrsfullal07_to4_comp = addDRS sdrsfullal07_to3 _al07_4 [(3, (relationFromLabel "Narration"))]

sdrsfullal07_to4 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Elaboration") 1 2) (Relation (relationFromLabel "Elaboration") 2 7)),
                                       (1, Segment _al07_1),
                                       (2, Segment _al07_2),
                                       (3, Segment _al07_3),
                                       (4, Segment _al07_4),
                                       (7, Relation (relationFromLabel "Narration") 3 4)]) 4

sdrsfullal07_to8 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Elaboration") 1 6),
                                   (1, Segment _al07_1),
                                   (2, Segment _al07_2),
                                   (3, Segment _al07_3),
                                   (4, Segment _al07_4),
                                   (5, Segment _al07_5),
                                   (8, Segment _al07_6),
                                   (6, And (Relation (relationFromLabel "Narration") 2 5)
                                       (And (Relation (relationFromLabel "Explanation") 2 7)
                                       (Relation (relationFromLabel "Elaboration") 5 8))),
                                   (7, Relation (relationFromLabel "Narration") 3 4)]) 8

sdrsfullal07 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Elaboration") 1 6),
                                   (1, Segment _al07_1),
                                   (2, Segment _al07_2),
                                   (3, Segment _al07_3),
                                   (4, Segment _al07_4),
                                   (5, Segment _al07_5),
                                   (6, And (Relation (relationFromLabel "Narration") 2 5)
                                           (Relation (relationFromLabel "Explanation") 2 7)),
                                   (7, Relation (relationFromLabel "Narration") 3 4)]) 5

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
-- the sfs in the And are switched
sdrsfullal07_iso = SDRS (Map.fromList [(0, Relation (relationFromLabel "Elaboration") 1 6),
                                (20, Segment _al07_2),
                                (1, Segment _al07_1),
                                (6, And (Relation (relationFromLabel "Narration") 20 49)
                                        (Relation (relationFromLabel "Explanation") 20 8)),
                                (3, Segment _al07_3),
                                (4, Segment _al07_4),
                                (49, Segment _al07_5),
                                (8, Relation (relationFromLabel "Narration") 3 4)]) 49

sdrsfullal07_to6_comp = addDRS sdrsfullal07 _al07_6 [(5, (relationFromLabel "Elaboration"))]

sdrsfullal07_to6_comp_2 = addDRS sdrsfullal07 _al07_6 [(4, (relationFromLabel "Narration"))]

sdrsneg1 = SDRS (Map.fromList [(0, Not (Relation (relationFromLabel "Explanation") 1 2)),
                                (1, Segment _neg_1),
                                (2, Segment _neg_2),
                                (3, Segment _neg_3),
                                (4, Relation (relationFromLabel "Explanation") 1 3),
                                (5, Relation (relationFromLabel "Contrast") 2 3)]) 3

-- for recursion testing only, not felicitous
--sdrsrec1 = SDRS (Map.fromList [(0, recsf1),
--                                 (1, recsf2),
--                                 (2, recsf3),
--                                 (3, recsf4)]) 0

-- "because"
sdrs_because d1 d2 = buildFromDRSs (relationFromLabel "Explanation") d1 d2

---------------------------------------------------------------------------
-- | malformed SDRSs
---------------------------------------------------------------------------

-- has self referencing relation
probselfref1 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Explanation") 1 1) (Relation (relationFromLabel "Explanation") 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
probselfref2 = SDRS (Map.fromList [(0, And (Relation (relationFromLabel "Explanation") 0 1) (Relation (relationFromLabel "Explanation") 2 3)),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- has self referencing relation
probselfref3 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Explanation") 1 1),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- This entry is problematic since it is deemed felicitous (although it isn't, since not all entries are in relations with eachother)
probnotenoughrels = SDRS (Map.fromList [(0, Relation (relationFromLabel "Explanation") 1 2),
                                            (1, Segment drs0),
                                            (2, Segment drs0),
                                            (3, Segment drs0)]) 3

-- the discourse graph is not well formed. TODO find out what rules exactly are broken here
probgraphnotwellformed = SDRS (Map.fromList [(0, Relation (relationFromLabel "Explanation") 1 5),
                                                (1, Segment drs0),
                                                (2, Segment drs0),
                                                (3, Segment drs0),
                                                (4, Segment drs0),
                                                (5, Segment drs0),
                                                (6, And (Relation (relationFromLabel "Explanation") 2 4) (Relation (relationFromLabel "Narration") 3 7)),
                                                (7, Relation (relationFromLabel "Narration") 4 7)]) 5

-- last is not a valid discourse variable
problast1 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Commentary") 1 2),
                                            (1, Relation (relationFromLabel "Explanation") 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 7

-- last is not a segment
problast2 = SDRS (Map.fromList [(0, Relation (relationFromLabel "Commentary") 1 2),
                                            (1, Relation (relationFromLabel "Explanation") 3 4),
                                            (2, Segment drs0),
                                            (3, Segment drs0),
                                            (4, Segment drs0)]) 1
