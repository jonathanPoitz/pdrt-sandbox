-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

import qualified Data.Map as M
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

drs3b = DRS [DRSRef "x", DRSRef "y", DRSRef "x1", DRSRef "y1"] [Rel (DRSRel "man") [(DRSRef "x")],
                                     Rel (DRSRel "glass") [(DRSRef "y")],
                                     Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

drs4b = DRS [DRSRef "z", DRSRef "z1"] [Rel (DRSRel "=") [(DRSRef "z"), (DRSRef "y")], 
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

map1 = M.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

mapal07 = M.fromList [(0, CDU (Relation (relationFromLabel "Explanation") 1 6)),
                         (1, EDU _al07_1),
                         (2, EDU _al07_2),
                         (3, EDU _al07_3),
                         (4, EDU _al07_4),
                         (5, EDU _al07_5),
                         (6, CDU (And (Relation (relationFromLabel "Elaboration") 2 7) (Relation (relationFromLabel "Narration") 2 5))),
                         (7, CDU (Relation (relationFromLabel "Narration") 3 4))] :: M.Map DisVar SDRSFormula

outscopesmap = M.fromList [(0,[1,6]),
                              (6,[2,7,5]),
                              (7,[3,4])]

--recsf1 = CDU $ And (EDU drs0) (Not (EDU drs0))

--recsf2b = CDU $ And (EDU drs0) (And (Not (And (EDU drs0) (EDU drs0))) (EDU drs0))

--recsf3 = CDU $ Not (Not (Not $ EDU drs0))

recsf4 = CDU $ Not (Not (Not $ Relation (relationFromLabel "Explanation") 2 4))

--testSDRSs = [("sdrsrec1", sdrsrec1),
--             ("sdrsal07", sdrsal07),
--             ("sdrsdanlos4", sdrsdanlos4),
--             ("sdrsdanlos3", sdrsdanlos3),
--             ("sdrsdanlos2", sdrsdanlos2),
--             ("sdrsdanlos1", sdrsdanlos1),
--             ("sdrs1", sdrs1),
--             ("sdrs2", sdrs2)]

discourseGraph1 = M.fromList [(0,[(1,Outscopes),(6,Outscopes)]),
                           (1,[(6,relationFromLabel "Elaboration")]),
                           (2,[(5,relationFromLabel "Narration"),(7,relationFromLabel "Elaboration")]),
                           (3,[(4,relationFromLabel "Narration")]),
                           (6,[(2,Outscopes),(5,Outscopes),(7,Outscopes)]),
                           (7,[(3,Outscopes),(4,Outscopes)])] :: M.Map DisVar [(DisVar, SDRSRelation)]

discourseGraphRFExt = M.fromList [(0,[(1,Outscopes),(2,Outscopes),(3,relationFromLabel "Explanation")]),
                                  (1,[(2,relationFromLabel "Parallel")]),
                                  --(2,[(4,relationFromLabel "Entity_Elaboration")]),
                                  (5,[(0,Outscopes)])] :: M.Map DisVar [(DisVar, SDRSRelation)]

---------------------------------------------------------------------------
-- | SDRSs
---------------------------------------------------------------------------
sdrsmerge1 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Result") 1 2)),
                                    (1, EDU drs3),
                                    (2, EDU drs4)]) 2

sdrsmerge2 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Result") 1 2)),
                                    (1, EDU drs3b),
                                    (2, EDU drs4b)]) 2

sdrs1 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Result") 1 2)),
                                    (1, EDU drs3),
                                    (2, EDU drs4)]) 2

-- drs3 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [(DRSRef "x")],
--                                      Rel (DRSRel "glass") [(DRSRef "y")],
--                                      Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

-- drs4 = DRS [DRSRef "z"] [Rel (DRSRel "=") [(DRSRef "z"), (DRSRef "y")], 
--                          Rel (DRSRel "break") [(DRSRef "z")]]

-- exampleDRS6 = stringToDRS "<{x },  {man(x), happy(x), not <{},{sad(x)}> }>"

sdrs1_string = stringToSDRS "<{0:(Result,1,2) & (Result,2,3); 1: <{x,y},{man(x), glass(y), drop(x,y)}>; 2: <{z},{=(z,y), break(z)}>, 3: <{x',z'},{=(x',x), =(z',z), remove(x',z')}>}, 3>"

sdrs2 = SDRS (M.fromList [(0, EDU $ DRS [] [])]) 0

sdrs_updateRefCase2_to2 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 2)),
                                          (1, EDU drs0),
                                          (2, EDU drs0)]) 2

sdrs_updateRefCase2_to3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Elaboration") 1 2) (Relation (relationFromLabel "Narration") 1 3))),
                                          (1, EDU drs0),
                                          (2, EDU drs0),
                                          (3, EDU drs0)]) 3


sdrsdanlos1 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Explanation") 1 2)),
                                            (1, EDU drs0),
                                            (2, CDU (Relation (relationFromLabel "Continuation") 3 4)),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 4

sdrsdanlos2 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Explanation") 1 2) (Relation (relationFromLabel "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

sdrsdanlos3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Explanation") 1 2) (Relation (relationFromLabel "Narration") 1 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

sdrsdanlos4 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromLabel "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 4

sdrsal07 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                (1, EDU drs0),
                                (2, EDU drs0),
                                (3, EDU drs0),
                                (4, EDU drs0),
                                (5, EDU drs0),
                                (6, CDU (And (Relation (relationFromLabel "Elaboration") 2 7) (Relation (relationFromLabel "Narration") 2 5))),
                                (7, CDU (Relation (relationFromLabel "Narration") 3 4))]) 5



sdrsfullal07_to2 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 2)),
                                (1, EDU _al07_1),
                                (2, EDU _al07_2)]) 2


sdrsfullal07_to3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Elaboration") 1 2) (Relation (relationFromLabel "Elaboration") 2 3))),
                                (1, EDU _al07_1),
                                (2, EDU _al07_2),
                                (3, EDU _al07_3)]) 3

--sdrsfullal07_to4_comp = addDRS sdrsfullal07_to3 _al07_4 [(3, (relationFromLabel "Narration"))]

sdrsfullal07_to4 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Elaboration") 1 2)
                                             (Relation (relationFromLabel "Elaboration") 2 5))),
                                     (1, EDU _al07_1),
                                     (2, EDU _al07_2),
                                     (3, EDU _al07_3),
                                     (4, EDU _al07_4),
                                     (5, CDU (Relation (relationFromLabel "Narration") 3 4))]) 4

sdrsfullal07 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (5, EDU _al07_5),
                                 (6, CDU (And (Relation (relationFromLabel "Narration") 2 5)
                                          (Relation (relationFromLabel "Explanation") 2 7))),
                                 (7, CDU (Relation (relationFromLabel "Narration") 3 4))]) 5

sdrsfullal07_to8 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                   (1, EDU _al07_1),
                                   (2, EDU _al07_2),
                                   (3, EDU _al07_3),
                                   (4, EDU _al07_4),
                                   (5, EDU _al07_5),
                                   (8, EDU _al07_6),
                                   (6, CDU (And (Relation (relationFromLabel "Narration") 2 5)
                                       (And (Relation (relationFromLabel "Explanation") 2 7)
                                       (Relation (relationFromLabel "Elaboration") 5 8)))),
                                   (7, CDU (Relation (relationFromLabel "Narration") 3 4))]) 8

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
-- the sfs in the And are switched
sdrsfullal07_iso = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                (20, EDU _al07_2),
                                (1, EDU _al07_1),
                                (6, CDU (And (Relation (relationFromLabel "Narration") 20 49)
                                        (Relation (relationFromLabel "Explanation") 20 8))),
                                (3, EDU _al07_3),
                                (4, EDU _al07_4),
                                (49, EDU _al07_5),
                                (8, CDU (Relation (relationFromLabel "Narration") 3 4))]) 49

--sdrsfullal07_to6_comp = addDRS sdrsfullal07 _al07_6 [(5, (relationFromLabel "Elaboration"))]

--sdrsfullal07_to6_comp_2 = addDRS sdrsfullal07 _al07_6 [(4, (relationFromLabel "Narration"))]

sdrsneg1 = SDRS (M.fromList [(0, CDU (Not (Relation (relationFromLabel "Explanation") 1 2))),
                                (1, EDU _neg_1),
                                (2, EDU _neg_2),
                                (3, EDU _neg_3),
                                (4, CDU (Relation (relationFromLabel "Explanation") 1 3)),
                                (5, CDU (Relation (relationFromLabel "Contrast") 2 3))]) 3

-- for recursion testing only, not felicitous
--sdrsrec1 = SDRS (M.fromList [(0, recsf1),
--                                 (1, recsf2),
--                                 (2, recsf3),
--                                 (3, recsf4)]) 0

-- One plaintiff was passed over for promotion three times.
-- Another didn't get a raise for five years.
-- A third plaintiff was given a lower wage compared to others.
-- These people were badly treated.
-- But the jury didn't believe this.
sdrs_plaintiff = SDRS (M.fromList [(0, CDU (Not (Relation (relationFromLabel "Explanation") 1 2))),
                                (1, EDU drs0), -- one plaintiff
                                (2, EDU drs0), -- another didn't
                                (3, EDU drs0), -- a third
                                (4, EDU drs0), -- these people
                                (6, EDU drs0), -- but the jury
                                (0, CDU (And (Relation (relationFromLabel "Continuation") 1 2)
                                        (Relation (relationFromLabel "Continuation") 2 3))),
                                (5, CDU (Relation (relationFromLabel "Topic") 0 4)),
                                (7, CDU (Relation (relationFromLabel "Contrast") 4 6))]) 3

sdrsRFExt = SDRS (M.fromList [(5, CDU (Relation (relationFromLabel "Explanation") 0 3)),
                              (0, CDU (And (Relation (relationFromLabel "Parallel") 1 2)
                                      (Relation (relationFromLabel "Entity_Elaboration") 2 4)))]) 3


---------------------------------------------------------------------------
-- | malformed SDRSs
---------------------------------------------------------------------------

-- has self referencing relation
probselfref1 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Explanation") 1 1) (Relation (relationFromLabel "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- has self referencing relation
probselfref2 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromLabel "Explanation") 0 1) (Relation (relationFromLabel "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- has self referencing relation
probselfref3 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Explanation") 1 1)),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- This entry is problematic since it is deemed felicitous (although it isn't, since not all entries are in relations with eachother)
probnotenoughrels = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Explanation") 1 2)),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- the discourse graph is not well formed. TODO find out what rules exactly are broken here
probgraphnotwellformed = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Explanation") 1 5)),
                                                (1, EDU drs0),
                                                (2, EDU drs0),
                                                (3, EDU drs0),
                                                (4, EDU drs0),
                                                (5, EDU drs0),
                                                (6, CDU (And (Relation (relationFromLabel "Explanation") 2 4) (Relation (relationFromLabel "Narration") 3 7))),
                                                (7, CDU (Relation (relationFromLabel "Narration") 4 7))]) 5

-- last is not a valid discourse variable
problast1 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromLabel "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 7

-- last is not a segment
problast2 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromLabel "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 1

root_sdrsfullal07 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (5, EDU _al07_5),
                                 (6, CDU (Relation (relationFromLabel "Narration") 2 5)),
                                 (7, CDU (Relation (relationFromLabel "Narration") 3 4))]) 5

-- sdrs without added new segment (5)
sdrsfullal07_wo5 = SDRS (M.fromList [(0, CDU (Relation (relationFromLabel "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (6, CDU (And (Relation (relationFromLabel "Narration") 2 5)
                                              (Relation (relationFromLabel "Explanation") 2 7))),
                                 (7, CDU (Relation (relationFromLabel "Narration") 3 4))]) 5


---------------------------------------------------------------------------
-- | Example sentences
---------------------------------------------------------------------------

-- If Mary sees a cat, she pets it. But if Peter sees it, he takes it home. 
-- It started raining. After he finished his shower, Peter noticed.
-- 

---------------------------------------------------------------------------
-- | Merges
---------------------------------------------------------------------------

--mergeRes = sdrsMerge sdrsmerge1 sdrsmerge2 [(0,relationFromLabel "Result")]
--merge2NarElab = sdrsMerge sdrsmerge1 sdrsmerge2 [(0,relationFromLabel "Narration"),(2, relationFromLabel "Elaboration")]





