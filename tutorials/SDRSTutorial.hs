-- This is a sandbox used for creating and manipulating SDRSs with SDRT-SANDBOX 
-- as part of the thesis "An Implementation of SDRT" by Jonathan Poitz.

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
                  ,Rel (DRSRel "happy") [DRSRef "x"]]

drs3 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [(DRSRef "x")],
                                     Rel (DRSRel "glass") [(DRSRef "y")],
                                     Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

drs4 = DRS [] [Rel (DRSRel "break") [(DRSRef "y")]]

drs5 = DRS [DRSRef "x"] [Rel (DRSRel "John") [(DRSRef "x")],
                                     Rel (DRSRel "make_dinner") [(DRSRef "x")]]

drs6 = DRS [] [Rel (DRSRel "make_pasta") [(DRSRef "x")]]

drs3b = DRS [DRSRef "x", DRSRef "y", DRSRef "x1", DRSRef "y1"] [Rel (DRSRel "man") [(DRSRef "x")],
                                     Rel (DRSRel "glass") [(DRSRef "y")],
                                     Rel (DRSRel "drop") [(DRSRef "x"), (DRSRef "y")]]

drs4b = DRS [DRSRef "z", DRSRef "z1"] [Rel (DRSRel "=") [(DRSRef "z"), (DRSRef "y")], 
                         Rel (DRSRel "break") [(DRSRef "z")]]

--drs5 = DRS [DRSRef "x"] [Rel (DRSRel "Fred") [(DRSRef "x")], 
                         --Rel (DRSRel "has_bad_mood") [(DRSRef "x")]]

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

_al07_1b = DRS [DRSRef "x"]
                [Rel (DRSRel "John") [DRSRef "x"]
                ,Rel (DRSRel "has_great_eve") [DRSRef "x"]]

_al07_2 = DRS [DRSRef "z"]
                [Rel (DRSRel "meal") [DRSRef "z"]
                ,Rel (DRSRel "great") [DRSRef "z"]
                ,Rel (DRSRel "have") [DRSRef "x", DRSRef "z"]]

_al07_3 = DRS [DRSRef "r"]
                [Rel (DRSRel "salmon") [DRSRef "r"]
                ,Rel (DRSRel "eat") [DRSRef "x", DRSRef "r"]]

_al07_4 = DRS [DRSRef "s"]
                [Rel (DRSRel "cheese") [DRSRef "s"]
                ,Rel (DRSRel "devour") [DRSRef "x", DRSRef "s"]]

_al07_5 = DRS [DRSRef "t"]
                [Rel (DRSRel "dancing_competition") [DRSRef "t"]
                ,Rel (DRSRel "win") [DRSRef "x", DRSRef "t"]]

_al07_6 = DRS [] [Rel (DRSRel "dance_well") [DRSRef "x5"]]


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

mapal07 = M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 1 6)),
                         (1, EDU _al07_1),
                         (2, EDU _al07_2),
                         (3, EDU _al07_3),
                         (4, EDU _al07_4),
                         (5, EDU _al07_5),
                         (6, CDU (And (Relation (relationFromRelName "Elaboration") 2 7) (Relation (relationFromRelName "Narration") 2 5))),
                         (7, CDU (Relation (relationFromRelName "Narration") 3 4))] :: M.Map DisVar SDRSFormula

outscopesmap = M.fromList [(0,[1,6]),
                           (6,[2,7,5]),
                           (7,[3,4])]

--recsf1 = CDU $ And (EDU drs0) (Not (EDU drs0))

--recsf2b = CDU $ And (EDU drs0) (And (Not (And (EDU drs0) (EDU drs0))) (EDU drs0))

--recsf3 = CDU $ Not (Not (Not $ EDU drs0))

recsf4 = CDU $ Not (Not (Not $ Relation (relationFromRelName "Explanation") 2 4))

--testSDRSs = [("sdrsrec1", sdrsrec1),
--             ("sdrsal07", sdrsal07),
--             ("sdrsdanlos4", sdrsdanlos4),
--             ("sdrsdanlos3", sdrsdanlos3),
--             ("sdrsdanlos2", sdrsdanlos2),
--             ("sdrsdanlos1", sdrsdanlos1),
--             ("sdrs1", sdrs1),
--             ("sdrs2", sdrs2)]

--discourseGraph1 = M.fromList [(0,[(1,Outscopes),(6,Outscopes)]),
--                           (1,[(6,relationFromRelName "Elaboration")]),
--                           (2,[(5,relationFromRelName "Narration"),(7,relationFromRelName "Elaboration")]),
--                           (3,[(4,relationFromRelName "Narration")]),
--                           (6,[(2,Outscopes),(5,Outscopes),(7,Outscopes)]),
--                           (7,[(3,Outscopes),(4,Outscopes)])] :: M.Map DisVar [(DisVar, SDRSRelation)]

--discourseGraphRFExt = M.fromList [(0,[(1,Outscopes),(2,Outscopes),(3,relationFromRelName "Explanation")]),
--                                  (1,[(2,relationFromRelName "Parallel")]),
--                                  --(2,[(4,relationFromRelName "Entity_Elaboration")]),
--                                  (5,[(0,Outscopes)])] :: M.Map DisVar [(DisVar, SDRSRelation)]

---------------------------------------------------------------------------
-- | SDRSs
---------------------------------------------------------------------------
exSDRS1 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 2)),
                                    (1, EDU drs0),
                                    (2, EDU drs0)]) 2

exSDRS2 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Elaboration") 1 2)
                                            (Relation (relationFromRelName "Elaboration") 2 3))),
                                    (1, EDU drs0),
                                    (2, EDU drs0),
                                    (3, EDU drs0)]) 3

exSDRS3 = addDRS exSDRS1 drs0 [(2, relationFromRelName "Narration")]

sdrs1 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Result") 1 2)),
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

sdrs_updateRefCase2_to2 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 2)),
                                          (1, EDU drs0),
                                          (2, EDU drs0)]) 2

sdrs_updateRefCase2_to3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Elaboration") 1 2) (Relation (relationFromRelName "Narration") 1 3))),
                                          (1, EDU drs0),
                                          (2, EDU drs0),
                                          (3, EDU drs0)]) 3


sdrsdanlos1 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 1 2)),
                                            (1, EDU drs0),
                                            (2, CDU (Relation (relationFromRelName "Continuation") 3 4)),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 4

sdrsdanlos2 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Explanation") 1 2) (Relation (relationFromRelName "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

sdrsdanlos3 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Explanation") 1 2) (Relation (relationFromRelName "Narration") 1 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

sdrsdanlos4 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromRelName "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 4

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

sdrsfullal07_to8 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                   (1, EDU _al07_1),
                                   (2, EDU _al07_2),
                                   (3, EDU _al07_3),
                                   (4, EDU _al07_4),
                                   (5, EDU _al07_5),
                                   (8, EDU _al07_6),
                                   (6, CDU (And (Relation (relationFromRelName "Narration") 2 5)
                                       (And (Relation (relationFromRelName "Elaboration") 2 7)
                                       (Relation (relationFromRelName "Elaboration") 5 8)))),
                                   (7, CDU (Relation (relationFromRelName "Narration") 3 4))]) 8

sdrsfullal07_to8b = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 8)),
                                   (1, EDU _al07_1),
                                   (2, EDU _al07_2),
                                   (3, EDU _al07_3),
                                   (4, EDU _al07_4),
                                   (5, EDU _al07_5),
                                   (6, EDU _al07_6),
                                   (8, CDU (And (Relation (relationFromRelName "Elaboration") 2 3)
                                       (And (Relation (relationFromRelName "Narration") 2 4)
                                       (Relation (relationFromRelName "Elaboration") 4 7)))),
                                   (7, CDU (Relation (relationFromRelName "Narration") 5 6))]) 6

sdrslotsofsub = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Elaboration") 1 2)
                                                (And (Relation (relationFromRelName "Elaboration") 2 3)
                                                     (Relation (relationFromRelName "Elaboration") 3 4)))),
                                  (1, EDU drs0),
                                  (2, EDU drs0),
                                  (3, EDU drs0),
                                  (4, EDU drs0)]) 4

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
-- the sfs in the And are switched
-- the drss are different
sdrsfullal07_iso1 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                (20, EDU _al07_2),
                                (1, EDU _al07_1),
                                (6, CDU (And (Relation (relationFromRelName "Narration") 20 49)
                                        (Relation (relationFromRelName "Elaboration") 20 8))),
                                (3, EDU _al07_3),
                                (4, EDU _al07_4),
                                (49, EDU _al07_6),
                                (8, CDU (Relation (relationFromRelName "Narration") 3 4))]) 49

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
-- the sfs in the And are switched
sdrsfullal07_iso2 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                (20, EDU _al07_2),
                                (1, EDU _al07_1),
                                (6, CDU (And (Relation (relationFromRelName "Narration") 20 49)
                                        (Relation (relationFromRelName "Elaboration") 20 8))),
                                (3, EDU _al07_3),
                                (4, EDU _al07_4),
                                (49, EDU _al07_5),
                                (8, CDU (Relation (relationFromRelName "Narration") 3 4))]) 49

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
sdrsfullal07_iso3 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                (20, EDU _al07_2),
                                (1, EDU _al07_1),
                                (6, CDU (And (Relation (relationFromRelName "Elaboration") 20 8)
                                        (Relation (relationFromRelName "Narration") 20 49))),
                                (3, EDU _al07_3),
                                (4, EDU _al07_4),
                                (49, EDU _al07_5),
                                (8, CDU (Relation (relationFromRelName "Narration") 3 4))]) 49

-- the order of declaration is different (does it affect the map of is it implicitly ordered?)
-- the labels are different and have holes
-- the drss are different
sdrsfullal07_iso4 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                (20, EDU _al07_2),
                                (1, EDU _al07_1),
                                (6, CDU (And (Relation (relationFromRelName "Elaboration") 20 8)
                                        (Relation (relationFromRelName "Narration") 20 49))),
                                (3, EDU _al07_3),
                                (4, EDU _al07_4),
                                (49, EDU _al07_6),
                                (8, CDU (Relation (relationFromRelName "Narration") 3 4))]) 49

--sdrsfullal07_to6_comp = addDRS sdrsfullal07 _al07_6 [(5, (relationFromRelName "Elaboration"))]

--sdrsfullal07_to6_comp_2 = addDRS sdrsfullal07 _al07_6 [(4, (relationFromRelName "Narration"))]

sdrsneg1b_to2 = SDRS (M.fromList [(0, CDU $ Relation (relationFromRelName "Explanation") 1 2),
                                 (1, EDU _neg_1),
                                 (2, EDU _neg_2)]) 2

sdrsneg1 = SDRS (M.fromList [(0, CDU $ And (Not (Relation (relationFromRelName "Explanation") 1 2))
                                       (And (Relation (relationFromRelName "Explanation") 1 3)
                                           (Relation (relationFromRelName "Contrast") 2 3))),
                             (1, EDU _neg_1),
                             (2, EDU _neg_2),
                             (3, EDU _neg_3)]) 3

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
sdrs_plaintiff = SDRS (M.fromList [(0, CDU (Not (Relation (relationFromRelName "Explanation") 1 2))),
                                (1, EDU drs0), -- one plaintiff
                                (2, EDU drs0), -- another didn't
                                (3, EDU drs0), -- a third
                                (4, EDU drs0), -- these people
                                (6, EDU drs0), -- but the jury
                                (0, CDU (And (Relation (relationFromRelName "Continuation") 1 2)
                                        (Relation (relationFromRelName "Continuation") 2 3))),
                                (5, CDU (Relation (relationFromRelName "Topic") 0 4)),
                                (7, CDU (Relation (relationFromRelName "Contrast") 4 6))]) 3

sdrsRFExtBefore = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 4 3)),
                                    (4, CDU (Relation (relationFromRelName "Parallel") 1 2)),
                                    (1, EDU drs0),
                                    (2, EDU drs0),
                                    (3, EDU drs0)]) 3

sdrsRFExtBeforeBefore = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Parallel") 1 2)),
                                    (1, EDU drs0),
                                    (2, EDU drs0)]) 2

sdrsExtAcc = SDRS (M.fromList [(0, EDU $ DRS [DRSRef "s"] []),
                               (1, EDU drs0),
                               (2, EDU drs0),
                               (3, EDU drs0),
                               (4, CDU (Relation (relationFromRelName "Consequence") 0 1)),
                               (5, CDU (Relation (relationFromRelName "Consequence") 2 3)),
                               (6, CDU (Relation (relationFromRelName "Contrast") 4 5))]) 3

---------------------------------------------------------------------------
-- | malformed SDRSs
---------------------------------------------------------------------------

-- has self referencing relation
probselfref1 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Explanation") 1 1) (Relation (relationFromRelName "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- has self referencing relation
probselfref2 = SDRS (M.fromList [(0, CDU (And (Relation (relationFromRelName "Explanation") 0 1) (Relation (relationFromRelName "Explanation") 2 3))),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- has self referencing relation
probselfref3 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 1 1)),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- This entry is problematic since it is deemed felicitous (although it isn't, since not all entries are in relations with eachother)
probnotenoughrels = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 1 2)),
                                            (1, EDU drs0),
                                            (2, EDU drs0),
                                            (3, EDU drs0)]) 3

-- the discourse graph is not well formed. TODO find out what rules exactly are broken here
probgraphnotwellformed = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Explanation") 1 5)),
                                                (1, EDU drs0),
                                                (2, EDU drs0),
                                                (3, EDU drs0),
                                                (4, EDU drs0),
                                                (5, EDU drs0),
                                                (6, CDU (And (Relation (relationFromRelName "Explanation") 2 4) (Relation (relationFromRelName "Narration") 3 7))),
                                                (7, CDU (Relation (relationFromRelName "Narration") 4 7))]) 5

-- last is not a valid discourse variable
problast1 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromRelName "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 7

-- last is not a segment
problast2 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Commentary") 1 2)),
                                            (1, CDU (Relation (relationFromRelName "Explanation") 3 4)),
                                            (2, EDU drs0),
                                            (3, EDU drs0),
                                            (4, EDU drs0)]) 1

root_sdrsfullal07 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (5, EDU _al07_5),
                                 (6, CDU (Relation (relationFromRelName "Narration") 2 5)),
                                 (7, CDU (Relation (relationFromRelName "Narration") 3 4))]) 5

-- sdrs without added new segment (5)
sdrsfullal07_wo5 = SDRS (M.fromList [(0, CDU (Relation (relationFromRelName "Elaboration") 1 6)),
                                 (1, EDU _al07_1),
                                 (2, EDU _al07_2),
                                 (3, EDU _al07_3),
                                 (4, EDU _al07_4),
                                 (6, CDU (And (Relation (relationFromRelName "Narration") 2 5)
                                              (Relation (relationFromRelName "Explanation") 2 7))),
                                 (7, CDU (Relation (relationFromRelName "Narration") 3 4))]) 5


---------------------------------------------------------------------------
-- | Example sentences, Demo
---------------------------------------------------------------------------

d1 = DRS [DRSRef "x"] [Rel (DRSRel "John") [DRSRef "x"],
                       Rel (DRSRel "have_dinner") [DRSRef "x"]]

d1b = DRS [] [Rel (DRSRel "have_dinner") [DRSRef "x"]]

s1 = renameDisVars (drsToSDRS d1) [(0,1)]

d2 = DRS [] [Rel (DRSRel "eat_pasta") [DRSRef "x"]]
d2a = DRS [] [Rel (DRSRel "feel_good") [DRSRef "x"]]
d2b = DRS [] [Rel (DRSRel "have_cramps") [DRSRef "x"]]

s2' = addDRS s1 d2 [(1, relationFromRelName "Elaboration")]
s2 = renameDisVars s2' [(3,0)]

rf2 = rf s2

d3 = DRS [DRSRef "x"] [Rel (DRSRel "person") [DRSRef "x"],
                       Rel (DRSRel "ring_bell") [DRSRef "x"]]

s3 = addDRS s2 d3 [(1, relationFromRelName "Narration")]
--s3 = renameDisVars s3' [(4,0),(1,2),(2,0)]

rf3 = rf s3

accs3_3 = accessibleDRSs s3 3

isXBoundIn2 = sdrsBoundRef (DRSRef "x") 1 s3

freeRefs = sdrsFreeRefs s3

isS3Proper = sdrsProperDRSs s3

isS3Pure = sdrsPureDRSs s3

--areDRSRefsUniqueS3 = sdrsUniqueDRSRefs s3

--s3Unique = sdrsAlphaConvertDRS s3 3 [(DRSRef "x1",DRSRef "x2")]

d4 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [DRSRef "x"],
                                   Rel (DRSRel "glass") [DRSRef "y"],
                                   Rel (DRSRel "drop") [DRSRef "x", DRSRef "y"]]

d5 = DRS [] [Rel (DRSRel "break") [(DRSRef "y")]]


d45 = DRS [DRSRef "x", DRSRef "y"] [Rel (DRSRel "man") [DRSRef "x"],
                                   Rel (DRSRel "glass") [DRSRef "y"],
                                   Rel (DRSRel "drop") [DRSRef "x", DRSRef "y"],
                                   Rel (DRSRel "break") [DRSRef "y"]]

exampleSDRS1 = SDRS (M.fromList [
  (0, CDU (Relation (relationFromRelName "Elaboration") 1 2)),
  (1, EDU d1),
  (2, EDU d2)]) 2

exampleSDRS1u x = SDRS (M.fromList [
  (0, CDU (Relation (relationFromRelName "Elaboration") 1 2)),
  (1, EDU x),
  (2, EDU d2)]) 2

---------------------------------------------------------------------------
-- | * addSDRS
---------------------------------------------------------------------------

--[1 John has a great evening.] [2 He has fish for dinner.] 
-- [3 The next day, he either feels good]  [4 or he has stomach cramps.]

sm1'' = drsToSDRS _al07_1b
sm1' = addDRS sm1'' d1b [(0, relationFromRelName "Elaboration")]
sm1 = renameDisVars sm1' [(2,0),(0,1),(1,2)]
sm1a = drsToSDRS d2a
sm1b' = addDRS sm1a d2b [(0, relationFromRelName "Alternation")]
sm1b = renameDisVars sm1b' [(2,1),(0,2),(1,3)]
sm2' = addSDRS sm1 sm1b [(1, relationFromRelName "Result")]
sm2 = renameDisVars sm2' [(3,5),(4,3),(5,4)]

---------------------------------------------------------------------------
-- | Merges
---------------------------------------------------------------------------

--mergeRes = addSDRS sdrsmerge1 sdrsmerge2 [(0,relationFromRelName "Result")]
--merge2NarElab = addSDRS sdrsmerge1 sdrsmerge2 [(0,relationFromRelName "Narration"),(2, relationFromRelName "Elaboration")]


sdrs_because p q = SDRS (M.fromList
  [(0, CDU $ Relation (relationFromRelName "Explanation") 1 2),
   (1, EDU p),
   (2, EDU q)]) 2


