-- This is a tutorial for creating and manipulating SDRSs with PDRT-SANDBOX.

import qualified Data.Map as Map
import Data.SDRS

map1 = Map.fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]

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

sdrs1 = SDRS [1,2,3] (Map.fromList [(0, Relation "Result" 1 2),
                                    (1, Segment $ drs3),
                                    (2, Segment $ drs4)]) 0

sdrs2 = SDRS [0] (Map.fromList [(0, Segment $ DRS [] [])]) 0

sdrs_danlos_1 = SDRS [0..4] (Map.fromList [(0, Relation "Explanation" 1 2),
                                            (1, Text "Fred is in a bad mood"),
                                            (2, Relation "Continuation" 3 4),
                                            (3, Text "because he lost his keys."),
                                            (4, Text "Moreover, he failed his exam.")]) 4

sdrs_danlos_2 = SDRS [0..3] (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Explanation" 2 3)),
                                            (1, Text "Fred is in a bad mood"),
                                            (2, Text "because he didn't sleep well."),
                                            (3, Text "He had nightmares.")]) 3

sdrs_danlos_3 = SDRS [0..3] (Map.fromList [(0, And (Relation "Explanation" 1 2) (Relation "Narration" 1 3)),
                                            (1, Text "Fred went to the supermarket"),
                                            (2, Text "because his fridge is empty."),
                                            (3, Text "Then, he went to the movies.")]) 3

sdrs_danlos_4 = SDRS [0..4] (Map.fromList [(0, Relation "Commentary" 1 2),
                                            (1, Relation "Explanation" 3 4),
                                            (2, Text "Fred is upset"),
                                            (3, Text "because his wife is abroad for a week."),
                                            (4, Text "This shows that he does love her.")]) 4

sdrs_al07 = SDRS [0..7] (Map.fromList [(0, Relation "Elaboration" 1 6),
                                       -- (1, Text "John had a great evening last night."),
                                       -- (2, Text "He had a great meal."),
                                       -- (3, Text "He ate salmon."),
                                       -- (4, Text "He devoured lots of cheese."),
                                       -- (5, Text "He won a dancing competition."),
                                       (6, And (Relation "Elaboration" 2 7) (Relation "Narration" 2 5)),
                                       (7, Relation "Narration" 3 4)]) 5
