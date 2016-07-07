
---------------------------------------------------------------------------
-- | Shows a discourse graph
---------------------------------------------------------------------------
--instance {-# OVERLAPPING #-} Show DGraph where
--  show dg = showDGraph dg

--showDGraph :: DGraph -> String
--showDGraph graph = "[" ++ graphToString (M.assocs graph) ++ "]"
--  where graphToString :: [(DisVar,[(DisVar, SDRSRelation)])] -> String
--        graphToString []                  = "\n"
--        graphToString ((node,edges):rest) = "\n" ++
--                                            "(" ++
--                                            show node ++
--                                            ", " ++
--                                            show edges ++
--                                            ")" ++
--                                            graphToString rest

---------------------------------------------------------------------------
-- | DGraph
---------------------------------------------------------------------------
--type DGraph = M.Map DisVar [(DisVar, SDRSRelation)]

---------------------------------------------------------------------------
-- | Given an SDRS, build a labeled graph structure, consisting of a tuple
-- of the graph itself and the last node
---------------------------------------------------------------------------
--discourseGraph :: SDRS -> DGraph
--discourseGraph (SDRS m _) = M.foldlWithKey build M.empty m
--  where build :: (M.Map DisVar [(DisVar, SDRSRelation)]) -> DisVar -> SDRSFormula -> M.Map DisVar [(DisVar, SDRSRelation)]
--        build acc dv0 (CDU (Relation rel dv1 dv2)) = M.insertWith (++) dv1 [(dv2,rel)] (M.insertWith (union) dv0 [(dv1,Outscopes),(dv2,Outscopes)] acc)
--        build acc dv0 (CDU (And sf1 sf2))          = build (build acc dv0 (CDU sf1)) dv0 (CDU sf2)
--        build acc dv0 (CDU (Not sf1))              = build acc dv0 (CDU sf1)
--        build acc _ _                              = acc

---------------------------------------------------------------------------
-- | Given the 'SDRS' @s@ and the 'DisVar' @dv1@, lists all accessible 'DisVar's
-- using the discourse graph of @s@. The output list's elements are ordered
-- with the first element being the most local and the last the most global.
---------------------------------------------------------------------------
