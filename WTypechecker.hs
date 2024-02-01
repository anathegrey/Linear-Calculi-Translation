module WTypechecker where
  import WData
  import Data.List
  import Parser

  -- predicate to express the types that can appear in a q-qualified data structure
  qType :: Q -> Type -> Bool
  qType q1 (Pre q2 t) = q1 <= q2

  -- predicate to express the qualifiers that can appear in an Environment
  qEnv :: Q -> Env -> Bool
  qEnv _ [] = True
  qEnv q ((_, t) : g) = if (qType q t) then qEnv q g else False

  -- remove element from Environment
  remove :: Env -> (V, Type) -> Env
  remove [] _ = []
  remove gs (x, Pre UN p) = if elem (x, Pre UN p) gs then delete (x, Pre UN p) gs else gs

  -- context difference check that linear variables do not appear and removes unrestricted variables
  contextDiff :: Env -> Env -> Env
  contextDiff g [] = g
  contextDiff g1 ((x, Pre LIN p) : g2) = let g3 = contextDiff g1 g2
                                         in if elem (x, Pre LIN p) g3 == False then g3 else error "linear variable still in environment after being used"
  contextDiff g1 ((x, Pre UN p) : g2) = let g3 = contextDiff g1 g2
                                        in remove g3 (x, Pre UN p)

  -- function to perform substitutions on types
  substType :: (String, PreType) -> Type -> Type
  substType (a,p) (Pre q TBool) = Pre q TBool
  substType (a,p) (Pre q (TypePair t1 t2)) = Pre q (TypePair (substType (a,p) t1) (substType (a,p) t2))
  substType (a,p) (Pre q (Arrow t1 t2)) = Pre q (Arrow (substType (a,p) t1) (substType (a,p) t2))
  substType (a,p) (Pre q (TVar b)) = if a == b then (Pre q p) else (Pre q (TVar b))

  -- Algorithmic type checking
  typing :: Env -> Term -> (Type, Env)
  typing ((y, Pre UN p) : gs) (Var x) = let (type1, res) = typing gs (Var x)                                                                                                                                                       -- A-UVar
                                        in if x == y then (Pre UN p, (y, Pre UN p) : gs) else (type1, [(y, Pre UN p)] ++ res)
  typing ((y, Pre LIN p) : gs) (Var x) = let (type1, res) = typing gs (Var x)                                                                                                                                                      -- A-LVar
                                         in if x == y then (Pre LIN p, gs) else (type1, [(y, Pre LIN p)] ++ res)
  typing g1 (Pair q t1 t2) = let (type1, g2) = typing g1 t1
                                 (type2, g3) = typing g2 t2                                                                                                                  -- A-Pair
                             in if (qType q type1) && (qType q type2) then (Pre q (TypePair type1 type2), g3) else error "Relation not possible between qualifiers"
  typing g1 (Split t1 x y t2) = let (Pre q (TypePair type1 type2), g2) = typing g1 t1                                                                        -- A-Split
                                    (type3, g3) = typing (g2 ++ [(x, type1), (y, type2)]) t2
                                in (type3, contextDiff g3 [(x, type1), (y, type2)])
  typing g1 (Lambda q x type1 t2)                                                                                                                                            -- A-Abs
    | q == UN = if g1 == g_un then (Pre q (Arrow type1 type2), g_un) else error "unrestricted type not consistent"
    | otherwise = (Pre q (Arrow type1 type2), contextDiff g2 [(x, type1)])
    where (type2, g2) = typing (g1 ++ [(x, type1)]) t2
          g_un = contextDiff g2 [(x, type1)]
  typing g1 (App t1 t2) = let (Pre q (Arrow type1 type2), g2) = typing g1 t1                                                                                                 -- A-App
                              (type3, g3) = typing g2 t2
                          in if type3 == type1 then (type2, g3) else error "Type not consistent in application"
  typing _ _ = error "pattern not found in function"

  -- parsing function
  runWT :: String -> String -> (Type, Env)
  runWT env term = typing (Parser.parseWEnv env) (Parser.parseWTerm term)
