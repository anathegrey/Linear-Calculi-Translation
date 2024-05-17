module WalkerCalculus where
  import WData
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.List as List
  import WParser


-- TYPECHECKER
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
  runWT env term = typing (WParser.parseWEnv env) (WParser.parseWTerm term)



-- OPERATIONAL SEMANTICS

  -- in order to avoid creation of two sets of operational rules, one for linear data, which is deallocated when used, and one for unrestricted data, which is never deallocated, we define this function to manage the differences
  updateStore :: Store -> Q -> V -> Store
  updateStore s q y
    = case Map.lookup y s of
        Nothing -> error "undefined variable"
        _       -> if q == UN then s else Map.delete y s

  -- function to assist in the creation of new variables
  nextAddr :: Store -> Int
  nextAddr s
    = case Map.lookup x s of
        Nothing -> n
        _       -> nextAddr' s (n + 1)
    where n = 1
          x = "a" ++ (show n)
          nextAddr' :: Store -> Int -> Int
          nextAddr' s n' = let x' = "a" ++ (show n')
                           in case Map.lookup x' s of
                               Nothing -> n'
                               _       -> nextAddr' s (n' + 1)

  {-newVar :: Supply [Char] [Char]
  newVar = do
      name <- supply
      return ("a" ++ name)-}

  -- substitution
  subst :: (V, Term) -> Term -> Term
  subst (x, y) (Var z) = if x == z then y else (Var z)
  subst (x, y) (Pair q t1 t2) = Pair q (subst (x, y) t1) (subst (x, y) t2)
  subst (x, y) (Split t1 a b t2) = Split (subst (x, y) t1) a b (subst (x, y) t2)
  subst (x, y) (Lambda q z p t) = if x == z then (Lambda q z p t) else (Lambda q z p (subst (x, y) t))
  subst (x, y) (App t1 t2) = App (subst (x, y) t1) (subst (x, y) t2)

  -- evaluation function
  eval :: Store -> Term -> (Store, Term)
  eval s (Pair q t1 t2) = let addr = nextAddr s
                              x = "a" ++ show addr
                          in (Map.insert x (QValue q (RPair t1 t2)) s, Var x)
  eval s (Split (Var x) y z t) = let Just (QValue q (RPair t1 t2)) = Map.lookup x s
                                     s'  = updateStore s q x
                                     t'  = subst (y, t1) t
                                     t'' = subst (z, t2) t'
                                 in (s', t'')
  eval s (Split e y z t) = let (s', v) = eval s e
                           in eval s' (Split v y z t)
  eval s (Lambda q y p t) = let addr = nextAddr s
                                x = "a" ++ show addr
                            in (Map.insert x (QValue q (RLambda y p t)) s, Var x)
  eval s (App (Var x1) (Var x2)) = let Just (QValue q (RLambda y p t)) = Map.lookup x1 s
                                       s' = updateStore s q x1
                                       t' = subst (y, Var x2) t
                                   in (s', t')
  eval s (App (Var x1) e) = let (s', v) = eval s e
                            in eval s' (App (Var x1) v)
  eval s (App e t) = let (s', v) = eval s e
                     in eval s' (App v t)

  sepValue :: Maybe Values -> Term
  sepValue (Just (QValue q (RLambda x p t))) = Lambda q x p t
  sepValue (Just (QValue q (RPair t1 t2))) = Pair q t1 t2

  deref :: Store -> Term -> (Store, Term)
  deref s (Var y) = case Map.lookup y s of
                     Nothing -> error (y ++ " has no binding")
                     t1@(Just (QValue q t2)) -> if q == UN then (deref s (sepValue t1)) else (deref (Map.delete y s) (sepValue t1))
  deref s (Lambda q x t term) = case Map.lookup x s of
                                 Nothing -> let (s', v) = deref s term
                                           in (s', Lambda q x t v)
                                 _       -> error (x ++ " is a bound variable, therefore it cannot appear in the heap")
  deref s (App t1 t2) = let (s1, v1) = deref s t1
                            (s2, v2) = deref s1 t2
                       in (s2, App v1 v2)
  deref s (Pair q t1 t2) = let (s1, v1) = deref s t1
                               (s2, v2) = deref s1 t2
                           in (s2, Pair q v1 v2)
  deref s (Split t1 y z t2) = let (s1, v1) = deref s t1
                                  (s2, v2) = deref s1 t2
                              in (s2, Split v1 y z v2)

  runderef :: String -> String -> (Store, Term)
  runderef store term = deref (WParser.parseWStore store) (WParser.parseWTerm term)

  runWO :: String -> String -> (Store, Term)
  runWO store term = eval (WParser.parseWStore store) (WParser.parseWTerm term)
