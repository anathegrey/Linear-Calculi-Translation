module WalkerCalculus where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.List as List
  import Control.Monad.State
  import Text.PrettyPrint
  import Data
  import Parser
  import Pretty


-- TYPECHECKER
  -- predicate to express the types that can appear in a q-qualified data structure
  qType :: Q -> WType -> Bool
  qType q1 (Pre q2 t) = q1 <= q2

  -- predicate to express the qualifiers that can appear in an Environment
  qEnv :: Q -> WEnv -> Bool
  qEnv _ [] = True
  qEnv q ((_, t) : g) = if (qType q t) then qEnv q g else False

  -- remove element from Environment
  remove :: WEnv -> (V, WType) -> WEnv
  remove [] _ = []
  remove gs (x, Pre UN p) = if elem (x, Pre UN p) gs then delete (x, Pre UN p) gs else gs

  -- context difference check that linear variables do not appear and removes unrestricted variables
  contextDiff :: WEnv -> WEnv -> WEnv
  contextDiff g [] = g
  contextDiff g1 ((x, Pre LIN p) : g2) = let g3 = contextDiff g1 g2
                                         in if elem (x, Pre LIN p) g3 == False then g3 else error "linear variable still in environment after being used"
  contextDiff g1 ((x, Pre UN p) : g2) = let g3 = contextDiff g1 g2
                                        in remove g3 (x, Pre UN p)

  -- function to perform substitutions on types
  substType :: (String, PreType) -> WType -> WType
  substType (a,p) (Pre q WTBool) = Pre q WTBool
  substType (a,p) (Pre q (WTypePair t1 t2)) = Pre q (WTypePair (substType (a,p) t1) (substType (a,p) t2))
  substType (a,p) (Pre q (WArrow t1 t2)) = Pre q (WArrow (substType (a,p) t1) (substType (a,p) t2))
  substType (a,p) (Pre q (WTVar b)) = if a == b then (Pre q p) else (Pre q (WTVar b))

  -- Algorithmic type checking
  typing :: WEnv -> WTerm -> (WType, WEnv)
  typing ((y, Pre UN p) : gs) (WVar x) = let (type1, res) = typing gs (WVar x)                                                                                                                                                       -- A-UVar
                                         in if x == y then (Pre UN p, (y, Pre UN p) : gs) else (type1, [(y, Pre UN p)] ++ res)
  typing ((y, Pre LIN p) : gs) (WVar x) = let (type1, res) = typing gs (WVar x)                                                                                                                                                      -- A-LVar
                                         in if x == y then (Pre LIN p, gs) else (type1, [(y, Pre LIN p)] ++ res)
  typing g1 (WPair q t1 t2) = let (type1, g2) = typing g1 t1
                                  (type2, g3) = typing g2 t2                                                                                                                  -- A-Pair
                              in if (qType q type1) && (qType q type2) then (Pre q (WTypePair type1 type2), g3) else error "Relation not possible between qualifiers"
  typing g1 (WSplit t1 x y t2) = let (Pre q (WTypePair type1 type2), g2) = typing g1 t1                                                                         -- A-Split
                                     (type3, g3) = typing (g2 ++ [(x, type1), (y, type2)]) t2
                                 in (type3, contextDiff g3 [(x, type1), (y, type2)])
  typing g1 (WLambda q x type1 t2)                                                                                                                                            -- A-Abs
    | q == UN = if g1 == g_un then (Pre q (WArrow type1 type2), g_un) else error "unrestricted type not consistent"
    | otherwise = (Pre q (WArrow type1 type2), contextDiff g2 [(x, type1)])
    where (type2, g2) = typing (g1 ++ [(x, type1)]) t2
          g_un = contextDiff g2 [(x, type1)]
  typing g1 (WApp t1 t2) = let (Pre q (WArrow type1 type2), g2) = typing g1 t1                                                                                                 -- A-App
                               (type3, g3) = typing g2 t2
                           in if type3 == type1 then (type2, g3) else error "Type not consistent in application"
  typing _ _ = error "pattern not found in function"

  -- parsing function
  runWT :: String -> String -> (String, String)
  runWT env term = let (t, e) = typing (Parser.parseWEnv env) (Parser.parseWTerm term)
                  in (render $ prettyWType t, render $ prettyWEnv e)

  transfType :: String -> String
  transfType t = render $ prettyWType (Parser.parseWType t)



-- OPERATIONAL SEMANTICS
  
  -- in order to avoid creation of two sets of operational rules, one for linear data, which is deallocated when used, and one for unrestricted data, which is never deallocated, we define this function to manage the differences
  updateStore :: WStore -> Q -> V -> WStore
  updateStore s q y
    = case (Map.lookup y s) of
        Nothing -> error "undefined variable"
        _       -> if q == UN then s else (Map.delete y s)

  subst :: (V, WTerm) -> WTerm -> WTerm
  subst (x, y) (WVar z) = if x == z then y else (WVar z)
  subst (x, y) (WPair q t1 t2) = WPair q (subst (x, y) t1) (subst (x, y) t2)
  subst (x, y) (WSplit t1 a b t2) = WSplit (subst (x, y) t1) a b (subst (x, y) t2)
  subst (x, y) (WLambda q z p t) = if x == z then (WLambda q z p t) else (WLambda q z p (subst (x, y) t))
  subst (x, y) (WApp t1 t2) = WApp (subst (x, y) t1) (subst (x, y) t2)

  -- evaluation function
  initEval :: WStore -> WTerm -> (WStore, WTerm)
  initEval s term = evalState (eval s term) 0

  eval :: WStore -> WTerm -> State Int (WStore, WTerm)
  eval s (WPair q t1 t2) = do
    i <- get
    modify (+1)
    let x = "a" ++ (show i)
    return (Map.insert x (QValue q (RPair t1 t2)) s, WVar x)
  eval s (WSplit (WVar x) y z t) = let Just (QValue q (RPair t1 t2)) = Map.lookup x s
                                       s'  = updateStore s q x
                                       t'  = subst (y, t1) t
                                       t'' = subst (z, t2) t'
                                   in eval s' t''
  eval s (WSplit e y z t) = do
    modify (+1)
    (s', v) <- eval s e
    eval s' (WSplit v y z t)
  eval s (WLambda q y p t) = do
    i <- get
    modify (+1)
    let x = "a" ++ (show i)
    return (Map.insert x (QValue q (RLambda y p t)) s, WVar x)
  eval s (WApp (WVar x1) e) = let Just (QValue q (RLambda y p t)) = Map.lookup x1 s
                                  s' = updateStore s q x1
                                  t' = subst (y, e) t
                              in eval s' t'
  eval s (WApp e t) = do
    modify (+1)
    (s', v) <- eval s e
    eval s' (WApp v t)

  sepValue :: Maybe Values -> WTerm
  sepValue (Just (QValue q (RLambda x p t))) = WLambda q x p t
  sepValue (Just (QValue q (RPair t1 t2))) = WPair q t1 t2

  bound :: WTerm -> [V]
  bound (WVar x) = []
  bound (WLambda q x t term) = x : (bound term)
  bound (WApp t1 t2) = (bound t1) ++ (bound t2)
  bound (WPair q t1 t2) = (bound t1) ++ (bound t2)
  bound (WSplit t1 y z t2) = y : z : (bound t1) ++ (bound t2)

-- deref function for the David Walker Calculus
  initDeref :: WStore -> WTerm -> (WStore, WTerm)
  initDeref g term = let l = (bound term)
                    in deref g l term

  deref :: WStore -> [V] -> WTerm -> (WStore, WTerm)
  deref s l (WVar y) = case elem y l of
                         True  -> (s, WVar y)
                         False -> case Map.lookup y s of
                                   Nothing -> error (y ++ " has no binding")
                                   t1@(Just (QValue q t2)) -> let t1' = sepValue t1
                                                            in if q == UN then (deref s (l ++ (bound t1')) t1') else (deref (Map.delete y s) (l ++ (bound t1')) t1')   
  deref s l (WLambda q x t term) = case Map.lookup x s of
                                     Nothing -> let (s', v) = deref s l term
                                               in (s', WLambda q x t v)
                                     _       -> error (x ++ " is a bound variable, therefore it cannot appear in the heap")
  deref s l (WApp t1 t2) = let (s1, v1) = deref s l t1
                               (s2, v2) = deref s1 l t2
                          in (s2, WApp v1 v2)
  deref s l (WPair q t1 t2) = let (s1, v1) = deref s l t1
                                  (s2, v2) = deref s1 l t2
                              in (s2, WPair q v1 v2)
  deref s l (WSplit t1 y z t2) = let (s1, v1) = deref s l t1
                                     (s2, v2) = deref s1 l t2
                                 in (s2, WSplit v1 y z v2)


-- RESULTS
  runderefW :: String -> String -> String
  runderefW store term = render $ prettyWTerm (snd $ initDeref (Parser.parseWStore store) (Parser.parseWTerm term))

  runWO :: String -> String -> (String, String)
  runWO store term = let (s, t) = initEval (Parser.parseWStore store) (Parser.parseWTerm term)
                    in (render $ prettyWStore s, render $ prettyWTerm t) 
  
