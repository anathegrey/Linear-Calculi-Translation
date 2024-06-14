module LinearHaskell where
  import LData
  import Data.Map (Map)
  import Data.List as List
  import Data.Maybe as Maybe
  import qualified Data.Map as Map
  import LParser
  import Control.Monad.State

-- TYPECHECKER

  mult :: Pi -> Pi -> Pi
  mult One One = One
  mult _ _ = Omega

  substList :: (V, Term) -> [(V, Type, Term)] -> [(V, Type, Term)]
  substList (x, term) [] = []
  substList (x, term) ((y, t, term') : xs) = (y, t, subst (x, term) term') : (substList (x, term) xs)

  subst :: (V, Term) -> Term -> Term
  subst (x, term) (Var y) = if x == y then term else (Var y)
  subst (x, term) (Lambda p y t term') = if x == y then (Lambda p y t term') else (Lambda p y t (subst (x, term) term'))
  subst (x, term) (Pair term1 term2 p) = Pair (subst (x, term) term1) (subst (x, term) term2) p
  subst (x, term) (App term1 term2) = App (subst (x, term) term1) (subst (x, term) term2)
  subst (x, term) (Split term1 y z term2) = Split (subst (x, term) term1) y z (subst (x, term) term2)
  subst (x, term) (Let p xs term') = Let p (substList (x, term) xs) (subst (x, term) term')

  typing :: Env -> Term -> (Type, Env)
  typing ((y, p, t) : env) (Var x) = let (t', env') = typing env (Var x)
                                     in if x == y then (if p == One then (t, env) else (t, (y, p, t) : env)) else (t', (y, p, t) : env')
  typing g (Lambda p x t term) = let (t', env') = typing (g ++ [(x, p, t)]) term
                                 in (Arrow t p t', env')
  typing g (App term1 term2) = let (Arrow t1 p t2, env') = typing g term1
                                   (t, env'')            = typing env' term2
                               in if t1 == t then (t2, env'') else error "Type not consistent in application"
  typing g (Pair term1 term2 p) = let (t', g')   = typing g term1
                                      (t'', g'') = typing g' term2
                                 in (TypePair t' p t'', g'')
  typing g (Split term1 x y term2) = let (TypePair t1 p t2, g') = typing g term1
                                         (t, g'')               = typing (g' ++ [(x, p, t1), (y, p, t2)]) term2
                                     in (t, g'')
  typing g (Let p [] term) = typing g term
  typing g (Let p ((x, a, t) : l) term) = let (a1, g1) = typing g t
                                         in if a == a1 then (typing (g1 ++ [(x, p, a)]) (Let p l term)) else error "Types are inconsistent in Let"

  -- parsing function
  runLT :: String -> String -> (Type, Env)
  runLT env term = typing (LParser.parseLEnv env) (LParser.parseLTerm term)

  transfType :: String -> Type
  transfType t = (LParser.parseLType t)


-- OPERATIONAL SEMANTICS

  -- function to assist in the creation of new variables
  type Counter = State Int

  getCount :: Counter Int
  getCount = get

  substTerm :: (V, Term) -> Term -> Term
  substTerm (y, t1) (Var x) = if x == y then t1 else (Var x)
  substTerm (y, t1) (Lambda p x t t2) = if x == y then (Lambda p x t t2) else (Lambda p x t (substTerm (y, t1) t2))
  substTerm (y, t1) (Pair t2 t3 p) = Pair (substTerm (y, t1) t2) (substTerm (y, t1) t3) p
  substTerm (y, t1) (App t2 t3) = App (substTerm (y, t1) t2) (substTerm (y, t1) t3)
  substTerm (y, t1) (Split t2 a b t3) = Split (substTerm (y, t1) t2) a b (substTerm (y, t1) t3)
  substTerm (y, t1) (Let p l term) = Let p (substList (y, t1) l) (substTerm (y, t1) term)
    where
      substList (_, _) [] = []
      substList (y, t1) ((x, t, term) : l) = (x, t, substTerm (y, t1) term) : (substList (y, t1) l)

  transform :: Store -> Env
  transform g = transform' (Map.toList g)
    where
      transform' [] = []
      transform' ((x, (p, t, term)) : l) = (x, p, t) : (transform' l)

  initlbury :: Store -> Term -> Term
  initlbury g term = let (t, env) = typing (transform g) term
                    in lbury env (execState getCount 0) term

  lbury :: Env -> Int -> Term -> Term
  lbury g i (Var x) = Var x
  lbury g i (Lambda p x t term) = Lambda p x t (lbury g (execState (modify (+1)) i) term)
  lbury g i (App term (Var y)) = App (lbury g (execState (modify (+1)) i) term) (Var y)
  lbury g i (App term1 term2) = let (Arrow t1 p t2, env) = typing g term1
                                    j = execState (modify (+1)) i
                                    x1 = "a" ++ (show i)
                                in Let p [(x1, t1, lbury g j term2)] (App (lbury g (execState (modify (+1)) j) term1) (Var x1))
  lbury g i (Pair term1 term2 p) = let (TypePair t1 p' t2, env) = typing g (Pair term1 term2 p)
                                       x1 = "a" ++ (show i)
                                       j = execState (modify (+1)) i
                                       x2 = "a" ++ (show j)
                                       z = execState (modify (+1)) j
                                   in Let p [(x1, t1, lbury g z term1), (x2, t2, lbury g (execState (modify (+1)) z) term2)] (Pair (Var x1) (Var x2) p)
  lbury g i (Split term1 x y term2) = Split (lbury g (execState getCount i) term1) x y (lbury g (execState getCount i) term2)
  lbury g i (Let p l term) = Let p (lbury' g j l) (lbury g (execState (modify (+1)) j) term)
    where
      j = execState (modify (+1)) i
      lbury' _ _ [] = []
      lbury' g i ((x1, a1, t1) : l) = let j = execState (modify (+1)) i
                                     in (x1, a1, lbury g j t1) : (lbury' g (execState (modify (+1)) j) l)

  eval, eval' :: Store -> Term -> (Store, Term)
  eval g t = let t' = initlbury g t
             in eval' g t'
  eval' g (Lambda p x t term) = (g, Lambda p x t term)
  eval' g (Pair (Var x) (Var y) p) = (g, Pair (Var x) (Var y) p)
  eval' g (Split t1 x y t2) = let (g', Pair (Var x1) (Var y1) p) = eval' g t1
                              in eval' g' (substTerm (y, Var y1) (substTerm (x, Var x1) t2))
  eval' g (App term (Var x)) = let (g', Lambda p y t term') = eval' g term
                               in eval' g' (substTerm (y, Var x) term')
  eval' g (Var x) = let Just (p, t, term) = Map.lookup x g
                        (g', term') = eval' (Map.delete x g) term
                    in if p == Omega then (Map.insert x (p, t, term') g', term') else (g', term')
  eval' g (Let p [] t) = eval' g t
  eval' g (Let p ((x1, a1, e1) : l) t) = eval' (Map.insert x1 (p, a1, e1) g) (Let p l t)

  bound :: Term -> [V]
  bound (Var x) = []
  bound (Lambda pi x t term) = x : (bound term)
  bound (App t1 t2) = (bound t1) ++ (bound t2)
  bound (Pair t1 t2 pi) = (bound t1) ++ (bound t2)
  bound (Split t1 y z t2) = y : z : (bound t1) ++ (bound t2)
  bound (Let pi [] term) = bound term
  bound (Let pi ((x, a, e) : xs) term) = bound e ++ bound (Let pi xs term)

  boundStore :: Store -> [V]
  boundStore g = boundStore' (Map.toList g)
    where
      boundStore' [] = []
      boundStore' ((x, (pi, t, term)) : xs) = bound term ++ (boundStore' xs)

  initDeref :: Store -> Term -> (Store, Term)
  initDeref g term = let l = (bound term) ++ (boundStore g)
                    in deref g l term

  deref :: Store -> [V] -> Term -> (Store, Term)
  deref g l (Var y) = case elem y l of
                       True  -> (g, Var y)
                       False -> case Map.lookup y g of
                                 Nothing -> error (y ++ " has no binding")
                                 Just (pi, t, term) -> if pi == Omega then (deref g l term) else (deref (Map.delete y g) l term)
  deref g l (Lambda pi x t term) = case Map.lookup x g of
                                     Nothing -> let (g', v) = deref g l term
                                               in (g', Lambda pi x t v)
                                     _       -> error (x ++ " is a bound variable, therefore it cannot appear in the heap")
  deref g l (App t1 t2) = let (g1, v1) = deref g l t1
                              (g2, v2) = deref g1 l t2
                          in (g2, App v1 v2)
  deref g l (Pair t1 t2 pi) = let (g1, v1) = deref g l t1
                                  (g2, v2) = deref g1 l t2
                              in (g2, Pair v1 v2 pi)
  deref g l (Split t1 y z t2) = let (g1, v1) = deref g l t1
                                    (g2, v2) = deref g1 l t2
                                in (g2, Split v1 y z v2)
  deref g l (Let pi [] term) = deref g l term
  deref g l (Let pi ((x, t, term') : xs) term) = deref (Map.insert x (pi, t, term') g) l (Let pi xs term)

  runderefL :: String -> String -> Term
  runderefL store term = snd $ initDeref (LParser.parseLStore store) (LParser.parseLTerm term)

  runLO :: String -> String -> (Store, Term)
  runLO store term = eval (LParser.parseLStore store) (LParser.parseLTerm term)
