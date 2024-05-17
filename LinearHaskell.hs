module LinearHaskell where
  import LData
  import Data.Map (Map)
  import Data.List as List
  import Data.Maybe as Maybe
  import qualified Data.Map as Map
  import LParser

-- TYPECHECKER

  mult :: Pi -> Pi -> Pi
  mult One One = One
  mult _ _ = Omega

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

 {- typingL :: LEnv -> LTerm -> (LType, LEnv)
  typingL ((y, p, t) : env) (LVar x) = let (t', env') = typingL env (LVar x)
                                      in if x == y then (if p == One then (t, env) else (t, (y, p, t) : env)) else (t', (y, p, t) : env')
  typingL g (LLambda p x t term) = let (t', env') = typingL (g ++ [(x, p, t)]) term
                                  in (LArrow t p t', env') -- ver se x existe em gama
  typingL g (LApp term1 term2) = let (LArrow t1 p' t2, env') = typingL g term1
                                     (t, env'')              = typingL env' term2
                                in if t1 == t then (t2, env'') else error "Type not consistent in application"
  typingL g (LPair term1 term2 p) = let (t', g') = typingL g term1
                                        (t'', g'') = typingL g' term2
                                   in (LTypePair t' p t'', g'')
  typingL g (LSplit term1 x y term2) = let (LTypePair t1 p t2, g') = typingL g term1
                                           (t, g'') = typingL (g' ++ [(x, p, t1), (y, p, t2)]) term2
                                      in (t, g'')
  typingL g (Let p [] u) = typingL g u
  typingL g (Let p ((x, a, t) : l) u) = let (a1, g1) = typingL (g ++ [(x, p, a)]) t
                                       in if a == a1 then (typingL g1 (Let p l u)) else error "types are inconsistent in let"-}

  -- parsing function
  runLT :: String -> String -> (Type, Env)
  runLT env term = typing (LParser.parseLEnv env) (LParser.parseLTerm term)




-- OPERATIONAL SEMANTICS

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

  nextLet :: Term -> Int
  nextLet (Let p l term) = length l + (nextLet term)
  nextLet _ = 0

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
                    in lbury env term

  lbury :: Env -> Term -> Term
  lbury g (Var x) = Var x
  lbury g (Lambda p x t term) = Lambda p x t (lbury g term)
  lbury g (App term (Var y)) = App (lbury g term) (Var y)
  lbury g (App term1 term2) = let addr = nextLet term1 + nextLet term2
                                  x1   = "a" ++ (show addr)
                                  (Arrow t1 p t2, env) = typing g term1
                              in Let p [(x1, t1, lbury g term2)] (App (lbury g term1) (Var x1))
  lbury g (Pair term1 term2 p) = let (TypePair t1 p' t2, env) = typing g (Pair term1 term2 p)
                                     addr1 = nextLet term1 + nextLet term2
                                     x1 = "a" ++ (show addr1)
                                     x2 = "a" ++ (show (addr1 + 1))
                                 in Let p [(x1, t1, lbury g term1), (x2, t2, lbury g term2)] (Pair (Var x1) (Var x2) p)
  lbury g (Split term1 x y term2) = Split (lbury g term1) x y (lbury g term2)
  lbury g (Let p l term) = Let p (lbury' g l) (lbury g term)
    where
      lbury' _ [] = []
      lbury' g ((x1, a1, t1) : l) = (x1, a1, lbury g t1) : (lbury' g l)

 {- lbury :: LStore -> LTerm -> LTerm
  lbury g (LVar x) = LVar x
  lbury g (LLambda p x t term) = LLambda p x t (lbury g term)
  lbury g (LApp term (LVar y)) = LApp (lbury g term) (LVar y)
  lbury g (LApp term1 term2) = let addr = nextAddr g
                                   x1 = "a" ++ (show addr)
                              in Let Omega [(x1, LTVar "a", lbury g term2)] (LApp (lbury g term1) (LVar x1))
  lbury g (LPair term1 term2 p) = let addr1 = nextAddr g
                                      x1 = "a" ++ (show addr1)
                                      addr2 = nextAddr (Map.insert x1 (p, LTVar "a", term1) g)
                                      x2 = "a" ++ (show addr2)
                                in Let Omega [(x1, LTVar "a", lbury g term1), (x2, LTVar "a", lbury g term2)] (LPair (LVar x1) (LVar x2) p)
  lbury g (LSplit term1 x y term2) = LSplit (lbury g term1) x y (lbury g term2)
  lbury g (Let p l term) = Let p (lbury' g l) (lbury g term)
    where
      lbury' _ [] = []
      lbury' g ((x1, a1, t1) : l) = (x1, a1, lbury g t1) : (lbury' g l)-}

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
  bound (Lambda p x t term) = x : (bound term)
  bound (App t1 t2) = (bound t1) ++ (bound t2)
  bound (Pair t1 t2 p) = (bound t1) ++ (bound t2)
  bound (Split t1 y z t2) = (bound t1) ++ (bound t2)
  bound (Let p l t) = bound t

{-
  derefL :: LStore -> LTerm -> LTerm
  derefL g t = derefL' g (boundL t) t

  derefL' :: LStore -> [V] -> LTerm -> LTerm
  derefL' g bound (LVar y)
    | a == Nothing = if (elem y bound) then (LVar y) else error (y ++ " has no binding")
    | otherwise = derefL' (Map.delete y g) bound (Maybe.fromJust (Map.lookup y g))
    where a =
  derefL' g bound (LLambda p x t term) = LLambda p x t (derefL' g (x : (bound ++ (boundL term))) term)
  derefL' g bound (LApp t1 t2) = LApp (derefL' g bound t1) (derefL' g bound t2)
  derefL' g bound (LPair t1 t2 p) = LPair (derefL' g bound t1) (derefL' g bound t2) p
  derefL' g bound (LSplit t1 y z t2) = LSplit (derefL' g bound t1) y z (derefL' g bound t2)
-}

--  runderefL :: String -> String -> LTerm
--  runderefL store term = derefL (Parser.parseLStore store) (Parser.parseLTerm term)

  runLO :: String -> String -> (Store, Term)
  runLO store term = eval (LParser.parseLStore store) (LParser.parseLTerm term)
