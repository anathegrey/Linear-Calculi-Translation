module LinearHaskell where
  import Data
  import Data.Map (Map)
  import Data.List as List
  import Data.Maybe as Maybe
  import qualified Data.Map as Map
  import Parser
  import Control.Monad.State

-- TYPECHECKER

  mult :: Pi -> Pi -> Pi
  mult One One = One
  mult _ _ = Omega

  substList :: (V, LTerm) -> [(V, LType, LTerm)] -> [(V, LType, LTerm)]
  substList (x, term) [] = []
  substList (x, term) ((y, t, term') : xs) = (y, t, subst (x, term) term') : (substList (x, term) xs)

  subst :: (V, LTerm) -> LTerm -> LTerm
  subst (x, term) (LVar y) = if x == y then term else (LVar y)
  subst (x, term) (LLambda p y t term') = if x == y then (LLambda p y t term') else (LLambda p y t (subst (x, term) term'))
  subst (x, term) (LPair term1 term2 p) = LPair (subst (x, term) term1) (subst (x, term) term2) p
  subst (x, term) (LApp term1 term2) = LApp (subst (x, term) term1) (subst (x, term) term2)
  subst (x, term) (LSplit term1 y z term2) = LSplit (subst (x, term) term1) y z (subst (x, term) term2)
  subst (x, term) (Let p xs term') = Let p (substList (x, term) xs) (subst (x, term) term')

  typing :: LEnv -> LTerm -> (LType, LEnv)
  typing ((y, p, t) : env) (LVar x) = let (t', env') = typing env (LVar x)
                                      in if x == y then (if p == One then (t, env) else (t, (y, p, t) : env)) else (t', (y, p, t) : env')
  typing g (LLambda p x t term) = let (t', env') = typing (g ++ [(x, p, t)]) term
                                 in (LArrow t p t', env')
  typing g (LApp term1 term2) = let (LArrow t1 p t2, env') = typing g term1
                                    (t, env'')            = typing env' term2
                                in if t1 == t then (t2, env'') else error "Type not consistent in application"
  typing g (LPair term1 term2 p) = let (t', g')   = typing g term1
                                       (t'', g'') = typing g' term2
                                  in (LTypePair t' p t'', g'')
  typing g (LSplit term1 x y term2) = let (LTypePair t1 p t2, g') = typing g term1
                                          (t, g'')                = typing (g' ++ [(x, p, t1), (y, p, t2)]) term2
                                      in (t, g'')
  typing g (Let p [] term) = typing g term
  typing g (Let p ((x, a, t) : l) term) = let (a1, g1) = typing g t
                                         in if a == a1 then (typing (g1 ++ [(x, p, a)]) (Let p l term)) else error "Types are inconsistent in Let"

  -- parsing function
  runLT :: String -> String -> (LType, LEnv)
  runLT env term = typing (Parser.parseLEnv env) (Parser.parseLTerm term)

  transfType :: String -> LType
  transfType t = (Parser.parseLType t)


-- OPERATIONAL SEMANTICS

  substTerm :: (V, LTerm) -> LTerm -> LTerm
  substTerm (y, t1) (LVar x) = if x == y then t1 else (LVar x)
  substTerm (y, t1) (LLambda p x t t2) = if x == y then (LLambda p x t t2) else (LLambda p x t (substTerm (y, t1) t2))
  substTerm (y, t1) (LPair t2 t3 p) = LPair (substTerm (y, t1) t2) (substTerm (y, t1) t3) p
  substTerm (y, t1) (LApp t2 t3) = LApp (substTerm (y, t1) t2) (substTerm (y, t1) t3)
  substTerm (y, t1) (LSplit t2 a b t3) = LSplit (substTerm (y, t1) t2) a b (substTerm (y, t1) t3)
  substTerm (y, t1) (Let p l term) = Let p (substList (y, t1) l) (substTerm (y, t1) term)
    where
      substList (_, _) [] = []
      substList (y, t1) ((x, t, term) : l) = (x, t, substTerm (y, t1) term) : (substList (y, t1) l)

  transform :: LStore -> LEnv
  transform g = transform' (Map.toList g)
    where
      transform' [] = []
      transform' ((x, (p, t, term)) : l) = (x, p, t) : (transform' l)

  initlbury :: LStore -> LTerm -> LTerm
  initlbury g term = let (t, env) = typing (transform g) term
                    in lbury env (execState getCount 0) term

  lbury :: LEnv -> Int -> LTerm -> LTerm
  lbury g i (LVar x) = LVar x
  lbury g i (LLambda p x t term) = LLambda p x t (lbury g (execState (modify (+1)) i) term)
  lbury g i (LApp term (LVar y)) = LApp (lbury g (execState (modify (+1)) i) term) (LVar y)
  lbury g i (LApp term1 term2) = let (LArrow t1 p t2, env) = typing g term1
                                     j = execState (modify (+1)) i
                                     x1 = "a" ++ (show i)
                                 in Let p [(x1, t1, lbury g j term2)] (LApp (lbury g (execState (modify (+1)) j) term1) (LVar x1))
  lbury g i (LPair term1 term2 p) = let (LTypePair t1 p' t2, env) = typing g (LPair term1 term2 p)
                                        x1 = "a" ++ (show i)
                                        j = execState (modify (+1)) i
                                        x2 = "a" ++ (show j)
                                        z = execState (modify (+1)) j
                                    in Let p [(x1, t1, lbury g z term1), (x2, t2, lbury g (execState (modify (+1)) z) term2)] (LPair (LVar x1) (LVar x2) p)
  lbury g i (LSplit term1 x y term2) = LSplit (lbury g (execState getCount i) term1) x y (lbury g (execState getCount i) term2)
  lbury g i (Let p l term) = Let p (lbury' g j l) (lbury g (execState (modify (+1)) j) term)
    where
      j = execState (modify (+1)) i
      lbury' _ _ [] = []
      lbury' g i ((x1, a1, t1) : l) = let j = execState (modify (+1)) i
                                     in (x1, a1, lbury g j t1) : (lbury' g (execState (modify (+1)) j) l)

  eval, eval' :: LStore -> LTerm -> (LStore, LTerm)
  eval g t = let t' = initlbury g t
             in eval' g t'
  eval' g (LLambda p x t term) = (g, LLambda p x t term)
  eval' g (LPair (LVar x) (LVar y) p) = (g, LPair (LVar x) (LVar y) p)
  eval' g (LSplit t1 x y t2) = let (g', LPair (LVar x1) (LVar y1) p) = eval' g t1
                               in eval' g' (substTerm (y, LVar y1) (substTerm (x, LVar x1) t2))
  eval' g (LApp term (LVar x)) = let (g', LLambda p y t term') = eval' g term
                                in eval' g' (substTerm (y, LVar x) term')
  eval' g (LVar x) = let Just (p, t, term) = Map.lookup x g
                         (g', term') = eval' (Map.delete x g) term
                     in if p == Omega then (Map.insert x (p, t, term') g', term') else (g', term')
  eval' g (Let p [] t) = eval' g t
  eval' g (Let p ((x1, a1, e1) : l) t) = eval' (Map.insert x1 (p, a1, e1) g) (Let p l t)

  bound :: LTerm -> [V]
  bound (LVar x) = []
  bound (LLambda pi x t term) = x : (bound term)
  bound (LApp t1 t2) = (bound t1) ++ (bound t2)
  bound (LPair t1 t2 pi) = (bound t1) ++ (bound t2)
  bound (LSplit t1 y z t2) = y : z : (bound t1) ++ (bound t2)
  bound (Let pi [] term) = bound term
  bound (Let pi ((x, a, e) : xs) term) = bound e ++ bound (Let pi xs term)

  initDeref :: LStore -> LTerm -> (LStore, LTerm)
  initDeref g term = let l = bound term
                    in deref g l term

  deref :: LStore -> [V] -> LTerm -> (LStore, LTerm)
  deref g l (LVar y) = case elem y l of
                        True  -> (g, LVar y)
                        False -> case Map.lookup y g of
                                  Nothing -> error (y ++ " has no binding")
                                  Just (pi, t, term) -> if pi == Omega then (deref g (l ++ (bound term)) term) else (deref (Map.delete y g) (l ++ (bound term)) term)
  deref g l (LLambda pi x t term) = case Map.lookup x g of
                                      Nothing -> let (g', v) = deref g l term
                                                in (g', LLambda pi x t v)
                                      _       -> error (x ++ " is a bound variable, therefore it cannot appear in the heap")
  deref g l (LApp t1 t2) = let (g1, v1) = deref g l t1
                               (g2, v2) = deref g1 l t2
                           in (g2, LApp v1 v2)
  deref g l (LPair t1 t2 pi) = let (g1, v1) = deref g l t1
                                   (g2, v2) = deref g1 l t2
                               in (g2, LPair v1 v2 pi)
  deref g l (LSplit t1 y z t2) = let (g1, v1) = deref g l t1
                                     (g2, v2) = deref g1 l t2
                                 in (g2, LSplit v1 y z v2)
  deref g l (Let pi [] term) = deref g l term
  deref g l (Let pi ((x, t, term') : xs) term) = deref (Map.insert x (pi, t, term') g) l (Let pi xs term)

  runderefL :: String -> String -> LTerm
  runderefL store term = snd $ initDeref (Parser.parseLStore store) (Parser.parseLTerm term)

  runLO :: String -> String -> (LStore, LTerm)
  runLO store term = eval (Parser.parseLStore store) (Parser.parseLTerm term)
