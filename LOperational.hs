module LOperational where
  import LData
  import Data.Map (Map)
  import Data.List as List
  import Data.Maybe as Maybe
  import qualified Data.Map as Map
  import Parser

  -- function to assist in the creation of new variables
  nextAddr :: LStore -> Int
  nextAddr s
    = case Map.lookup x s of
        Nothing -> n
        _       -> nextAddr' s (n + 1)
    where n = 1
          x = "a" ++ (show n)
          nextAddr' :: LStore -> Int -> Int
          nextAddr' s n' = let x' = "a" ++ (show n')
                           in case Map.lookup x' s of
                               Nothing -> n'
                               _       -> nextAddr' s (n' + 1)

  substCtx :: (V, LTerm) -> LStore -> LStore
  substCtx (y, t1) g = Map.fromList (substCtx' (y, t1) (Map.toList g))
    where substCtx' (y, t1) [] = []
          substCtx' (y, t1) ((a, c) : l) = (a, substTerm (y, t1) c) : (substCtx' (y, t1) l)

  substTerm :: (V, LTerm) -> LTerm -> LTerm
  substTerm (y, t1) (LVar x) = if x == y then t1 else (LVar x)
  substTerm (y, t1) (LLambda p x t t2) = if x == y then (LLambda p x t t2) else (LLambda p x t (substTerm (y, t1) t2))
  substTerm (y, t1) (LPair t2 t3 p) = LPair (substTerm (y, t1) t2) (substTerm (y, t1) t3) p
  substTerm (y, t1) (LApp t2 t3) = LApp (substTerm (y, t1) t2) (substTerm (y, t1) t3)
  substTerm (y, t1) (LSplit t2 a b t3) = LSplit (substTerm (y, t1) t2) a b (substTerm (y, t1) t3)

  ctxAdd :: LStore -> LStore -> LStore
  ctxAdd g1 g2
    | g1 == Map.empty = g2
    | g2 == Map.empty = g1
    | otherwise = ctxAdd' (Map.toList g1) g2
    where ctxAdd' :: [(V, LTerm)] -> LStore -> LStore
          ctxAdd' ((x, t1) : g1) g2
            | t2 == Just t1 = ctxAdd (Map.fromList g1) g2
            | t2 == Nothing = ctxAdd (Map.fromList g1) (Map.insert x t1 g2)
            | otherwise = error "Same variables have different values"
            where t2 = Map.lookup x g2

  lbury :: LStore -> LTerm -> (LStore, LTerm)
  lbury g (LVar x) = (g, LVar x)
  lbury g (LLambda p x t term) = let (g', term') = lbury g term
                                in (g', LLambda p x t term')
  lbury g (LApp term (LVar y)) = let (g', term') = lbury g term
                                 in (g', LApp term' (LVar y))
  lbury g (LApp term1 term2) = let (g1, term1') = lbury g term1
                                   (g2, term2') = lbury g term2
                                   g' = ctxAdd g1 g2
                                   addr1 = nextAddr g'
                                   x1 = "a" ++ (show addr1)
                               in (Map.insert x1 term2' g', LApp term1' (LVar x1))
  lbury g (LPair term1 term2 p) = let (g1, term1') = lbury g term1
                                      (g2, term2') = lbury g term2
                                      g' = ctxAdd g1 g2
                                      addr1 = nextAddr g'
                                      x1 = "a" ++ (show addr1)
                                      g'' = Map.insert x1 term1' g'
                                      addr2 = nextAddr g''
                                      x2 = "a" ++ (show addr2)
                                   in (Map.insert x2 term2' g'', LPair (LVar x1) (LVar x2) p)
  lbury g (LSplit t1 x y t2) = let (g1, t1') = lbury g t1
                                   (g2, t2') = lbury g t2
                               in (ctxAdd g1 g2, LSplit t1' x y t2')

  evalL, evalL' :: LStore -> LTerm -> (LStore, LTerm)
  evalL g t = let (g', t') = lbury g t
             in evalL' g' t'
  evalL' g (LLambda p x t term) = (g, LLambda p x t term)
  evalL' g (LPair (LVar x) (LVar y) p) = (g, LPair (LVar x) (LVar y) p)
  evalL' g (LSplit t1 x y t2) = let (g', LPair (LVar x1) (LVar y1) p) = evalL' g t1
                                in evalL' (substCtx (y, LVar y1) (substCtx (x, LVar x1) g')) (substTerm (y, LVar y1) (substTerm (x, LVar x1) t2))
  evalL' g (LApp term (LVar x)) = let (g', LLambda p y t term') = evalL' g term
                                 in evalL' (substCtx (y, LVar x) g') (substTerm (y, LVar x) term')
  evalL' g (LVar x) = let Just t = Map.lookup x g
                          g' = Map.delete x g
                          (g'', LVar z) = evalL' g' t
                      in (Map.insert x (LVar z) g'', LVar z)
                      
  boundL :: LTerm -> [V]
  boundL (LVar x) = []
  boundL (LLambda p x t term) = x : (boundL term)
  boundL (LApp t1 t2) = (boundL t1) ++ (boundL t2)
  boundL (LPair t1 t2 p) = (boundL t1) ++ (boundL t2)
  boundL (LSplit t1 y z t2) = (boundL t1) ++ (boundL t2)

  derefL :: LStore -> LTerm -> LTerm
  derefL g t = derefL' g (boundL t) t

  derefL' :: LStore -> [V] -> LTerm -> LTerm
  derefL' g bound (LVar y)
    | a == Nothing = if (elem y bound) then (LVar y) else error (y ++ " has no binding")
    | otherwise = derefL' (Map.delete y g) bound (Maybe.fromJust a)
    where a = Map.lookup y g
  derefL' g bound (LLambda p x t term) = LLambda p x t (derefL' g (x : (bound ++ (boundL term))) term)
  derefL' g bound (LApp t1 t2) = LApp (derefL' g bound t1) (derefL' g bound t2)
  derefL' g bound (LPair t1 t2 p) = LPair (derefL' g bound t1) (derefL' g bound t2) p
  derefL' g bound (LSplit t1 y z t2) = LSplit (derefL' g bound t1) y z (derefL' g bound t2)

  runl :: String -> String -> (LStore, LTerm)
  runl store term = lbury (Parser.parseLStore store) (Parser.parseLTerm term)

  runderefL :: String -> String -> LTerm
  runderefL store term = derefL (Parser.parseLStore store) (Parser.parseLTerm term)

  runLO :: String -> String -> (LStore, LTerm)
  runLO store term = evalL (Parser.parseLStore store) (Parser.parseLTerm term)
