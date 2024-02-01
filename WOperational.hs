module WOperational where
  import WData
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.List as List
  import Parser

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

  -- substitution
  subst :: (V, Term) -> Term -> Term
  subst (x, y) (Var z) = if x == z then y else (Var z)
  subst (x, y) (Pair q t1 t2) = Pair q (subst (x, y) t1) (subst (x, y) t2)
  subst (x, y) (Split t1 a b t2) = Split (subst (x, y) t1) a b (subst (x, y) t2)
  subst (x, y) (Lambda q z p t) = if x == z then (Lambda q z p t) else (Lambda q z p (subst (x, y) t))
  subst (x, y) (App t1 t2) = App (subst (x, y) t1) (subst (x, y) t2)

  -- evaluation function
  eval :: Store -> Term -> (Store, Term)
  eval s (Pair q (Var y) (Var z)) = let addr = nextAddr s
                                        x = "a" ++ show addr
                                    in (Map.insert x (QValue q (RPair y z)) s, Var x)
  eval s (Pair q (Var y) e) = let (s', v) = eval s e
                              in eval s' (Pair q (Var y) v)
  eval s (Pair q e t) = let (s', v) = eval s e
                        in eval s' (Pair q v t)
  eval s (Split (Var x) y z t) = let Just (QValue q (RPair y1 z1)) = Map.lookup x s
                                     s'  = updateStore s q x
                                     t'  = subst (y, Var y1) t
                                     t'' = subst (z, Var z1) t'
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
  sepValue (Just (QValue q (RPair x1 x2))) = Pair q (Var x1) (Var x2)

  boundW :: Term -> [V]
  boundW (Var x) = []
  boundW (Lambda q x t term) = x : (boundW term)
  boundW (App t1 t2) = (boundW t1) ++ (boundW t2)
  boundW (Pair q t1 t2) = (boundW t1) ++ (boundW t2)
  boundW (Split t1 y z t2) = (boundW t1) ++ (boundW t2)

  derefW :: Store -> Term -> Term
  derefW s t = derefW' s (boundW t) t

  derefW' :: Store -> [V] -> Term -> Term
  derefW' s bound (Var y)
    | a == Nothing = if (elem y bound) then (Var y) else error (y ++ " has no binding")
    | otherwise = derefW' (Map.delete y s) bound (sepValue a)
    where a = Map.lookup y s
  derefW' s bound (Lambda q x t (Var y)) = Lambda q x t (derefW' s (x : bound) (Var y))
  derefW' s bound (Lambda q x t term) = Lambda q x t (derefW' s (x : (bound ++ (boundW term))) term)
  derefW' s bound (App t1 t2) = App (derefW' s bound t1) (derefW' s bound t2)
  derefW' s bound (Pair q t1 t2) = Pair q (derefW' s bound t1) (derefW' s bound t2)
  derefW' s bound (Split t1 y z t2) = Split (derefW' s bound t1) y z (derefW' s bound t2)

  runderefW :: String -> String -> Term
  runderefW store term = derefW (Parser.parseWStore store) (Parser.parseWTerm term)

  runWO :: String -> String -> (Store, Term)
  runWO store term = eval (Parser.parseWStore store) (Parser.parseWTerm term)
