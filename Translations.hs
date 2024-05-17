module Translations where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import WData
  import LData
  import WalkerCalculus
  import LinearHaskell
  import WParser
  import LParser

  translTypeWL :: WData.Type -> LData.Type
  translTypeWL (Pre q WData.TBool) = LData.TBool
  translTypeWL (Pre q (WData.TypePair t1 t2)) = case q of
                                                 LIN -> LData.TypePair (translTypeWL t1) One (translTypeWL t2)
                                                 UN  -> LData.TypePair (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (WData.Arrow (Pre q' t1') t2)) = case q' of
                                                        LIN -> LData.Arrow (translTypeWL (Pre q' t1')) One (translTypeWL t2)
                                                        UN  -> LData.Arrow (translTypeWL (Pre q' t1')) Omega (translTypeWL t2)
  translTypeWL (Pre q (WData.TVar a)) = LData.TVar a

  translTypeLW :: Pi -> LData.Type -> WData.Type
  translTypeLW q LData.TBool = case q of
                                One -> Pre LIN WData.TBool
                                Omega -> Pre UN WData.TBool
  translTypeLW _ (LData.TypePair t1 q t2) = case q of
                                             One -> Pre LIN (WData.TypePair (translTypeLW q t1) (translTypeLW q t2))
                                             Omega -> Pre UN (WData.TypePair (translTypeLW q t1) (translTypeLW q t2))
  translTypeLW q (LData.Arrow t1 q' t2) = case q of
                                           One -> Pre LIN (WData.Arrow (translTypeLW q' t1) (translTypeLW q' t2))
                                           Omega -> Pre UN (WData.Arrow (translTypeLW q' t1) (translTypeLW q' t2))
  translTypeLW q (LData.TVar a) = case q of
                                   One -> Pre LIN (WData.TVar a)
                                   Omega -> Pre UN (WData.TVar a)

{-  derefW :: [(WData.V, Values)] -> WData.V -> Term
  derefW [] y = Var y
  derefW ((x, QValue q (RLambda z t term)) : l) y = if x == y then (Lambda q z t term) else (derefW l y)
  derefW ((x, QValue q (RPair x1 x2)) : l) y = if x == y then (Pair q (derefW l x1) (derefW l x2)) else (derefW l y) -}

  translStoG :: WData.Store -> LData.Store
  translStoG s = Map.fromList (translStoG' (Map.toList s))
    where
      translStoG' [] = []
      translStoG' ((x, QValue q (RLambda y (Pre q' t) term)) : l) = case q of
                                                                      LIN -> (x, (One, translTypeWL (fst (WalkerCalculus.typing [] (WData.Lambda q y (Pre q' t) term))), translWL (WData.Lambda q y (Pre q' t) term))) : (translStoG' l)
                                                                      UN  -> (x, (Omega, translTypeWL (fst (WalkerCalculus.typing [] (WData.Lambda q y (Pre q' t) term))), translWL (WData.Lambda q y (Pre q' t) term))) : (translStoG' l)
      translStoG' ((x, QValue q (RPair t1 t2)) : l) = case q of
                                                        LIN -> (x, (One, translTypeWL (fst (WalkerCalculus.typing [] (WData.Pair q t1 t2))), translWL (WData.Pair q t1 t2))) : (translStoG' l)
                                                        UN  -> (x, (Omega, translTypeWL (fst (WalkerCalculus.typing [] (WData.Pair q t1 t2))), translWL (WData.Pair q t1 t2))) : (translStoG' l)

  translWL :: WData.Term -> LData.Term
  translWL (WData.Var x) = (LData.Var x)
  translWL (WData.Pair q term1 term2) = case q of
                                         LIN -> LData.Pair (translWL term1) (translWL term2) One
                                         UN  -> LData.Pair (translWL term1) (translWL term2) Omega
  translWL (WData.Split term1 x y term2) = LData.Split (translWL term1) x y (translWL term2)
  translWL (WData.Lambda q x t@(Pre q' p) term) = case q' of
                                                   LIN -> LData.Lambda One x (translTypeWL t) (translWL term)
                                                   UN  -> LData.Lambda Omega x (translTypeWL t) (translWL term)
  translWL (WData.App term1 term2) = LData.App (translWL term1) (translWL term2)

{-
  derefL :: [(LData.V, (Pi, LType, LTerm))] -> LTerm -> Values
  derefL _
  derefL ((x, (p, t, term)) : l) (LVar y) = if x == y then (termToValue x (translPi p) (derefL l term)) else (deref l (LVar y))
  derefL l (LLambda p y t term) = QValue (translPi p) (RLambda y (Pre (translPi p) translTypeLW t) (derefL l term))
  derefL l (LPair (LVar x1) (LVar x2) p) = QValue (translPi p) (RPair x1 x2)
  derefL l (LSplit term1 x y term2) = derefL l term1
  derefL l (LApp term1 term2) =

  LPair LTerm LTerm Pi
 | LSplit LTerm V V LTerm   -- split t as x,y in t
 | LLambda Pi V LType LTerm  -- q \x:T.t
 | LApp LTerm LTerm
 | Let Pi [(V, LType, LTerm)] LTerm
  translGtoS :: LStore -> Store
  translGtoS g = Map.fromList (translGtoS' (Map.toList g))
    where
      translGtoS' [] = []
      translGtoS' ((x, (p, t, term)) : l) = let v = derefL l term
                                            in (x, translPi p, v) : (translGtoS' l)

  translLW :: LTerm -> (Store, Term)
  translLW (LVar x) = (Map.empty, Var x)
  translLW (LPair term1 term2 q) = case q of
                                    One -> (Map.empty, Pair LIN (translLW term1) (translLW term2))
                                    Omega -> (Map.empty, Pair UN (translLW term1) (translLW term2))
  translLW (LSplit term1 x y term2) = (Map.empty, Split (translLW term1) x y (translLW term2))
  translLW (LLambda q x t term) = (Map.empty, Lambda UN x (translTypeLW q t) (translLW term))
  translLW (LApp term1 term2) = (Map.empty, App (translLW term1) (translLW term2))
  translLW (Let p l term) = (ctxAdd (toS l) s', term')
    where (s, term') = translLW term
          toS ((x, t, term1) : l) = (x, translLW)
          (s', term1') = translLW term1

  (V, LType, LTerm)
-}
  runWL :: String -> LData.Term
  runWL term = translWL (WParser.parseWTerm term)

 -- runLW :: String -> WData.Term
 -- runLW term = translLW (WParser.parseWTerm term)

  example1, example2 :: String
  example1 = "lin <lin \\x: lin a. x, lin \\y: lin a. y>"
  example2 = "1 <\\1 x: a. x, \\1 y: a. y>"
