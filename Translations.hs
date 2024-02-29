module Translations where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import WData
  import LData
  import WOperational
  import LOperational
  import WTypechecker
  import LTypechecker
  import Parser

  translTypeWL :: Type -> LType
  translTypeWL (Pre q TBool) = LTBool
  translTypeWL (Pre q (TypePair t1 t2)) = case q of
                                            LIN -> LTypePair (translTypeWL t1) One (translTypeWL t2)
                                            UN  -> LTypePair (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (Arrow (Pre q' t1') t2)) = case q' of
                                                  LIN -> LArrow (translTypeWL (Pre q' t1')) One (translTypeWL t2)
                                                  UN  -> LArrow (translTypeWL (Pre q' t1')) Omega (translTypeWL t2)
  translTypeWL (Pre q (TVar a)) = LTVar a

  translTypeLW :: Pi -> LType -> Type
  translTypeLW q LTBool = case q of
                            One -> Pre LIN TBool
                            Omega -> Pre UN TBool
  translTypeLW _ (LTypePair t1 q t2) = case q of
                                         One -> Pre LIN (TypePair (translTypeLW q t1) (translTypeLW q t2))
                                         Omega -> Pre UN (TypePair (translTypeLW q t1) (translTypeLW q t2))
  translTypeLW q (LArrow t1 q' t2) = case q of
                                      One -> Pre LIN (Arrow (translTypeLW q' t1) (translTypeLW q' t2))
                                      Omega -> Pre UN (Arrow (translTypeLW q' t1) (translTypeLW q' t2))
  translTypeLW q (LTVar a) = case q of
                               One -> Pre LIN (TVar a)
                               Omega -> Pre UN (TVar a)

{-  derefW :: [(WData.V, Values)] -> WData.V -> Term
  derefW [] y = Var y
  derefW ((x, QValue q (RLambda z t term)) : l) y = if x == y then (Lambda q z t term) else (derefW l y)
  derefW ((x, QValue q (RPair x1 x2)) : l) y = if x == y then (Pair q (derefW l x1) (derefW l x2)) else (derefW l y) -}

  translStoG :: Store -> LStore
  translStoG s = Map.fromList (translStoG' (Map.toList s))
    where
      translStoG' [] = []
      translStoG' ((x, QValue q (RLambda y (Pre q' t) term)) : l) = case q of
                                                                      LIN -> (x, (One, translTypeWL (fst (typing [] (Lambda q y (Pre q' t) term))), translWL (Lambda q y (Pre q' t) term))) : (translStoG' l)
                                                                      UN  -> (x, (Omega, translTypeWL (fst (typing [] (Lambda q y (Pre q' t) term))), translWL (Lambda q y (Pre q' t) term))) : (translStoG' l)
      translStoG' ((x, QValue q (RPair t1 t2)) : l) = case q of
                                                        LIN -> (x, (One, translTypeWL (fst (typing [] (Pair q t1 t2))), translWL (Pair q t1 t2))) : (translStoG' l)
                                                        UN  -> (x, (Omega, translTypeWL (fst (typing [] (Pair q t1 t2))), translWL (Pair q t1 t2))) : (translStoG' l)

  translWL :: Term -> LTerm
  translWL (Var x) = (LVar x)
  translWL (Pair q term1 term2) = case q of
                                    LIN -> LPair (translWL term1) (translWL term2) One
                                    UN  -> LPair (translWL term1) (translWL term2) Omega
  translWL (Split term1 x y term2) = LSplit (translWL term1) x y (translWL term2)
  translWL (Lambda q x t@(Pre q' p) term) = case q' of
                                              LIN -> LLambda One x (translTypeWL t) (translWL term)
                                              UN  -> LLambda Omega x (translTypeWL t) (translWL term)
  translWL (App term1 term2) = LApp (translWL term1) (translWL term2)

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
  runWL :: String -> LTerm
  runWL term = translWL (Parser.parseWL term)

--  runLW :: String -> Term
--  runLW term = translLW (Parser.parseLW term)

  example1, example2 :: String
  example1 = "lin <lin \\x: lin a. x, lin \\y: lin a. y>"
  example2 = "1 <\\1 x: a. x, \\1 y: a. y>"
