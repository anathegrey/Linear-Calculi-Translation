module Translations where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import WData
  import LData
  import WalkerCalculus
  import LinearHaskell
  import WParser
  import LParser

  -- DAVID WALKER CALCULUS => LINEAR HASKELL

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

  translTypeWL :: WData.Type -> LData.Type
  translTypeWL (Pre q WData.TBool) = LData.TBool
  translTypeWL (Pre q (WData.TypePair t1 t2)) = case q of
                                                 LIN -> LData.TypePair (translTypeWL t1) One (translTypeWL t2)
                                                 UN  -> LData.TypePair (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (WData.Arrow t1@(Pre q' t1') t2)) = case q' of
                                                           LIN -> LData.Arrow (translTypeWL t1) One (translTypeWL t2)
                                                           UN  -> LData.Arrow (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (WData.TVar a)) = LData.TVar a

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


  -- LINEAR HASKELL => DAVID WALKER CALCULUS


  translGtoS :: LData.Store -> WData.Store
  translGtoS g = Map.fromList (translGtoS' (Map.toList g)) 
    where
      translGtoS' [] = []
      translGtoS' ((x, (p, t, term)) : l) = let v = LinearHaskell.initDeref (Map.fromList l) term
                                            in case p of
                                                One -> (x, QValue LIN (translPreValues (snd v))) : (translGtoS' l)
                                                Omega -> (x, QValue UN (translPreValues (snd v))) : (translGtoS' l)

  translPreValues :: LData.Term -> PreValues
  translPreValues (LData.Lambda pi x t term) = RLambda x (translTypeLW pi t) (translLW term)
  translPreValues (LData.Pair term1 term2 pi) = RPair (translLW term1) (translLW term2)

  translLW :: LData.Term -> WData.Term
  translLW (LData.Var x) = WData.Var x
  translLW (LData.Pair term1 term2 pi) = case pi of
                                          One -> WData.Pair LIN (translLW term1) (translLW term2)
                                          Omega -> WData.Pair UN (translLW term1) (translLW term2)
  translLW (LData.Split term1 x y term2) = WData.Split (translLW term1) x y (translLW term2)
  translLW (LData.Lambda pi x t term) = case pi of
                                         One -> WData.Lambda LIN x (translTypeLW pi t) (translLW term)
                                         Omega -> WData.Lambda UN x (translTypeLW pi t) (translLW term)
  translLW (LData.App term1 term2) = WData.App (translLW term1) (translLW term2)
  translLW (LData.Let p [] term) = translLW term
  translLW (LData.Let p ((x, t, term') : xs) term) = translLW (LData.Let p xs (LinearHaskell.subst (x, term') term))


  translTypeLW :: Pi -> LData.Type -> WData.Type
  translTypeLW q LData.TBool = case q of
                                One -> Pre LIN WData.TBool
                                Omega -> Pre UN WData.TBool
  translTypeLW _ (LData.TypePair t1 q t2) = case q of
                                             One -> Pre LIN (WData.TypePair (translTypeLW q t1) (translTypeLW q t2))
                                             Omega -> Pre UN (WData.TypePair (translTypeLW q t1) (translTypeLW q t2))
  translTypeLW _ (LData.Arrow t1 q' t2) = case q' of
                                           One -> Pre LIN (WData.Arrow (translTypeLW q' t1) (translTypeLW q' t2))
                                           Omega -> Pre UN (WData.Arrow (translTypeLW q' t1) (translTypeLW q' t2))
  translTypeLW q (LData.TVar a) = case q of
                                   One -> Pre LIN (WData.TVar a)
                                   Omega -> Pre UN (WData.TVar a)
  runWL :: String -> LData.Term
  runWL term = translWL (WParser.parseWTerm term)

  runLW :: String -> WData.Term
  runLW term = translLW (LParser.parseLTerm term)

  runTransfTypeWL :: String -> LData.Type
  runTransfTypeWL t = translTypeWL (WParser.parseWType t)

  runTransfTypeLW :: Pi -> String -> WData.Type
  runTransfTypeLW pi t = translTypeLW pi (LParser.parseLType t)

  example1, example2 :: String
  example1 = "lin <lin \\x: lin a. x, lin \\y: lin a. y>"
  example2 = "1 <\\1 x: a. x, \\1 y: a. y>"
