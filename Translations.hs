module Translations where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Text.PrettyPrint
  import Data
  import WalkerCalculus
  import LinearHaskell
  import Parser
  import Pretty

  -- DAVID WALKER CALCULUS => LINEAR HASKELL

-- translation of an environment in the David Walker Calculus to Linear Haskell
  translStoG :: WStore -> LStore
  translStoG s = Map.fromList (translStoG' (Map.toList s))
    where
      translStoG' [] = []
      translStoG' ((x, QValue q (RLambda y (Pre q' t) term)) : l) = case q of
                                                                      LIN -> (x, (One, translTypeWL (fst (WalkerCalculus.typing [] (WLambda q y (Pre q' t) term))), translWL (WLambda q y (Pre q' t) term))) : (translStoG' l)
                                                                      UN  -> (x, (Omega, translTypeWL (fst (WalkerCalculus.typing [] (WLambda q y (Pre q' t) term))), translWL (WLambda q y (Pre q' t) term))) : (translStoG' l)
      translStoG' ((x, QValue q (RPair t1 t2)) : l) = case q of
                                                        LIN -> (x, (One, translTypeWL (fst (WalkerCalculus.typing [] (WPair q t1 t2))), translWL (WPair q t1 t2))) : (translStoG' l)
                                                        UN  -> (x, (Omega, translTypeWL (fst (WalkerCalculus.typing [] (WPair q t1 t2))), translWL (WPair q t1 t2))) : (translStoG' l)

-- translation of a type in the David Walker Calculus to Linear Haskell
  translTypeWL :: WType -> LType
  translTypeWL (Pre q WTBool) = LTBool
  translTypeWL (Pre q (WTypePair t1 t2)) = case q of
                                             LIN -> LTypePair (translTypeWL t1) One (translTypeWL t2)
                                             UN  -> LTypePair (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (WArrow t1@(Pre q' t1') t2)) = case q' of
                                                       LIN -> LArrow (translTypeWL t1) One (translTypeWL t2)
                                                       UN  -> LArrow (translTypeWL t1) Omega (translTypeWL t2)
  translTypeWL (Pre q (WTVar a)) = LTVar a

-- translation of terms in the David Walker Calculus to Linear Haskell
  translWL :: WTerm -> LTerm
  translWL (WVar x) = (LVar x)
  translWL (WPair q term1 term2) = case q of
                                    LIN -> LPair (translWL term1) (translWL term2) One
                                    UN  -> LPair (translWL term1) (translWL term2) Omega
  translWL (WSplit term1 x y term2) = LSplit (translWL term1) x y (translWL term2)
  translWL (WLambda q x t@(Pre q' p) term) = case q' of
                                              LIN -> LLambda One x (translTypeWL t) (translWL term)
                                              UN  -> LLambda Omega x (translTypeWL t) (translWL term)
  translWL (WApp term1 term2) = LApp (translWL term1) (translWL term2)


  -- LINEAR HASKELL => DAVID WALKER CALCULUS

-- translation of an environment in Linear Haskell to the David Walker Calculus
  translGtoS :: LStore -> WStore
  translGtoS g = Map.fromList (translGtoS' (Map.toList g)) 
    where
      translGtoS' [] = []
      translGtoS' ((x, (p, t, term)) : l) = let v = LinearHaskell.initDeref (Map.fromList l) term
                                            in case p of
                                                One -> (x, QValue LIN (translPreValues (snd v))) : (translGtoS' l)
                                                Omega -> (x, QValue UN (translPreValues (snd v))) : (translGtoS' l)

-- translation of values in Linear Haskell to the David Walker Calculus
  translPreValues :: LTerm -> PreValues
  translPreValues (LLambda pi x t term) = RLambda x (translTypeLW pi t) (translLW term)
  translPreValues (LPair term1 term2 pi) = RPair (translLW term1) (translLW term2)

-- translation of terms in Linear Haskell to the David Walker Calculus
  translLW :: LTerm -> WTerm
  translLW (LVar x) = WVar x
  translLW (LPair term1 term2 pi) = case pi of
                                      One -> WPair LIN (translLW term1) (translLW term2)
                                      Omega -> WPair UN (translLW term1) (translLW term2)
  translLW (LSplit term1 x y term2) = WSplit (translLW term1) x y (translLW term2)
  translLW (LLambda pi x t term) = case pi of
                                     One -> WLambda LIN x (translTypeLW pi t) (translLW term)
                                     Omega -> WLambda UN x (translTypeLW pi t) (translLW term)
  translLW (LApp term1 term2) = WApp (translLW term1) (translLW term2)
  translLW (Let p [] term) = translLW term
  translLW (Let p ((x, t, term') : xs) term) = translLW (Let p xs (LinearHaskell.subst (x, term') term))

-- translation of a type in Linear Haskell to the David Walker Calculus
  translTypeLW :: Pi -> LType -> WType
  translTypeLW q LTBool = case q of
                            One -> Pre LIN WTBool
                            Omega -> Pre UN WTBool
  translTypeLW _ (LTypePair t1 q t2) = case q of
                                         One -> Pre LIN (WTypePair (translTypeLW q t1) (translTypeLW q t2))
                                         Omega -> Pre UN (WTypePair (translTypeLW q t1) (translTypeLW q t2))
  translTypeLW _ (LArrow t1 q' t2) = case q' of
                                      One -> Pre LIN (WArrow (translTypeLW q' t1) (translTypeLW q' t2))
                                      Omega -> Pre UN (WArrow (translTypeLW q' t1) (translTypeLW q' t2))
  translTypeLW q (LTVar a) = case q of
                              One -> Pre LIN (WTVar a)
                              Omega -> Pre UN (WTVar a)


-- RESULTS
  runWL :: String -> String
  runWL term = render $ prettyLTerm (translWL (Parser.parseWTerm term))

  runLW :: String -> String
  runLW term = render $ prettyWTerm (translLW (Parser.parseLTerm term))

  runTransfTypeWL :: String -> String
  runTransfTypeWL t = render $ prettyLType (translTypeWL (Parser.parseWType t))

  runTransfTypeLW :: Pi -> String -> String
  runTransfTypeLW pi t = render $ prettyWType (translTypeLW pi (Parser.parseLType t))
