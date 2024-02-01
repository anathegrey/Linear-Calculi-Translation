module Translations where
  import LData
  import WData
  import Parser

  translQ :: Q -> Q -> Pi
  translQ q q' = case q of
                   LIN -> case q' of
                            LIN -> mult One One
                            UN  -> mult One Omega
                   UN  -> case q' of
                            LIN -> mult Omega One
                            UN  -> mult Omega Omega

  mult :: Pi -> Pi -> Pi
  mult One One = One
  mult _ _ = Omega


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

  translWL :: Term -> LTerm
  translWL (Var x) = (LVar x)
  translWL (Pair q term1 term2) = case q of
                                    LIN -> LPair (translWL term1) (translWL term2) One
                                    UN  -> LPair (translWL term1) (translWL term2) Omega
  translWL (Split term1 x y term2) = LSplit (translWL term1) x y (translWL term2)
  translWL (Lambda q x t@(Pre q' p) term) = let m = translQ q q'
                                            in LLambda m x (translTypeWL t) (translWL term)
  translWL (App term1 term2) = LApp (translWL term1) (translWL term2)

  translLW :: LTerm -> Term
  translLW (LVar x) = (Var x)
  translLW (LPair term1 term2 q) = case q of
                                    One -> Pair LIN (translLW term1) (translLW term2)
                                    Omega -> Pair UN (translLW term1) (translLW term2)
  translLW (LSplit term1 x y term2) = Split (translLW term1) x y (translLW term2)
  translLW (LLambda q x t term) = Lambda UN x (translTypeLW q t) (translLW term)
  translLW (LApp term1 term2) = App (translLW term1) (translLW term2)

  runWL :: String -> LTerm
  runWL term = translWL (Parser.parseWL term)

  runLW :: String -> Term
  runLW term = translLW (Parser.parseLW term)

  example1, example2 :: String
  example1 = "lin <lin \\x: lin a. x, lin \\y: lin a. y>"
  example2 = "1 <\\1 x: a. x, \\1 y: a. y>"
