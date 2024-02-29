module LTypechecker where
  import LData
  import Data.List
  import Parser

  mult :: Pi -> Pi -> Pi
  mult One One = One
  mult _ _ = Omega

  typingL :: LEnv -> LTerm -> (LType, LEnv)
  typingL ((y, p, t) : env) (LVar x) = let (t', env') = typingL env (LVar x)
                                      in if x == y then (if p == One then (t, env) else (t, (y, p, t) : env)) else (t', (y, p, t) : env')
  typingL g (LLambda p x t term) = let (t', env') = typingL (g ++ [(x, p, t)]) term
                                   in (LArrow t p t', env')
  typingL g (LApp term1 term2) = let (LArrow t1 p t2, env') = typingL g term1
                                     (t, env'')             = typingL env' term2
                                 in if t1 == t then (t2, env'') else error "Type not consistent in application"
  typingL g (LPair term1 term2 p) = let (t', g')   = typingL g term1
                                        (t'', g'') = typingL g' term2
                                    in (LTypePair t' p t'', g'')
  typingL g (LSplit term1 x y term2) = let (LTypePair t1 p t2, g') = typingL g term1
                                           (t, g'')                = typingL (g' ++ [(x, p, t1), (y, p, t2)]) term2
                                       in (t, g'')
  typingL g (Let p [] term) = typingL g term
  typingL g (Let p ((x, a, t) : l) term) = let (a1, g1) = typingL g t
                                          in if a == a1 then (typingL (g1 ++ [(x, p, a)]) (Let p l term)) else error "Types are inconsistent in Let"

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
  runLT :: String -> String -> (LType, LEnv)
  runLT env term = typingL (Parser.parseLEnv env) (Parser.parseLTerm term)
