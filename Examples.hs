module Examples where
import Translations

-- DAVID WALKER CALCULUS
ex1, ex2, ex3, ex4 :: String
ex1 = "un \\x:un Bool.x"  
ex2 = "lin \\x: lin Bool. lin \\y: lin Bool. split lin <x,y> as a, b in (lin <a,b>)"  
ex3 = "(lin \\f: un (lin a -> lin a). lin \\x: lin a. f (f x)) (un \\y: lin a. y)"
ex4 = "lin <" ++ ex3 ++ ", lin \\y: lin a. y>"

-- LINEAR HASKELL
ex5 :: String
ex5 = "let w [x : a ->w a = \\w y: a. y] in 1 <x, x>"
