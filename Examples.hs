module Examples where
import Translations

ex1, ex2, ex3, ex4 :: String
ex1 = "lin \\x:un Bool.x"  -- (Pre LIN (Arrow (Pre UN TBool) (Pre UN TBool)),[])
ex2 = "lin \\x: lin Bool. lin \\y: lin Bool. split lin <x,y> as a, b in (lin <a,b>)"  -- (Pre LIN (TypePair (Pre LIN TBool) (Pre LIN TBool)),[])
ex3 = "(lin \\f: un (lin a -> lin a). lin \\x: lin a. f (f x)) (un \\y: lin a. y)"
ex4 = "lin <" ++ ex3 ++ ", lin \\y: lin a. y)>"
