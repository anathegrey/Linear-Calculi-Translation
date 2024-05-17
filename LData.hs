module LData where
  import Data.Map (Map)
  import qualified Data.Map as Map

  data Pi = One
          | Omega
          deriving (Show, Eq)

  type V = String

  data Term = Var V
            | Pair Term Term Pi
            | Split Term V V Term   -- split t as x,y in t
            | Lambda Pi V Type Term  -- q \x:T.t
            | App Term Term
            | Let Pi [(V, Type, Term)] Term
            deriving (Show, Eq)

  data Type = TBool
            | TypePair Type Pi Type  -- T * T
            | Arrow Type Pi Type     -- T -> T
            | TVar String
            deriving (Show, Eq)

  type Env = [(V, Pi, Type)]

  type Store = Map V (Pi, Type, Term)
