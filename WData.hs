module WData where
  import Data.Map (Map)
  import qualified Data.Map as Map

  data Q = LIN
         | UN
         deriving (Show, Eq)
  instance Ord Q where
    LIN <= LIN = True
    UN  <= UN  = True
    LIN <= UN  = True
    _   <= _   = False

  type V = String

  data Term = Var V
            | Pair Q Term Term
            | Split Term V V Term   -- split t as x,y in t
            | Lambda Q V Type Term  -- q \x:T.t
            | App Term Term
            deriving (Show, Eq)

  data PreType = TBool
               | TypePair Type Type  -- T * T
               | Arrow Type Type     -- T -> T
               | TVar String
               deriving (Show, Eq)


  data Type = Pre Q PreType
            deriving (Show, Eq)

  type Env = [(V, Type)]
  data PreValues = RPair V V
                 | RLambda V Type Term
                 deriving (Show, Eq)

  data Values = QValue Q PreValues
              deriving (Show, Eq)

  type Store = Map V Values
