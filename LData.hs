module LData where
  import Data.Map (Map)
  import qualified Data.Map as Map

  data Pi = One
          | Omega
          deriving (Show, Eq)

  type V = String

  data LTerm = LVar V
            | LPair LTerm LTerm Pi
            | LSplit LTerm V V LTerm   -- split t as x,y in t
            | LLambda Pi V LType LTerm  -- q \x:T.t
            | LApp LTerm LTerm
            deriving (Show, Eq)

  data LType = LTBool
            | LTypePair LType Pi LType  -- T * T
            | LArrow LType Pi LType     -- T -> T
            | LTVar String
            deriving (Show, Eq)

  type LEnv = [(V, Pi, LType)]

  type LStore = Map V LTerm
