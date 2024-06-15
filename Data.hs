module Data where
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Control.Monad.State

-- DAVID WALKER CALCULUS
  data Q = LIN
         | UN
         deriving (Show, Eq)
  instance Ord Q where
    LIN <= LIN = True
    UN  <= UN  = True
    LIN <= UN  = True
    _   <= _   = False

  type V = String

  data WTerm = WVar V
             | WPair Q WTerm WTerm
             | WSplit WTerm V V WTerm   -- split t as x,y in t
             | WLambda Q V WType WTerm  -- q \x:T.t
             | WApp WTerm WTerm
             deriving (Show, Eq)

  data PreType = WTBool
               | WTypePair WType WType  -- T * T
               | WArrow WType WType     -- T -> T
               | WTVar String
               deriving (Show, Eq)


  data WType = Pre Q PreType
             deriving (Show, Eq)

  type WEnv = [(V, WType)]

  data PreValues = RPair WTerm WTerm
                 | RLambda V WType WTerm
                 deriving (Show, Eq)

  data Values = QValue Q PreValues
              deriving (Show, Eq)

  type WStore = Map V Values

-- LINEAR HASKELL

  data Pi = One
          | Omega
          deriving (Show, Eq)

  data LTerm = LVar V
             | LPair LTerm LTerm Pi
             | LSplit LTerm V V LTerm   -- split t as x,y in t
             | LLambda Pi V LType LTerm  -- q \x:T.t
             | LApp LTerm LTerm
             | Let Pi [(V, LType, LTerm)] LTerm
             deriving (Show, Eq)

  data LType = LTBool
             | LTypePair LType Pi LType  -- T * T
             | LArrow LType Pi LType     -- T -> T
             | LTVar String
             deriving (Show, Eq)

  type LEnv = [(V, Pi, LType)]

  type LStore = Map V (Pi, LType, LTerm)


  -- function to assist in the creation of new variables
  type Counter = State Int

  getCount :: Counter Int
  getCount = get
