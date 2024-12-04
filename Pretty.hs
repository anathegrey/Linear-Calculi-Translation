{-# LANGUAGE FlexibleInstances #-}

module Pretty where
import Prelude hiding ((<>))
import Text.PrettyPrint
import Data.Map (Map)
import qualified Data.Map as Map
import Data

-- DAVID WALKER CALCULUS

prettyQ :: Q -> Doc
prettyQ LIN = text "lin"
prettyQ UN = text "un"

prettyWTerm :: WTerm -> Doc
prettyWTerm term = prettyPrint' term
  where
    prettyPrint' = prettyPrintWrapped id
    prettyWithParens = prettyPrintWrapped $ \pretty -> text "(" <> pretty <> text ")"
    prettyPrintWrapped wrapper t =
      case t of
        WVar x -> text x
        WPair q t1 t2 -> wrapper $ (prettyQ q) <+> text "<" <> (prettyWTerm t1) <> text "," <+> (prettyWTerm t2) <> text ">"
        WSplit t1 y z t2 -> wrapper $ text "split" <+> (prettyWTerm t1) <+> text "as" <+> text y <> text "," <+> text z <+> text "in" <+> (prettyWTerm t2)
        WLambda q x t t1 -> wrapper $ (prettyQ q) <+> text "\\" <> text x <> text ":" <+> (prettyWType t) <> text "." <+> (prettyWTerm t1)
        WApp t1 t2 -> wrapper $ (prettyWithParens t1) <+> (prettyWithParens t2)

prettyWType :: WType -> Doc
prettyWType t = prettyPrint' t
  where
    prettyPrint' = prettyPrintWrapped id
    prettyWithParens = prettyPrintWrapped $ \pretty -> text "(" <> pretty <> text ")"
    prettyPrintWrapped wrapper t = case t of
      Pre q WTBool -> (prettyQ q) <+> text "Bool"
      Pre q (WTypePair t1 t2) -> wrapper $ (prettyQ q) <+> text "(" <> (prettyWithParens t1) <+> text "*" <+> (prettyWithParens t2) <> text ")"
      Pre q (WArrow t1 t2) -> wrapper $ (prettyQ q) <+> text "(" <> (prettyWithParens t1) <+> text "->" <+> (prettyWithParens t2) <> text ")"
      Pre q (WTVar a) -> (prettyQ q) <+> text a

prettyWEnv :: WEnv -> Doc
prettyWEnv [] = empty
prettyWEnv ((x, t) : []) = text x <> text ":" <+> (prettyWType t)
prettyWEnv ((x, t) : xs) = text x <> text ":" <+> (prettyWType t) <> text "," <+> (prettyWEnv xs)

prettyValues :: Values -> Doc
prettyValues (QValue q (RPair t1 t2)) = (prettyQ q) <+> text "<" <> (prettyWTerm t1) <> text "," <+> (prettyWTerm t2) <> text ">"
prettyValues (QValue q (RLambda x t t1)) = (prettyQ q) <+> text "\\" <> text x <> text ":" <+> (prettyWType t) <> text "." <+> (prettyWTerm t1)

prettyWStore :: WStore -> Doc
prettyWStore store = prettyPrimWStore (Map.toList store)

prettyPrimWStore :: [(V, Values)] -> Doc
prettyPrimWStore [] = empty
prettyPrimWStore ((x, v) : []) =  text x <+> text "=" <+> (prettyValues v)
prettyPrimWStore ((x, v) : xs) = text x <+> text "=" <+> (prettyValues v) <> text "," <+> (prettyPrimWStore xs)


-- LINEAR HASKELL
prettyPi :: Pi -> Doc
prettyPi One = text "1"
prettyPi Omega = text "w"

prettyLList :: [(V, LType, LTerm)] -> Doc
prettyLList [] = empty
prettyLList ((x, t, term) : []) = text "(" <> text x <> text "," <+> (prettyLType t) <> text "," <+> (prettyLTerm term) <> text ")"
prettyLList ((x, t, term) : xs) = text "(" <> text x <> text "," <+> (prettyLType t) <> text "," <+> (prettyLTerm term) <> text ")" <> text "," <+> (prettyLList xs) 

prettyLTerm :: LTerm -> Doc
prettyLTerm term = prettyPrint' term
  where
    prettyPrint' = prettyPrintWrapped id
    prettyWithParens = prettyPrintWrapped $ \pretty -> text "(" <> pretty <> text ")"
    prettyPrintWrapped wrapper t =
      case t of
        LVar x -> text x
        LPair t1 t2 p -> wrapper $ (prettyPi p) <+> text "<" <> (prettyLTerm t1) <> text "," <+> (prettyLTerm t2) <> text ">"
        LSplit t1 y z t2 -> wrapper $ text "split" <+> (prettyLTerm t1) <+> text "as" <+> text y <> text "," <+> text z <+> text "in" <+> (prettyLTerm t2)
        LLambda p x t t1 -> wrapper $ text "\\" <>  (prettyPi p) <+> text x <> text ":" <+> (prettyLType t) <> text "." <+> (prettyLTerm t1)
        LApp t1 t2 -> wrapper $ (prettyWithParens t1) <+> (prettyWithParens t2)
        Let pi xs term -> wrapper $ text "let" <+> (prettyPi pi) <+> text "[" <> prettyLList xs <> text "]" <+> text "in" <+> (prettyLTerm term)

        
prettyLType :: LType -> Doc
prettyLType t = prettyPrint' t
  where
    prettyPrint' = prettyPrintWrapped id
    prettyWithParens = prettyPrintWrapped $ \pretty -> text "(" <> pretty <> text ")"
    prettyPrintWrapped wrapper t = case t of
      LTBool -> text "Bool"
      LTypePair t1 p t2 -> wrapper $ (prettyWithParens t1) <+> text "*" <> (prettyPi p) <+> (prettyWithParens t2)
      LArrow t1 p t2 -> wrapper $ (prettyWithParens t1) <+> text "->" <> (prettyPi p) <+> (prettyWithParens t2)
      LTVar a -> text a

prettyLEnv :: LEnv -> Doc
prettyLEnv [] = empty
prettyLEnv ((x, p, t) : []) = text "(" <> text x <> text ":" <> (prettyPi p) <+> (prettyLType t) <> text ")"
prettyLEnv ((x, p, t) : xs) = text "(" <> text x <> text ":" <> (prettyPi p) <+> (prettyLType t) <> text ")" <> text "," <+> (prettyLEnv xs)

prettyLStore :: LStore -> Doc
prettyLStore store = prettyPrimLStore (Map.toList store)

prettyPrimLStore :: [(V, (Pi, LType, LTerm))] -> Doc
prettyPrimLStore [] = empty
prettyPrimLStore ((x, (p, t, term)) : []) = text x <+> text ":" <> (prettyPi p) <+> (prettyLType t) <+> text "=" <+> (prettyLTerm term)
prettyPrimLStore ((x, (p, t, term)) : xs) = text x <+> text ":" <> (prettyPi p) <+> (prettyLType t) <+> text "=" <+> (prettyLTerm term) <> text "," <+> (prettyPrimLStore xs)
