{
module Parser where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data
}

%name parse1 WEnv
%name parse2 WTerm
%name parse3 WStore
%name parse4 WType
%name parse5 LEnv
%name parse6 LTerm
%name parse7 LStore
%name parse8 LType

%tokentype { Token }
%error { parseError }

%token
      "lin"     { TokenLin }
      "un"      { TokenUn }
      "1"       { TokenOne }
      "w"       { TokenOmega }
      "Bool"    { TokenStringBool }
      '\\'      { TokenLambda }
      "->"      { TokenArrow }
      '*'       { TokenPair }
      '<'       { TokenLess }
      '>'       { TokenBigger }
      '.'       { TokenDot }
      ':'       { TokenColon }
      ','       { TokenComma }
      '('       { TokenOBrack }
      ')'       { TokenCBrack }
      '['       { TokenOSquare }
      ']'       { TokenCSquare }
      '='       { TokenEq }
      split     { TokenSplit }
      as        { TokenAs }
      in        { TokenIn }
      let       { TokenLet }
      var       { TokenVar $$ }

%left AP 
%right '\\' "->" '.' 
%nonassoc "lin" "un" "1" "w" "Bool" '*' '<' '>' ':' ',' '(' ')' '[' ']' '=' let var split as in

%%
WEnv : {- empty -} { [] }
     | WEnv ',' WE1 { $3 : $1 }
     | WE1 { [$1] }

WE1 : var ':' WType { ($1, $3) }

WStore : {- empty -} { [] }
       | WStore ',' WS { $3 : $1 }
       | WS { [$1] }

WS : var '=' Values { ($1, $3) }

WTerm : var { WVar $1 }
      | Qual '<' WTerm ',' WTerm '>' { WPair $1 $3 $5 }
      | split WTerm as var ',' var in WTerm { WSplit $2 $4 $6 $8 }
      | Qual '\\' var ':' WType '.' WTerm { WLambda $1 $3 $5 $7 }
      | WTerm WTerm %prec AP { WApp $1 $2 }
      | '(' WTerm ')' { $2 }

Qual : "lin" { LIN }
     | "un" { UN }

WType : '(' WType ')' { $2 }
      | Qual PreType { Pre $1 $2 }

PreType : "Bool" { WTBool }
        | WType "->" WType { WArrow $1 $3 }
        | WType '*' WType { WTypePair $1 $3 }
        | var { WTVar $1 }
        | '(' PreType ')' { $2 }

Values : Qual PreValues { QValue $1 $2 }

PreValues : '<' WTerm ',' WTerm '>' { RPair $2 $4 }
          | '\\' var ':' WType '.' WTerm { RLambda $2 $4 $6 }

LEnv : {- empty -} { [] }
     | LEnv ',' LE1 { $3 : $1 }
     | LE1 { [$1] }

LE1 : var ':' Pi LType { ($1, $3, $4) }

LStore : {- empty -} { [] }
       | LStore ',' LS { $3 : $1 }
       | LS { [$1] }

LS : var ':' Pi LType '=' LTerm { ($1, ($3, $4, $6)) }

LTerm : var { LVar $1 }
      | Pi '<' LTerm ',' LTerm '>' { LPair $3 $5 $1 }
      | split LTerm as var ',' var in LTerm { LSplit $2 $4 $6 $8 }
      | '\\' Pi var ':' LType '.' LTerm { LLambda $2 $3 $5 $7}
      | LTerm LTerm %prec AP { LApp $1 $2 }
      | let Pi '[' Vs ']' in LTerm { Let $2 $4 $7 } 
      | '(' LTerm ')' { $2 }

Vs : {- empty -} { [] }
   | Vs ',' V { $3 : $1 }
   | V { [$1] }

V : var ':' LType '=' LTerm { ($1, $3, $5) } 

Pi : "1" { One }
   | "w" { Omega }

LType : "Bool" { LTBool }
      | LType "->" Pi LType { LArrow $1 $3 $4 }
      | LType '*' Pi LType { LTypePair $1 $3 $4 }
      | var { LTVar $1 }
      | '(' LType ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenVar String
      | TokenLin
      | TokenUn
      | TokenOne
      | TokenOmega
      | TokenStringBool
      | TokenArrow
      | TokenLambda
      | TokenPair
      | TokenLess
      | TokenBigger
      | TokenEq
      | TokenDot
      | TokenColon
      | TokenComma
      | TokenOBrack
      | TokenCBrack
      | TokenOSquare
      | TokenCSquare
      | TokenSplit
      | TokenAs
      | TokenIn
      | TokenLet
      deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexVar (c:cs)
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('*':cs) = TokenPair : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenBigger : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs
lexer ('[':cs) = TokenOSquare : lexer cs
lexer (']':cs) = TokenCSquare : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
  
lexVar :: String -> [Token]
lexVar cs =
   case span (\x -> (isAlpha x) || (isDigit x)) cs of
      ("split", rest) -> TokenSplit : lexer rest
      ("as", rest) -> TokenAs : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("Bool", rest) -> TokenStringBool : lexer rest
      ("lin", rest) -> TokenLin : lexer rest
      ("un", rest) -> TokenUn : lexer rest
      ("1", rest) -> TokenOne : lexer rest
      ("w", rest) -> TokenOmega : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      (var, rest) -> TokenVar var : lexer rest

parseWEnv :: String -> WEnv
parseWEnv env = parse1(lexer env)

parseWTerm :: String -> WTerm
parseWTerm term = parse2(lexer term)

parsePrimWStore :: [(String, Values)] -> WStore
parsePrimWStore [] = Map.empty
parsePrimWStore l = Map.fromList l

parseWStore :: String -> WStore
parseWStore store = parsePrimWStore $ parse3 (lexer store)

parseWType :: String -> WType
parseWType t = parse4(lexer t)

parseLEnv :: String -> LEnv
parseLEnv env = parse5(lexer env)

parseLTerm :: String -> LTerm
parseLTerm term = parse6(lexer term)

parsePrimLStore :: [(String, (Pi, LType, LTerm))] -> LStore
parsePrimLStore [] = Map.empty
parsePrimLStore l = Map.fromList l

parseLStore :: String -> LStore
parseLStore store = parsePrimLStore $ parse7 (lexer store)

parseLType :: String -> LType
parseLType t = parse8(lexer t)

}

