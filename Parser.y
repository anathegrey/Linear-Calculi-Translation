{
module Parser where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import WData
import LData
}

%name parse1 Env
%name parse2 Term
%name parse3 Store
%name parse4 LEnv
%name parse5 LTerm
%name parse6 LStore


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
      split     { TokenSplit }
      as        { TokenAs }
      in        { TokenIn }
      var       { TokenVar $$ }

%left AP
%right '\\' "->" '.'
%nonassoc "lin" "un" "1" "w" "Bool" bool '*' '<' '>' ':' ',' '(' ')' split as in var

%%
Env : {- empty -} { [] }
    | Env ',' E1 { $3 : $1 }
    | E1 { [$1] }

E1 : var ':' Type { ($1, $3) }

Store : {- empty -} { [] }
       | Store ',' S { $3 : $1 }
       | S { [$1] }

S : var ':' Values { ($1, $3) }

Term : var { Var $1 }
     | Qual '<' Term ',' Term '>' { Pair $1 $3 $5 }
     | split Term as var ',' var in Term { Split $2 $4 $6 $8 }
     | Qual '\\' var ':' Type '.' Term { Lambda $1 $3 $5 $7 }
     | Term Term %prec AP { App $1 $2 }
     | '(' Term ')' { $2 }

Qual : "lin" { LIN }
     | "un" { UN }

Type : '(' Type ')' { $2 }
     | Qual PreType { Pre $1 $2 }

PreType : "Bool" { TBool }
        | Type "->" Type { Arrow $1 $3 }
        | Type '*' Type { TypePair $1 $3 }
        | var { TVar $1 }
        | '(' PreType ')' { $2 }

Values : Qual PreValues { QValue $1 $2 }

PreValues : '<' var ',' var '>' { RPair $2 $4 }
          | '\\' var ':' Type '.' Term { RLambda $2 $4 $6 }

LEnv : {- empty -} { [] }
     | LEnv ',' LE1 { $3 : $1 }
     | LE1 { [$1] }

LE1 : var ':' Pi LType { ($1, $3, $4) }

LStore : {- empty -} { [] }
       | LStore ',' LS { $3 : $1 }
       | LS { [$1] }

LS : var ':' LTerm { ($1, $3) }

LTerm : var { LVar $1 }
      | Pi '<' LTerm ',' LTerm '>' { LPair $3 $5 $1 }
      | split LTerm as var ',' var in LTerm { LSplit $2 $4 $6 $8 }
      | '\\' Pi var ':' LType '.' LTerm { LLambda $2 $3 $5 $7}
      | LTerm LTerm %prec AP { LApp $1 $2 }
      | '(' LTerm ')' { $2 }

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
      | TokenDot
      | TokenColon
      | TokenComma
      | TokenOBrack
      | TokenCBrack
      | TokenSplit
      | TokenAs
      | TokenIn
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
lexer ('.':cs) = TokenDot : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs

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
      (var, rest) -> TokenVar var : lexer rest

parseWEnv :: String -> Env
parseWEnv env = parse1(lexer env)

parseWTerm :: String -> Term
parseWTerm term = parse2(lexer term)

parsePrimWStore :: [(String, Values)] -> Store
parsePrimWStore [] = Map.empty
parsePrimWStore l = Map.fromList l

parseWStore :: String -> Store
parseWStore store = parsePrimWStore $ parse3 (lexer store)

parseLEnv :: String -> LEnv
parseLEnv env = parse4(lexer env)

parseLTerm :: String -> LTerm
parseLTerm term = parse5(lexer term)

parsePrimLStore :: [(String, LTerm)] -> LStore
parsePrimLStore [] = Map.empty
parsePrimLStore l = Map.fromList l

parseLStore :: String -> LStore
parseLStore store = parsePrimLStore $ parse6 (lexer store)

parseWL :: String -> Term
parseWL term = parse2(lexer term)

parseLW :: String -> LTerm
parseLW term = parse5(lexer term)

}
