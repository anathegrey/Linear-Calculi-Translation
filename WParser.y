{
module WParser where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import WData
}

%name parse1 Env
%name parse2 Term
%name parse3 Store
%name parse4 Type

%tokentype { Token }
%error { parseError }

%token
      "lin"     { TokenLin }
      "un"      { TokenUn }
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
      '='       { TokenEq }
      split     { TokenSplit }
      as        { TokenAs }
      in        { TokenIn }
      var       { TokenVar $$ }

%left AP
%right '\\' "->" '.'
%nonassoc "lin" "un" "Bool" bool '*' '<' '>' ':' ',' '(' ')' '=' split as in var

%%
Env : {- empty -} { [] }
    | Env ',' E1 { $3 : $1 }
    | E1 { [$1] }

E1 : var ':' Type { ($1, $3) }

Store : {- empty -} { [] }
       | Store ',' S { $3 : $1 }
       | S { [$1] }

S : var '=' Values { ($1, $3) }

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

PreValues : '<' Term ',' Term '>' { RPair $2 $4 }
          | '\\' var ':' Type '.' Term { RLambda $2 $4 $6 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenVar String
      | TokenLin
      | TokenUn
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
lexer ('=':cs) = TokenEq : lexer cs
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

parseWType :: String -> Type
parseWType t = parse4(lexer t)

}

