{
module LParser where
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import LData
}

%name parse1 Env
%name parse2 Term
%name parse3 Store


%tokentype { Token }
%error { parseError }

%token
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
      '='       { TokenEq }
      split     { TokenSplit }
      as        { TokenAs }
      in        { TokenIn }
      let       { TokenLet }
      var       { TokenVar $$ }

%left AP
%right '\\' "->" '.'
%nonassoc "1" "w" "Bool" bool '*' '<' '>' ':' ',' '(' ')' '=' split as in let var

%%
Env : {- empty -} { [] }
    | Env ',' E1 { $3 : $1 }
    | E1 { [$1] }

E1 : var ':' Pi Type { ($1, $3, $4) }

Store : {- empty -} { [] }
      | Store ',' S { $3 : $1 }
      | S { [$1] }

S : var ':' Pi Type '=' Term { ($1, ($3, $4, $6)) }

Term : var { Var $1 }
     | Pi '<' Term ',' Term '>' { Pair $3 $5 $1 }
     | split Term as var ',' var in Term { Split $2 $4 $6 $8 }
     | '\\' Pi var ':' Type '.' Term { Lambda $2 $3 $5 $7}
     | Term Term %prec AP { App $1 $2 }
     | let Pi Vs in Term { Let $2 $3 $5 } 
     | '(' Term ')' { $2 }

Vs : {- empty -} { [] }
   | Vs ',' V { $3 : $1 }
   | V { [$1] }

V : var ':' Type '=' Term { ($1, $3, $5) } 

Pi : "1" { One }
   | "w" { Omega }

Type : "Bool" { TBool }
     | Type "->" Pi Type { Arrow $1 $3 $4 }
     | Type '*' Pi Type { TypePair $1 $3 $4 }
     | var { TVar $1 }
     | '(' Type ')' { $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenVar String
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
      | TokenEq
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
lexer ('.':cs) = TokenDot : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs
lexer ('=':cs) = TokenEq : lexer cs

lexVar :: String -> [Token]
lexVar cs =
   case span (\x -> (isAlpha x) || (isDigit x)) cs of
      ("split", rest) -> TokenSplit : lexer rest
      ("as", rest) -> TokenAs : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      ("Bool", rest) -> TokenStringBool : lexer rest
      ("1", rest) -> TokenOne : lexer rest
      ("w", rest) -> TokenOmega : lexer rest
      ("let", rest) -> TokenLet : lexer rest
      (var, rest) -> TokenVar var : lexer rest

parseLEnv :: String -> Env
parseLEnv env = parse1(lexer env)

parseLTerm :: String -> Term
parseLTerm term = parse2(lexer term)

parsePrimLStore :: [(String, (Pi, Type, Term))] -> Store
parsePrimLStore [] = Map.empty
parsePrimLStore l = Map.fromList l

parseLStore :: String -> Store
parseLStore store = parsePrimLStore $ parse3 (lexer store)

}
