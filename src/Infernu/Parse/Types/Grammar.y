{
module Infernu.Parse.Types.Grammar where
import Infernu.Parse.Types.Tokens
}

%name parseType
%tokentype { Token }
%error { parseError }

%token
    var  { TokenVar    _ $$ }
    cons { TokenCons   _ $$ }
    '('  { TokenLParen _ }
    ')'  { TokenRParen _ }
    '['  { TokenLBracket _ }
    ']'  { TokenRBracket _ }
    '{'  { TokenLBrace _ }
    '}'  { TokenRBrace _ }
    ar   { TokenArrow  _ }
    '.'  { TokenDot    _ }
    ','  { TokenComma  _ }
    ':'  { TokenColon  _ }
    '|'  { TokenPipe   _ }

%right '.'
%right ar
%right APP
%%

TypeExp : '(' TypeExp ')'           { $2 }
        -- | '(' tupledn ')'           { Tuple $2 }
        | '[' TypeExp ']'           { Array $2 }
        | TypeExp '.' '(' tupled ar TypeExp ')' { This $1 $4 $6 }
        | TypeExp '.' '(' TypeExp ar TypeExp ')' { This $1 [$4] $6 }
        | TypeExp ar TypeExp        { Arr $1 $3 }
        | TypeExp TypeExp %prec APP { App $1 $2 }
        | '{' fields '}'            { Row $2 Nothing }
        | '{' fields '|' var '}'    { Row $2 (Just $4) }
        | cons                      { cons $1 }
        | var                       { Var $1 }

tupled : '(' tupledn ')' { $2 }

tupledn  : TypeExp            { [$1] }
         | tupledn ',' TypeExp { $3 : $1 }

fields : field { [$1] }
       | fields ',' field { $3 : $1 }

field : var ':' TypeExp { ($1, $3) }



{

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ show ts

cons s =
    case s of
    "Number" -> Number
    "Boolean" -> Boolean
    "String" -> String
    "Regex" -> Regex
    "Undefined" -> Undefined
    "Null" -> Null
    "Date" -> Date
    _ -> error $ "Unknown constructor name: " ++ s

data TypeExp = App TypeExp TypeExp
             | Arr TypeExp TypeExp
             | Var String
             | Cons String
             | Array TypeExp
             | Tuple [TypeExp]
             | This TypeExp [TypeExp] TypeExp
             | Row [(String, TypeExp)] (Maybe String)
             | Number | Boolean | String | Regex | Undefined | Null | Date
             deriving Show
}
