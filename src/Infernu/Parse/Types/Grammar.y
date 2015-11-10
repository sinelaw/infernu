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

%right ar
%right APP
%%

TypeExp : '(' TypeExp ')'           { $2 }
        | '[' TypeExp ']'           { Array $2 }
        | TypeExp ar TypeExp        { Arr $1 $3 }
        | TypeExp TypeExp %prec APP { App $1 $2 }
        | cons                      { cons $1 }
        | var                       { Var $1 }

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
    "EmptyThis" -> EmptyThis
    "Date" -> Date
    _ -> error $ "Unknown constructor name: " ++ s

data TypeExp = App TypeExp TypeExp
             | Arr TypeExp TypeExp
             | Var String
             | Cons String
             | Array TypeExp
             | Number | Boolean | String | Regex | Undefined | Null | EmptyThis | Date
             deriving Show
}
