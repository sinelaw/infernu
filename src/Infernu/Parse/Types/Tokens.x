{
module Infernu.Parse.Types.Tokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$upper = [A-Z]
$lower = [a-z]
tokens :-

  $white+                       ;
  \-\>                          { \p s -> TokenArrow p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  \[                            { \p s -> TokenLBracket p }
  \]                            { \p s -> TokenRBracket p }
  \{                            { \p s -> TokenLBrace p }
  \}                            { \p s -> TokenRBrace p }
  $lower [$alpha $digit \_ \']* { \p s -> TokenVar p s }
  $upper [$alpha $digit \_ \']* { \p s -> TokenCons p s }

{

-- The token type:
data Token = TokenLParen AlexPosn
           | TokenRParen AlexPosn
           | TokenLBracket AlexPosn
           | TokenRBracket AlexPosn
           | TokenLBrace AlexPosn
           | TokenRBrace AlexPosn
           | TokenArrow AlexPosn
           | TokenVar AlexPosn String
           | TokenCons AlexPosn String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
