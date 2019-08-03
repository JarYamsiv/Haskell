{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  let                           { \s -> TokenLet }
  in                            { \s -> TokenIn }
  if                            { \s -> TokenIf }
  else                          { \s -> TokenElse }
  while                         { \s -> TokenWhile}
  end                           { \s -> TokenEnd}
  $digit+                       { \s -> TokenInt (read s) }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLCurl }
  \}                            { \s -> TokenRCurl }
  
  \=\=                          { \s -> TokenEQ }
  \!\=                          { \s -> TokenNEQ }
  \>                            { \s -> TokenGT }
  \<                            { \s -> TokenLT }
  \>\=                          { \s -> TokenGTEQ }
  \<\=                          { \s -> TokenLTEQ }

  \&\&                          { \s -> TokenAND }
  \|\|                          { \s -> TokenOR }
  \!                            { \s -> TokenNOT }              

  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenIf
           | TokenElse
           | TokenWhile
           | TokenEnd
           | TokenInt Int
           | TokenSym String
           | TokenEq
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenLParen
           | TokenRParen
           | TokenLCurl
           | TokenRCurl

           | TokenEQ
           | TokenNEQ
           | TokenGT
           | TokenLT
           | TokenGTEQ
           | TokenLTEQ

           | TokenOR
           | TokenAND
           | TokenNOT

           deriving (Eq,Show)

scanTokens = alexScanTokens

}
