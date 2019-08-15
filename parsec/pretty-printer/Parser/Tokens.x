{
module Parser.Tokens where
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
  while                         { \s -> TokenWhile }
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

  \<                            { \s -> TokenLT }
  \>                            { \s -> TokenGT }
  \<\=                          { \s -> TokenLTEQ }
  \>\=                          { \s -> TokenGTEQ }
  \=\=                          { \s -> TokenEQ }
  \!\=                          { \s -> TokenNEQ }
  \&\&                          { \s -> TokenAND }
  \|\|                          { \s -> TokenOR }



  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenIf
           | TokenElse
           | TokenWhile
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
           | TokenLT
           | TokenGT
           | TokenGTEQ
           | TokenLTEQ
           | TokenEQ
           | TokenNEQ
           | TokenAND
           | TokenOR
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
