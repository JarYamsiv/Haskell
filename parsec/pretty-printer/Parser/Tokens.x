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

  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

-- The token type:
data Token = TokenLet
           | TokenIn
           | TokenIf
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
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
