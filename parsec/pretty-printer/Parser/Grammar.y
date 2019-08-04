{
module Parser.Grammar where
import Parser.Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    let   { TokenLet }
    in    { TokenIn }
    if    { TokenIf }
    else  { TokenElse }
    while { TokenWhile }
    int   { TokenInt $$ }
    var   { TokenSym $$ }
    '='   { TokenEq }
    '+'   { TokenPlus }
    '-'   { TokenMinus }
    '*'   { TokenTimes }
    '/'   { TokenDiv }
    '('   { TokenLParen }
    ')'   { TokenRParen }
    '{'   { TokenLCurl}
    '}'   { TokenRCurl}

%right in
%nonassoc '>' '<'
%left '+' '-'
%left '*' '/'
%left NEG

%%

Statements : Statement Statements {$1:$2}
            |                       {[]}

Statement : var '=' Exp      { Assignment $1 $3}
          | if '(' var ')' '{' Statements '}' {If $3 $6}
          | if '(' var ')' '{' Statements '}' else '{' Statements '}' {IfEl $3 $6 $10}
          | while '(' var ')' '{' Statements '}' {While $3 $6}

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }





{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = Let String Exp Exp
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Div Exp Exp
         | Negate Exp
         | Brack Exp
         | Int Int
         | Var String
         deriving Show

type Statements = [Statement]


data Statement = Assignment String Exp
                | If String Statements
                | IfEl String Statements Statements
                | While String Statements
                deriving Show

}
