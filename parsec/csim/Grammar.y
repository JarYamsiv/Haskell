{
module Grammar where
import Tokens
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
    let { TokenLet }
    in  { TokenIn }
    if  { TokenIf }
    else { TokenElse }
    while { TokenWhile }
    end { TokenEnd}
    int { TokenInt $$ }
    var { TokenSym $$ }
    '=' { TokenEq }
    '+' { TokenPlus }
    '-' { TokenMinus }
    '*' { TokenTimes }
    '/' { TokenDiv }
    '(' { TokenLParen }
    ')' { TokenRParen }
    '{' { TokenLCurl}
    '}' { TokenRCurl}

    "==" { TokenEQ }
    "!=" { TokenNEQ }
    '>' { TokenGT }
    '<' { TokenLT }
    ">=" { TokenGTEQ }
    "<=" { TokenLTEQ }
    
    "&&" { TokenAND }
    "||" { TokenOR }
    '!' { TokenNOT }

%right in
%nonassoc '>' '<'
%nonassoc ">=" "<="
%left '+' '-'
%left '*' '/'
%left "&&" "||"
%left NEG

%%

Statements : Statement Statements {$1:$2}
            |                       {[]}

Statement : var '=' Exp      { Assignment $1 $3}
          | if  RelExp   Statements end {If $2 $3}
          | if '(' RelExp ')' '{' Statements '}' else '{' Statements '}' {IfEl $3 $6 $10}
          | while '(' RelExp ')' '{' Statements '}' {While $3 $6}

Exp : let var '=' Exp in Exp { Let $2 $4 $6 }
    | Exp '+' Exp            { Plus $1 $3 }
    | Exp '-' Exp            { Minus $1 $3 }
    | Exp '*' Exp            { Times $1 $3 }
    | Exp '/' Exp            { Div $1 $3 }
    | '(' Exp ')'            { $2 }
    | '-' Exp %prec NEG      { Negate $2 }
    | int                    { Int $1 }
    | var                    { Var $1 }

RelExp : Exp '>' Exp        { Gt $1 $3}
       | Exp '<' Exp        { Lt $1 $3}
       | Exp ">=" Exp       { Gteq $1 $3}
       | Exp "<=" Exp       { Lteq $1 $3}
       | Exp "==" Exp       { Eq $1 $3}
       | Exp "!=" Exp       { Neq $1 $3}



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
                | If RelExp Statements
                | IfEl RelExp Statements Statements
                | While RelExp Statements
                deriving Show

data RelExp = Gt Exp Exp
            | Lt Exp Exp
            | Gteq Exp Exp
            | Lteq Exp Exp
            | Eq Exp Exp
            | Neq Exp Exp
            | Not RelExp
            | And RelExp RelExp
            | Or RelExp RelExp
            deriving Show

}
