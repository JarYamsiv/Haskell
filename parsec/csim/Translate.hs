module Translate where
    import Grammar
    translateExp :: Exp -> String
    translateExp (Int v) = show v
    translateExp (Plus e1 e2) = (translateExp e1)++"+"++(translateExp e2)
    translateExp (Minus e1 e2) = (translateExp e1)++"-"++(translateExp e2)
    translateExp (Times e1 e2) = (translateExp e1)++"*"++(translateExp e2)
    translateExp (Div e1 e2) = (translateExp e1)++"/"++(translateExp e2)
    translateExp (Negate e) = "-"++(translateExp e)
    translateExp (Var v)    = v
    translateExp (Let s e1 e2) = ""

    translateRelExp :: RelExp -> String
    translateRelExp (Gt e1 e2) = (translateExp e1)++">"++(translateExp e2)
    translateRelExp (Lt e1 e2) = (translateExp e1)++"<"++(translateExp e2)
    translateRelExp (Gteq e1 e2) = (translateExp e1)++">="++(translateExp e2)
    translateRelExp (Lteq e1 e2) = (translateExp e1)++"<="++(translateExp e2)
    translateRelExp (Eq e1 e2) = (translateExp e1)++"=="++(translateExp e2)
    translateRelExp (Neq e1 e2) = (translateExp e1)++"!="++(translateExp e2)

    translateStatement :: Statement -> String
    translateStatement (Assignment v e) = v++"="++(translateExp e)
    translateStatement (If cond st) = "if("++(translateRelExp cond)++"){\n"++(translateStatements st)++"}"
    translateStatement (IfEl cond st1 st2) = "if("++(translateRelExp cond)++"){\n"++(translateStatements st1)++"}else{\n"++(translateStatements st2)++"}"
    translateStatement (While cond st) = "while("++(translateRelExp cond)++"){\n"++(translateStatements st)++"}"

    translateStatements :: Statements -> String
    translateStatements (a:b) = (translateStatement a)++"\n"++(translateStatements b)
    translateStatements ([])  = "" 