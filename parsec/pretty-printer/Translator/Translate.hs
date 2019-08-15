module Translator.Translate where
    import Parser.Grammar

    spaces :: Int -> String
    spaces x = replicate (2*x) ' ' --tabs are considered as two spaces


    translateExp :: Exp -> Int -> String
    translateExp (Int v)            t = show v
    translateExp (Plus e1 e2)       t = (translateExp e1 t)++"+"++(translateExp e2 t)
    translateExp (Minus e1 e2)      t = (translateExp e1 t)++"-"++(translateExp e2 t)
    translateExp (Times e1 e2)      t = (translateExp e1 t)++"*"++(translateExp e2 t)
    translateExp (Div e1 e2)        t = (translateExp e1 t)++"/"++(translateExp e2 t)
    translateExp (Negate e)         t = "-"++(translateExp e t)
    translateExp (Var v)            t = v
    translateExp (Let s e1 e2)      t = ""

    translateStatement :: Statement -> Int ->String
    translateStatement (Assignment v e) t   = (spaces t)++ v++"="++(translateExp e t)

    translateStatement (If cond st) t       = (spaces t)++ "if("++cond++"){\n"
        ++(translateStatements st (t+1))++(spaces t)++"}\n"

    translateStatement (IfEl cond st1 st2) t = (spaces t)++ "if("++cond++"){\n"
        ++(translateStatements st1 (t+1))++(spaces t)++"}\n"
        ++(spaces t)++"else{\n"++(translateStatements st2 (t+1))++(spaces t)++"}\n"

    translateStatement (While cond st) t= (spaces t)++"while("++cond++"){\n"
        ++(translateStatements st (t+1))++(spaces t)++"}\n"


    translateStatements :: Statements ->Int -> String
    translateStatements (a:b) t = (translateStatement a t)++"\n"++(translateStatements b t)
    translateStatements ([]) t  = "" 