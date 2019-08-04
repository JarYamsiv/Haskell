module Translator.Translate where
	import Parser.Grammar
	translateExp :: Exp -> String
	translateExp (Int v) = show v
	translateExp (Plus e1 e2) = (translateExp e1)++"+"++(translateExp e2)
	translateExp (Minus e1 e2) = (translateExp e1)++"-"++(translateExp e2)
	translateExp (Times e1 e2) = (translateExp e1)++"*"++(translateExp e2)
	translateExp (Div e1 e2) = (translateExp e1)++"/"++(translateExp e2)
	translateExp (Negate e) = "-"++(translateExp e)
	translateExp (Var v)    = v
	translateExp (Let s e1 e2) = ""

	translateStatement :: Statement -> String
	translateStatement (Assignment v e) = v++"="++(translateExp e)
	translateStatement (If cond st) = "if("++cond++"){\n"++(translateStatements st)++"\n}"

	translateStatements :: Statements -> String
	translateStatements (a:b) = (translateStatement a)++"\n"++(translateStatements b)
	translateStatements ([])  = "" 