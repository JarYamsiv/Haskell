module Dot where
    import Grammar
    dotExp :: Exp -> String
    dotExp (Int v) = show v
    dotExp (Plus e1 e2) = (dotExp e1)++"+"++(dotExp e2)
    dotExp (Minus e1 e2) = (dotExp e1)++"-"++(dotExp e2)
    dotExp (Times e1 e2) = (dotExp e1)++"*"++(dotExp e2)
    dotExp (Div e1 e2) = (dotExp e1)++"/"++(dotExp e2)
    dotExp (Negate e) = "-"++(dotExp e)
    dotExp (Var v)    = v
    dotExp (Let s e1 e2) = ""

    dotRelExp :: RelExp -> String
    dotRelExp (Gt e1 e2) = (dotExp e1)++">"++(dotExp e2)
    dotRelExp (Lt e1 e2) = (dotExp e1)++"<"++(dotExp e2)
    dotRelExp (Gteq e1 e2) = (dotExp e1)++">="++(dotExp e2)
    dotRelExp (Lteq e1 e2) = (dotExp e1)++"<="++(dotExp e2)
    dotRelExp (Eq e1 e2) = (dotExp e1)++"=="++(dotExp e2)
    dotRelExp (Neq e1 e2) = (dotExp e1)++"!="++(dotExp e2)


    --compiling the statement
    --o/p (statementLabel , nodename)
    --i/p (statement,node_count,lst node list)
    dotStatement :: (Statement,Int,[Maybe String]) -> (String,[Maybe String], Int)
    dotStatement ((Assignment v e),node_id,last_node) = 
        let
            nodename = "a"++(show node_id)
            statement = v++"="++(dotExp e)
            makeCon x= case x of
                            Nothing -> ""
                            Just n -> n++"->"++nodename++"\n"
            lastNodeCon = concat (map makeCon last_node)
            node = nodename++"[shape=box label=\""++statement++"\"]\n" ++ lastNodeCon
        in
            (node,[Just nodename] , node_id+1)

    dotStatement ((If cond st),node_id,last_node) = 
        let
            nodename = "i"++(show node_id)
            label = dotRelExp cond

            makeCon x= case x of
                Nothing -> ""
                Just n -> n++"->"++nodename++"\n"
            lastNodeCon = concat (map makeCon last_node)

            (nodesData , node_list , new_id ) = dotStatements (st,node_id+1 ,[Just nodename] )

            node = nodename++"[label=\""++label++"\"]" ++ lastNodeCon ++ nodesData
        in
            (node,[Just nodename]++node_list,new_id)


    -- dotStatement (IfEl cond st1 st2) = 
    --     let
    --         statement = "if("++(dotRelExp cond)++"){\n"++
    --                         (dotStatements (st1,0))++
    --                     "}else{\n"++
    --                         (dotStatements (st2,0))++"}"
    --         nodename = "e"
    --         node = nodename++"[label=\""++statement++"\"]"
    --     in
    --         (node,nodename)
        


    -- dotStatement (While cond st) = 
    --     let
    --         statement = "while("++(dotRelExp cond)++"){\n"++
    --                         (dotStatements (st,0))++"}"
    --         nodename = "e"
    --         node = nodename++"[label=\""++statement++"\"]"
    --     in
    --         (node,nodename)



    --recursivly compiles the statement list

    -- i/p : ( statement list , node count , last node list)
    -- o/p : ( node , last node elist , after node count)
    dotStatements :: (Statements,Int,[Maybe String]) -> (String , [Maybe String] , Int)
    dotStatements ((a:b),t,last) = 
        let
            (cur_statement,node_list , new_node_id) = dotStatement (a,t,last)
            (nex_list , last_node_list , new_id) = (dotStatements (b,new_node_id,node_list))
        in
        (cur_statement++"\n"++nex_list , last_node_list , new_id)

    dotStatements (([]),t,last)  = ("" ,last,t)