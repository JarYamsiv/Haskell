module Main where
import Grammar
import Tokens
import System.Environment
import System.IO  
import Control.Monad

type Env = String -> Exp
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

eval :: Exp -> Env -> Int
eval (Int v) _         = v
eval (Plus e1 e2) env  = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env   = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env    = -(eval e env)
eval (Var s) env       = eval (envLookup s env) env
eval (Let s e1 e2) env = eval e2 env'
    where env' = envBind s e1 env
    
run :: Exp -> Int
run e = eval e emptyEnv


main :: IO ()
main = do
    -- I can make some changes here to make it read from command line (made)

    let readContent = do
        args <- getArgs
        if (length args) == 0 
            then do --It doesn't require a do if it's only one statement
                contents <- getContents
                return contents
            else do
                handle <- openFile (head args) ReadMode
                contents <- hGetContents handle
                return contents


    -- let a = if (length args) == 0 
    --     then
    --         do
    --             contents <- getContents
    --             parse contents
    --     else
    --         do
    --             contents <- getContents
    --             parse contents
    -- print "end"

    -- print a

    -- handle <- openFile (head args) ReadMode
    -- contents <- hGetContents handle

    contents <- readContent

    let ast = parseCalc (scanTokens contents)
    print ast
    print (run ast)


    