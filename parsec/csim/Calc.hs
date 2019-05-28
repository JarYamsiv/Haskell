module Main where
import Grammar
import Tokens
import System.Environment
import System.IO  
import Control.Monad
-- import Translate
import Dot
 


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

    let (compiled,_,_) = (dotStatements (ast,0,[Nothing]))
    putStr ("digraph{\n"++compiled++"\n}")
    -- print (run ast)


    