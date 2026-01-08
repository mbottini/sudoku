module Main where

import System.IO
import System.Environment
import Sudoku

unifyFileInput :: Maybe String -> IO Handle
unifyFileInput Nothing = return stdin
unifyFileInput (Just "-") = return stdin
unifyFileInput (Just path) = openFile path ReadMode

getPath :: IO (Maybe String)
getPath =
    do
        args <- getArgs 
        case args of
            [] -> return Nothing
            (path:_) -> return (Just path)

getHandle :: IO Handle
getHandle = getPath >>= unifyFileInput
    
printPuzzle :: [Mark] -> IO ()
printPuzzle =
    putStrLn . showSolvedPuzzle

main :: IO ()
main = do
    handle <- getHandle
    solutions <- solvePuzzle . lines <$> hGetContents handle
    putStrLn ("There " ++ 
        (if length solutions == 1 then "is " else "are ") ++
        (show . length $ solutions) ++ 
        (if length solutions == 1 then " solution" else " solutions"))
    mapM_ (putStrLn . showSolvedPuzzle) solutions
    hClose handle
