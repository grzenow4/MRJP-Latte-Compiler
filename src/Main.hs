module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO

import Latte.ParLatte

import Frontend.TypeChecker (typeCheck)

run :: String -> IO ()
run s = case pProgram (myLexer s) of
    Left e -> hPutStrLn stderr ("ERROR\nParsing error\n" ++ e) >> exitFailure
    Right parsed -> case typeCheck parsed of
        Left err -> hPutStrLn stderr ("ERROR\nType check error\n" ++ show err) >> exitFailure
        Right _ -> hPutStrLn stderr "OK"

main :: IO ()
main = do
    args <- getArgs
    case args of
        fs -> mapM_ (\f -> readFile f >>= run) fs
