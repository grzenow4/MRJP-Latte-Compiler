module Main where

import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.IO as T (writeFile)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, replaceExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (system)

import Backend.Compiler (compile)
import Frontend.TypeChecker (typeCheck)
import Latte.ParLatte

run :: String -> IO Builder
run s = case pProgram (myLexer s) of
    Left e -> hPutStrLn stderr ("ERROR\nParsing error\n" ++ e) >> exitFailure
    Right parsed -> case typeCheck parsed of
        Left err -> hPutStrLn stderr ("ERROR\nType check error\n" ++ show err) >> exitFailure
        Right _ -> hPutStrLn stderr "OK" >> compile parsed

main :: IO ()
main = do
    args <- getArgs
    mapM_
        ( \file -> do
            output <- readFile file >>= run
            let outputFile = replaceExtension file ".s"
            let objFile = replaceExtension file ".o"
            let execFile = dropExtension file
            T.writeFile outputFile (toLazyText output)
            system $ "nasm -f elf64 " ++ outputFile
            system $ "gcc -m64 -no-pie lib/runtime.o -o " ++ execFile ++ " " ++ objFile
        )
        args
