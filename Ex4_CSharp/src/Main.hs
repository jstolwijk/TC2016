module Main where

import Prelude hiding ((<*>), (<$>), (<$), (<*), (*>))

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode

start :: Parser s a -> [s] -> a
start p = fst . head . filter (null . snd) . parse p

main :: IO ()
main = do
         -- get command line arguments
         args  <- getArgs
         -- compute a list of input an output files
         files <- case args of
                    []  ->  do
                              putStrLn "no argument given; assuming example.cs"
                              return [("example.cs", "example.ssm")]
                    xs  ->  return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
         -- translate each of the files
         mapM_ processFile files

-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode
                . foldCSharp codeAlgebra
                . start (pClass <* eof)
                . start lexicalScanner


testLex infile =   do
              xs <- readFile infile
              putStrLn (show (process xs))
  where process = start lexicalScanner