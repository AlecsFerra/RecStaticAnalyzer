{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Analysis.Naive (eval)
import Control.Exception (IOException, catch)
import Language.Compile (compile)
import Parsing.Parser (ParseError (..), parse)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Prelude hiding (error, fail)
import Analysis.StrictnessResults (pretty)

error :: Bool -> String -> IO ()
error True msg = fail msg
error False _ = pure ()

fail :: String -> IO a
fail msg = putStrLn msg >> exitFailure

prettyParseError :: String -> ParseError -> String
prettyParseError fileName error = printf "Error while parsing '%s': %s" fileName $ pretty error
  where
    pretty (UnexpectedCharacter c) = printf "Unexpected '%c'" c
    pretty (UnexpectedToken t) = printf "Unexpected '%s'" t
    pretty UnexpectedEOF = "Unexpected end of file"

main :: IO ()
main = do
    programName <- getProgName
    args <- getArgs
    error (length args /= 1) $ printf "Usage: %s source.rec" programName

    let sourceName = head args
    source <- readFile sourceName `catch` const @_ @IOException (fail $ printf "Source file '%s' not found" sourceName)

    parsed <- either (fail . prettyParseError sourceName) pure $ parse source

    let compiled = compile parsed
    putStrLn $ pretty $ eval compiled
