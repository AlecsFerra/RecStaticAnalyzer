{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Analysis.Analysis (run)
import Domain.StrictnessResults (makeResults)
import Control.Exception (IOException, catch)
import Language.Check (checkProgram)
import Language.Compile (compile)
import Parsing.Parser (parse)
import Pretty (prettyCheckError, prettyParseError, prettyStrictnessResult)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Prelude hiding (error, fail)
import Domain.Strictness (strictnessValueSemantics)

error :: Bool -> String -> IO ()
error True msg = fail msg
error False _ = pure ()

fail :: String -> IO a
fail msg = putStrLn msg >> exitFailure

main :: IO ()
main = do
  programName <- getProgName
  args <- getArgs
  error (length args /= 1) $ printf "Usage: %s source.rec" programName

  let sourceName = head args
  source <- readFile sourceName `catch` const @_ @IOException (fail $ printf "Source file '%s' not found" sourceName)

  parsed <- either (fail . prettyParseError sourceName) pure $ parse source

  let compiled = compile parsed

  either (fail . prettyCheckError sourceName) pure $ checkProgram compiled

  putStrLn $ prettyStrictnessResult $ makeResults $ run strictnessValueSemantics compiled
