{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Analysis.Analysis (run)
import Control.Exception (IOException, catch)
import Domain.Sign (signValueSemantics)
import Domain.SignResults (makeSignResults)
import Domain.Strictness (strictnessValueSemantics)
import Domain.StrictnessResults (makeStrictnessResults)
import Language.Check (checkProgram)
import Language.Compile (compile)
import Parsing.Parser (parse)
import Pretty (
  prettyCheckError,
  prettyParseError,
  prettySignResult,
  prettyStrictnessResult,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Prelude hiding (error, fail)

error :: Bool -> String -> IO ()
error True msg = fail msg
error False _ = pure ()

fail :: String -> IO a
fail msg = putStrLn msg >> exitFailure

data Analysis = Strictness | Sign

parseMode :: String -> IO Analysis
parseMode "--strictness" = pure Strictness
parseMode "--sign" = pure Sign
parseMode m = fail $ printf "Unrecognized execution flag '%s'" m

main :: IO ()
main = do
  programName <- getProgName
  args <- getArgs
  error (length args /= 2) $ printf "Usage: %s source.rec (--strictness|--sign)" programName

  let sourceName = head args
  source <- readFile sourceName `catch` const @_ @IOException (fail $ printf "Source file '%s' not found" sourceName)

  mode <- parseMode $ head $ tail args

  parsed <- either (fail . prettyParseError sourceName) pure $ parse source

  let compiled = compile parsed

  either (fail . prettyCheckError sourceName) pure $ checkProgram compiled

  let out = case mode of
        Strictness -> prettyStrictnessResult $ makeStrictnessResults $ run strictnessValueSemantics compiled
        Sign -> prettySignResult $ makeSignResults $ run signValueSemantics compiled

  putStrLn out
