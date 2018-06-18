{-# LANGUAGE DeriveDataTypeable #-}
module Exception (AsciimathException(..), Position(..), printAndExit, renderError) where

import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hFlush, hPutStrLn, stderr)


import Control.Exception (Exception)
import Data.Data

-- Location
-- abs, line, column, length
data Position =
  Position !Int !Int !Int
  | PositionElement !Int !Int !Int !Int
  deriving (Data, Show, Eq)

data AsciimathException =
    LexicalError String Position
  | ErrorAndSource AsciimathException String
  deriving (Show, Typeable)

instance Exception AsciimathException

renderError :: AsciimathException -> String
renderError (LexicalError msg (PositionElement _ l c len)) =
  "Line " ++ show l ++
  ", characters " ++ show c ++ "-" ++ show (c + len) ++ ":\n" ++
  "lexical error near: \"" ++ msg ++ "\""
renderError (LexicalError msg (Position _ l c)) =
  "Line " ++ show l ++
  ", characters " ++ show c ++ "-" ++ show (c + 1) ++ ":\n" ++
  "lexical error near: " ++ msg
renderError (ErrorAndSource exn src) =
  "In expression:\n" ++ src ++ "\n" ++
  renderError exn

printAndExit :: AsciimathException -> IO ()
printAndExit e = do
  hPutStrLn stderr $ renderError e
  hFlush stderr
  exitWith (ExitFailure 1)
