module AsciiMath (readAscii, writeTeX, compile, run, LexicalError(..)) where
import Control.Exception (throw)
import Lexer (get_tokens, LexicalError(..))
import Parser (parseAscii)
import Passes (matrix)
import TeXWriter (writeTeX)
import Ast

readAscii :: String -> Either LexicalError Code
readAscii s = return . matrix =<< parseAscii =<< get_tokens s
  
compile :: String -> Either LexicalError String
compile s = fmap writeTeX $ readAscii s

run :: String -> String
run s = case compile s of
  Right txt -> txt
  Left e -> throw e
