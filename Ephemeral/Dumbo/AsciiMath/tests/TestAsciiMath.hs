module Main ( main ) where
import TestLib
import Test.HUnit

tests :: IO Test
tests = TestList <$> sequence [
  mkTestSuite "Operation symbols" "op-sym",
  mkTestSuite "Miscellaneous symbols" "misc-sym",
  mkTestSuite "Relation symbols" "rel-sym",
  mkTestSuite "Greek letters" "greek",
  mkTestSuite "Logical symbols" "logical-sym",
  mkTestSuite "Brackets" "brackets", 
  mkTestSuite "Arrows" "arrows",
  mkTestSuite "Accents" "accents",
  mkTestSuite "Font commands" "fonts",
  mkTestSuite "Matrices" "matrices",
  mkTestSuite "Standard functions" "std-fun"]

main :: IO ()
main = run =<< tests
