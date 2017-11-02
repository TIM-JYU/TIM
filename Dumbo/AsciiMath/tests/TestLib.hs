module TestLib (mkTestSuite, run, (<$>)) where
import Test.HUnit
import AsciiMath (compile, LexicalError(..))
import Prelude hiding ((<$>))
import Control.Applicative ((<$>))

unComment :: [String] -> [String]
unComment [] = []
unComment ("":ss) = "" : unComment ss
unComment (('#':_):ss) = unComment ss
unComment (s:ss) = s : unComment ss

readSrc :: String -> IO [String]
readSrc = (unComment . lines <$>) . readFile . ("tests/spec/"++)

mkTest :: String -> String -> Test
mkTest inp out = case compile inp of
  Right s -> TestCase $ assertEqual ("for " ++ inp ++ ",") out s
  Left (LexicalError msg _) ->
    TestCase $ assertFailure $ "Error while compiling \"" ++ inp ++ "\" : "
      ++ msg ++ ".\n"

mkTestSuite :: String -> String -> IO Test
mkTestSuite name filename = do {
  inp <- readSrc $ filename ++ ".txt";
  out <- readSrc $ filename ++ ".latex";
  return $ (TestLabel name) . TestList $ zipWith mkTest inp out
  }

run :: Test -> IO ()
run t = do {
  c <- runTestTT t;
  if (errors c == 0 && failures c == 0)
  then return ()
  else error "fail";
}

