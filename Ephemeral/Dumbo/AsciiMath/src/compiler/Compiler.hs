module Main (main) where
import AsciiMath (run)

main :: IO ()
main = interact $ (++"\n") . run
