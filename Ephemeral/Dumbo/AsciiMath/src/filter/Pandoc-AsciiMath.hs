import AsciiMath (run)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter asciimath
    where asciimath (Math t s) = Math t $ run s
          asciimath x = x
