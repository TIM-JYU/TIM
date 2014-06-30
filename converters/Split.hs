module Main where
import Text.Pandoc.Walk
import Text.Pandoc.Shared
import Text.Pandoc.JSON
import Text.Pandoc
import Data.List
import Data.List.Split
import Data.Char (isSpace)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Shared
import Text.Blaze.Html.Renderer.String
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Text.Printf

main = do
    (Pandoc meta blocks) <- readMarkdown def <$> getContents     
    writeFile "meta" . writeMarkdown def . metaToPandoc $ meta
    sequence_ 
      [writeFile (printf "block_%05d" (i::Int)) . writeMarkdown def . blockToPandoc $ b
      | (b,i) <- zip blocks [1..]]

blockToPandoc :: Block -> Pandoc
blockToPandoc b = Pandoc mempty [b]

metaToPandoc :: Meta -> Pandoc
metaToPandoc b = Pandoc b []
