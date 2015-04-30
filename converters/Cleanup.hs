{-#LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, ViewPatterns#-}
module Main where
import Text.Pandoc.Walk
import Text.Pandoc.Shared
import Text.Pandoc.JSON
import Data.List
import Data.List.Split
import Data.Char (isSpace)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Shared
import Text.Blaze.Html.Renderer.String
import Data.Monoid
import Data.Maybe
import Control.Applicative

main = toJSONFilter (\x -> walk remLinks $ walk joinImgs (x::Pandoc))


toString :: [Inline] -> String
toString = stringify . Plain

remLinks :: Inline -> Inline
remLinks (Link (toString -> "#") _) = Space
remLinks (Link (toString -> "") _) = Space
remLinks x = x

joinImgs :: Block -> Block

joinImgs (Para ((Image d t):des@((dropWhile isSpace . stringify) -> k)))
     | "Kuva " `isPrefixOf` k = Para [Image des t]
joinImgs x = x
