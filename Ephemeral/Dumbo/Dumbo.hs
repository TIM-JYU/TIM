{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections#-}
module Main where
import Data.Monoid
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.List
import Data.Char
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Snap.Core
import Data.Aeson
import Data.Traversable
import Snap.Util.Readable
import Snap.Http.Server
import Text.Blaze.Html.Renderer.Text

convert :: T.Text -> LT.Text
convert = convertBlock
 where 
   htmlOpts = PDC.def{PDC.writerHTMLMathMethod=PDC.MathJax 
               "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"}
   markdownOpts = PDC.def{PDC.writerSetextHeaders=False}
   convertBlock = renderHtml . PDC.writeHtml htmlOpts . PDC.readMarkdown PDC.def . T.unpack
                            
main :: IO ()
main = quickHttpServe . method POST $ do
         bdy <- decode `fmap`  readRequestBody 1000000000 -- Allow up to gigabyte at once..
         writeLBS $ case bdy of
            Nothing   -> encode $ object ["error" .= ("Could not decode input"::T.Text)]
            Just list -> encode . map convert $ list


