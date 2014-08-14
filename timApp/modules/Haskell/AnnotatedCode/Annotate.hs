{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Maybe
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html.Renderer.Text
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import PluginType

import HSMarkup

data AnnotateMarkup = AM {code :: T.Text, tips :: Maybe (HashMap T.Text T.Text)}
    deriving (Show,Generic)

parsingOptions = defaultOptions{omitNothingFields=True}
instance FromJSON AnnotateMarkup where
    parseJSON = genericParseJSON parsingOptions

annotate :: Plugin AnnotateMarkup () () ()
annotate = Plugin{..}
  where 
    requirements = [CSS "style.css"] -- [JS "shortNote.js"
                      -- ,NGModule "Note"]
    additionalFiles = [] 
    initial = ()
    update _ = return $ TC () ()
    render (markup,()) = let
                           bytes = T.encodeUtf8 . code $ markup
                           stringify = map (\(a,b) -> (T.unpack a, T.unpack b))
                          in case parseCode bytes of
                              Left err -> return . LT.pack $ "<pre class='error'>Error in Code"<>err<>"</pre>"
                              Right parsed -> return . renderHtml $ renderStructureHTML 
                                                    (stringify . HashMap.toList .fromMaybe mempty. tips $ markup) 
                                                    parsed

    additionalRoutes = noRoutes
                                

exampleAnn :: AnnotateMarkup
exampleAnn = AM "foldl (+) <t1> 0 </t1> [1..10]" $ Just (HashMap.fromList [("t1","This is zero")])
