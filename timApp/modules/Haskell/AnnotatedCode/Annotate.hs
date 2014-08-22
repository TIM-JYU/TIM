{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Annotate where

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
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Control.Applicative

import PluginType

import HSMarkup

data AnnotateMarkup = CodeListing {code :: T.Text, tips :: Maybe (HashMap T.Text T.Text)}
                    | Derivation  {code :: T.Text, tips :: Maybe (HashMap T.Text T.Text)}
    deriving (Show,Generic)

parsingOptions = defaultOptions{omitNothingFields=True}
instance FromJSON AnnotateMarkup where
    parseJSON (Object v) = do
        cl <- optional (CodeListing <$> v .: "code"       <*> (optional (v .: "tips")))
        dr <- optional (Derivation  <$> v .: "derivation" <*> (optional (v .: "tips")))
        maybe (fail "No 'code' or 'derivation' key") return (cl<|>dr)
    parseJSON x = empty
instance ToJSON AnnotateMarkup where

annotate :: Plugin (Markup AnnotateMarkup) () ()
annotate = Plugin{..}
  where 
    requirements = [CSS "style.css"] -- [JS "shortNote.js"
                      -- ,NGModule "Note"]
    additionalFiles = [] 
    initial = ()
    update () = return $ () 
    render (Markup markup) = let
                           bytes = T.encodeUtf8 . code $ markup
                           stringify = map (\(a,b) -> (T.unpack a, T.unpack b))
                          in case parseCode bytes of
                              Left err -> return . LT.pack $ "<pre class='error'>Error in Code"<>err<>"</pre>"
                              Right parsed -> return . renderHtml . addWrapper $ renderStructureHTML 
                                                    (stringify . HashMap.toList .fromMaybe mempty. tips $ markup) 
                                                    parsed

    additionalRoutes = noRoutes
    addWrapper x = H.pre H.! A.class_ "annotated" $ x 
                                

exampleAnn :: AnnotateMarkup
exampleAnn = CodeListing "foldl (+) <t1> 0 </t1> [1..10]" $ Just (HashMap.fromList [("t1","This is zero")])
