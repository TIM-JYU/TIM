{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Choices where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Text.Pandoc
import Text.Blaze.Html.Renderer.Text

import PluginType

data Choice = Choice {text :: T.Text, correct :: Bool, reason :: T.Text} deriving (Show,Generic)
instance ToJSON   Choice where

newtype Blind = Blind T.Text deriving (Show, Generic)

instance ToJSON Blind where
    toJSON (Blind t) = object ["text" .= t]

blind :: MCQMarkup a Choice -> MCQMarkup a Blind
blind MCM{..} = MCM stem (map hide choices) 
hide :: Choice -> Blind
hide = Blind . text

data MC
data MMC
data MCQMarkup mckind choice 
    = MCM {stem    :: T.Text
          ,choices :: [choice]} 
      deriving (Show,Generic)

instance ToJSON a => ToJSON (MCQMarkup x a) where
instance FromJSON a => FromJSON (MCQMarkup x a) where
instance FromJSON Choice where

formatMarkdown :: T.Text -> T.Text
formatMarkdown = LT.toStrict . LT.concatMap escapeQuotes . renderHtml . writeHtml def . readMarkdown def . T.unpack

escapeQuotes '\'' = "&#39"
escapeQuotes '\"' = "&#34"
escapeQuotes x    = LT.singleton x

class Typesettable a where
   typeset :: a -> a

instance Typesettable a => Typesettable (MCQMarkup x a) where
    typeset (MCM stem choices) = MCM (formatMarkdown stem) (map typeset choices)

instance Typesettable Choice where
    typeset (Choice t c r) = Choice (formatMarkdown t) c (formatMarkdown r)

instance Typesettable Blind where
    typeset (Blind t) = Blind (formatMarkdown t)


multipleMultipleChoice :: Plugin (MCQMarkup MMC Choice) (Maybe [Maybe Bool]) [Maybe Bool] Value
multipleMultipleChoice  
   = Plugin{..}
  where 
    requirements = [
                    JS "SimpleDirective.js"  
                   ,JS "script2.js"
                   ,NGModule "MCQ"]
    additionalFiles = ["MMCQTemplate.html"]
    initial = Nothing
    update (mcm,_,i) = return $ TC (Just i) (object ["state".=i,"question".=mcm])
    render (mcm,state) = return . LT.decodeUtf8 $
                        case state of
                             Just i  -> ngDirective "mmcq" 
                                            $ object ["question" .= typeset mcm 
                                                     ,"state"    .= Just i]
                             Nothing -> ngDirective "mmcq"
                                            $ object ["question" .= typeset (blind mcm)
                                                     ,"state"    .= (Nothing :: Maybe ()) ]
    additionalRoutes = noRoutes
                                


simpleMultipleChoice :: Plugin (MCQMarkup MC Choice) (Maybe Integer) Integer Value
simpleMultipleChoice 
   = Plugin{..}
  where 
    requirements = [
                    JS "SimpleDirective.js"  
                   ,JS "script2.js"
                   ,NGModule "MCQ"]
    additionalFiles = ["MCQTemplate.html"]
    initial = Nothing
    update (mcm,_,i) = return $ TC (Just i) (object ["state".=i,"question".=typeset mcm])
    render (mcm,state) = return . LT.decodeUtf8 $
                        case state of
                             Just i  -> ngDirective "mcq" 
                                            $ object ["question" .= typeset mcm 
                                                     ,"state"    .= Just i]
                             Nothing -> ngDirective "mcq"
                                            $ object ["question" .= typeset (blind mcm)
                                                     ,"state"    .= (Nothing :: Maybe ()) ]
    additionalRoutes = noRoutes
                                

testQ :: MCQMarkup MC Choice
testQ = MCM "Valitse kissa" [Choice "Koira" False "Piti valita kissa"
                            ,Choice "Kissa" True  "Kissat Rulez"
                            ,Choice "Kani"  False "Kissa voi syödä kanin"]

testMQ :: MCQMarkup MMC Choice
testMQ = MCM "Valitse eläin" [Choice "Koira" True "Joo"
                            ,Choice "Kissa" True  "On"
                            ,Choice "Perl"  False "Ei oo"]
