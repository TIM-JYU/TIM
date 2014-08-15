{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Choices where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT

import PluginType

data Choice = Choice {text :: T.Text, correct :: Bool, reason :: T.Text} deriving (Show,Generic)
instance ToJSON   Choice where

newtype Blind = Blind T.Text deriving (Show, Generic)

instance ToJSON Blind where
    toJSON (Blind t) = object ["text" .= t]

blind :: MCQMarkup Choice -> MCQMarkup Blind
blind MCM{..} = MCM stem (map hide choices) 
hide :: Choice -> Blind
hide = Blind . text

data MCQMarkup choice 
    = MCM {stem    :: T.Text
          ,choices :: [choice]} 
      deriving (Show,Generic)

instance ToJSON a => ToJSON (MCQMarkup a) where
instance ToJSON (MCQState) where
instance FromJSON Choice where
instance FromJSON a => FromJSON (MCQMarkup a) where
instance FromJSON MCQState where


data MCQState = Revealed Integer | Unchecked deriving (Eq,Show,Generic)

simpleMultipleChoice :: Plugin (MCQMarkup Choice) (Maybe Integer) Integer Value
simpleMultipleChoice 
   = Plugin{..}
  where 
    requirements = [JS "script.js"
                  ,NGModule "MCQ"]
    additionalFiles = ["MCQTemplate.html"]
    initial = Nothing
    update (mcm,_,i) = return $ TC (Just i) (object ["state".=i,"question".=mcm])
    render (mcm,state) = return . LT.decodeUtf8 $
                        case state of
                             Just i  -> ngDirective "mcq" 
                                            $ object ["question" .= mcm 
                                                     ,"state"    .= Just i]
                             Nothing -> ngDirective "mcq"
                                            $ object ["question" .= blind mcm 
                                                     ,"state"    .= (Nothing :: Maybe ()) ]
    additionalRoutes = noRoutes
                                

testQ :: MCQMarkup Choice
testQ = MCM "Valitse kissa" [Choice "Koira" False "Piti valita kissa"
                            ,Choice "Kissa" True  "Kissat Rulez"
                            ,Choice "Kani"  False "Kissa voi syödä kanin"]
