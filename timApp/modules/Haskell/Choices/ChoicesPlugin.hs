{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Snap.Http.Server

import PluginType
import Choice
                                
main :: IO ()
main = quickHttpServe $ serve simpleMultipleChoice

testQ :: MCQMarkup Choice
testQ = MCM "ekatehtävä" "Valitse kissa" [Choice "Koira" False "Piti valita kissa"
                                         ,Choice "Kissa" True  "Kissat Rulez"
                                         ,Choice "Kani"  False "Kissa voi syödä kanin"]
