{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Snap.Http.Server
import System.Process
import UtilityPrelude 

import PluginType

data Size = Normal | Small | Large deriving (Show,Generic)
instance FromJSON Size where
instance ToJSON   Size where

data GVMarkup = GV {gvType::T.Text, size::Size, gvData::T.Text} deriving (Show,Generic)
instance FromJSON GVMarkup where
instance ToJSON   GVMarkup where

graphViz :: Plugin GVMarkup () ()
graphViz = Plugin{..}
  where 
    requirements = []
    additionalFiles = []
    initial = ()
    update markup _ _ = return []
    render markup _   = case gvType markup of
        "dot"        -> runGV markup "dot"
        "neato"      -> runGV markup "neato"
        "osage"      -> runGV markup "neato"
        "patchwork"  -> runGV markup "patchwork"
    additionalRoutes = noRoutes

    runGV markup prog = inFig markup <$> readProcess prog ["-T","svg"] (T.unpack $ gvData markup)
    inFig markup gvOutput = "<div class='figure graphviz "<>LT.pack (show (size markup))<>"'>"<>LT.pack gvOutput<>"</div>"
                                
main :: IO ()
main = quickHttpServe $ serve graphViz

testGraph :: GVMarkup 
testGraph = GV "dot" Normal
               "digraph text {a->b;b->c;a->c;}"  
