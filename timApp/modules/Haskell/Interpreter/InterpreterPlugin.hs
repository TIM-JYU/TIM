{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Snap.Http.Server
import Control.Monad.Trans.Either

import PluginType
import CallApiHec
import HecIFace as H

data InterpreterMarkup  = I {context::FilePath} deriving (Show,Generic)
data InterpreterCommand = Evaluate String deriving (Show,Generic)
instance FromJSON InterpreterMarkup where
instance ToJSON   InterpreterMarkup where
instance FromJSON InterpreterCommand where
instance ToJSON   InterpreterCommand where

mkInterpreter :: IO (Plugin InterpreterMarkup () InterpreterCommand)
mkInterpreter = do
    evaluator <- eitherT error return findApiHec
    let
        requirements = [JS "NewConsole/Console.js"
                       ,JS "https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular-sanitize.min.js"
                       ,CSS "NewConsole/style.css"
                       ,NGModule "console"]
        additionalFiles = ["NewConsole/Console.template.html"]
        initial = ()
        update markup _ (Evaluate expr) = do
                             res <- callApiHec evaluator $ Input (Eval expr) (Main.context markup)
                             case res of
                                H.Plain s  -> return $ [web "reply" $ "<span>"++s++"</span>"] -- TODO: SANITIZE!!
                                H.Html  s  -> return $ [web "reply" $ s] -- TODO: SANITIZE!!
                                H.Error ss -> return $ [web "reply" $ "<pre class='error'>"++unlines ss++"</pre>"] -- TODO: SANITIZE!! 
                                H.NonTermination -> return $ [web "reply" ("<span class='warning'>Your expression is diverging (ie. does not terminate)</span>"::String)] 
                                H.TimeOut -> return $ [web "reply" ("<span class='warning'>Your expression did not terminate on time</span>"::String)] 
                                H.ProtocolError e -> return $ [web "reply" $ "<span class='warning'>Interactive interpreter is broken. Please report this to a human:"++e++"</span>"]  -- TODO: SANITIZE!!
        render markup state = return . LT.decodeUtf8 $ ngDirective "console" ()
        additionalRoutes = noRoutes
    return Plugin{..}
                                
main :: IO ()
main = mkInterpreter >>= quickHttpServe . serve 

testInterp :: InterpreterMarkup 
testInterp = I "" 
