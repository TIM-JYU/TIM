{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DeriveGeneric#-}
module Main where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Snap.Http.Server
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.Either

import PluginType
import CallApiHec
import HecIFace as H

data Example = Example {title :: Maybe String
                       ,expr::String
                       } 
                       deriving (Show,Generic)
data InterpreterMarkup  = I {context::FilePath,examples::[Example],goals::[(String,String)]} deriving (Show,Generic)
data InterpreterCommand = Evaluate String deriving (Show,Generic)
instance FromJSON Example where
instance ToJSON   Example where
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
             let ctx = Main.context markup
             res <- if ":t" `isPrefixOf` expr 
                     then callApiHec evaluator $ Input (TypeOf $ drop 2 expr) ctx
                     else callApiHec evaluator $ Input (Eval expr) ctx
             case res of
                H.Plain s  -> do
                            chkRes <- listToMaybe . catMaybes <$> mapM (check ctx expr) (goals markup)
                            return $ [web "reply" $ "<span>"++s++"</span>"++fromMaybe "" chkRes] -- TODO: SANITIZE s!!
                H.Html  s  -> do
                            chkRes <- listToMaybe . catMaybes <$> mapM (check ctx expr) (goals markup)
                            return $ [web "reply" $ s++fromMaybe "" chkRes] -- TODO: SANITIZE s!!
                H.Error ss -> return $ [web "reply" $ "<pre class='error'>"++unlines ss++"</pre>"] -- TODO: SANITIZE ss!! 
                H.NonTermination -> return $ [web "reply" ("<span class='warning'>Your expression is diverging (ie. does not terminate)</span>"::String)] 
                H.TimeOut -> return $ [web "reply" ("<span class='warning'>Your expression did not terminate on time</span>"::String)] 
                H.ProtocolError e -> return $ [web "reply" $ "<span class='warning'>Interactive interpreter is broken. Please report this to a human:"++e++"</span>"]  -- TODO: SANITIZE e!!

             
        check ctx expr (cond,desc) = do
                    res <- callApiHec evaluator $ Input (Check cond expr) ctx
                    case res of
                        CheckResult True  -> return . Just $ "<span class='success'>"++desc++"</span>"
                        CheckResult False -> return Nothing
                        x                 -> print ("ERROR"++ show x) >> return Nothing -- TODO: Log an error!
        render markup state = return . LT.decodeUtf8 . ngDirective "console" . object $
                                    ["examples" .= examples markup]
        additionalRoutes = noRoutes
    return Plugin{..}
                                
main :: IO ()
main = mkInterpreter >>= quickHttpServe . serve 

testInterp :: InterpreterMarkup 
testInterp = I "" [Example (Just "This is fun") "take 10 [1..]"
                  ,Example Nothing "reverse . reverse"]
                  [("(==11)","Great, that's eleven")]
