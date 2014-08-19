{-#LANGUAGE RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Interpreter where
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson.Types

import PluginType
import CallApiHec
import HecIFace as H

data Example = Example {title :: Maybe String
                       ,expr  :: String
                       } 
                       deriving (Show,Generic)
data Goal = Goal {cond::String,reply::String} deriving(Show,Generic)
data InterpreterMarkup  = I {context::Maybe FilePath,examples::Maybe [Example],goals::Maybe [Goal]} deriving (Show,Generic)
data InterpreterCommand = Evaluate String deriving (Show,Generic)

parsingOptions = defaultOptions{omitNothingFields=True}
instance FromJSON Goal where
instance ToJSON   Goal where
instance FromJSON Example where
    parseJSON = genericParseJSON parsingOptions
instance ToJSON   Example where
instance FromJSON InterpreterMarkup where
    parseJSON = genericParseJSON parsingOptions
instance ToJSON   InterpreterMarkup where
instance FromJSON InterpreterCommand where
instance ToJSON   InterpreterCommand where

mkInterpreter :: IO (Plugin InterpreterMarkup () InterpreterCommand String)
mkInterpreter = do
    evaluator <- eitherT error return findApiHec
    let
        requirements = [JS "NewConsole/Console.js"
                       ,JS "https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular-sanitize.min.js"
                       ,CSS "NewConsole/style.css"
                       ,NGModule "console"]
        additionalFiles = ["NewConsole/Console.template.html"]
        initial = ()
        update (markup,_,Evaluate expr) = do
             let ctx = maybe (StringContext "") FileContext (Interpreter.context markup)
             res <- if ":t" `isPrefixOf` expr 
                     then callApiHec evaluator $ Input (TypeOf $ drop 2 expr) ctx
                     else callApiHec evaluator $ Input (Eval expr)            ctx
             let reply x = return $ TC () x 
             case res of
                H.Plain s  -> do
                            chkRes <- listToMaybe . catMaybes <$> mapM (check ctx expr) (fromMaybe [] $ goals markup)
                            reply $  "<span>"++s++"</span>"++fromMaybe "" chkRes -- TODO: SANITIZE s!!
                H.Html  s  -> do
                            chkRes <- listToMaybe . catMaybes <$> mapM (check ctx expr) (fromMaybe [] $ goals markup)
                            reply $ s++fromMaybe "" chkRes -- TODO: SANITIZE s!!
                H.Error ss -> reply $  "<pre class='error'>"++unlines ss++"</pre>" -- TODO: SANITIZE ss!! 
                H.NonTermination -> reply ("<span class='warning'>Your expression is diverging (ie. does not terminate)</span>"::String)
                H.TimeOut -> reply $ ("<span class='warning'>Your expression did not terminate on time</span>"::String)
                H.ProtocolError e -> reply $ "<span class='warning'>Interactive interpreter is broken. Please report this to a human:"++e++"</span>"  -- TODO: SANITIZE e!!

             
        check ctx expr (Goal cond desc) = do
                    res <- callApiHec evaluator $ Input (Check cond expr) ctx
                    case res of
                        CheckResult True  -> return . Just $ "<span class='success'>"++desc++"</span>"
                        CheckResult False -> return Nothing
                        x                 -> print ("ERROR"++ show x) >> return Nothing -- TODO: Log an error!
        render (markup,state) = return . ngDirective "console" . object $
                                    ["examples" .= examples markup]
        additionalRoutes = noRoutes
    return Plugin{..}
