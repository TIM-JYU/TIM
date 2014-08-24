{-#LANGUAGE RecordWildCards, DeriveGeneric, OverloadedStrings, NoMonomorphismRestriction #-}
module Interpreter where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson.Types

import PluginType hiding (Input)
import qualified PluginType as PT
import CallApiHec
import HecIFace as H
import           Data.Configurator       (Worth(..), load, require)

data Example = Example {title :: Maybe String
                       ,expr  :: String
                       } 
                       deriving (Show,Generic)
data Goal = Goal {cond::String
                 ,reply::String
                 ,tag::T.Text} deriving(Show,Generic)
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

mkInterpreter :: IO (Plugin (PT.Markup InterpreterMarkup) 
                            (PT.Markup InterpreterMarkup,PT.Input InterpreterCommand) 
                            (PT.Web String,PT.BlackboardOut))
mkInterpreter = do
    conf   <- load [Required "Interpreter.conf"]
    contextPath <- require conf "contextpath" 
    evaluator <- eitherT error return findApiHec
    let
        requirements = [JS "NewConsole/Console.js"
                       ,JS "https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular-sanitize.min.js"
                       ,CSS "NewConsole/style.css"
                       ,NGModule "console"]
        additionalFiles = ["NewConsole/Console.template.html"]
        update (PT.Markup markup, PT.Input (Evaluate expr)) = do
             let ctx = maybe (StringContext "") (FileContext.((contextPath++"/")++)) (Interpreter.context markup)
             print ("Context:",ctx)
             res <- if ":t" `isPrefixOf` expr 
                     then callApiHec evaluator $ Input (TypeOf $ drop 2 expr) ctx
                     else callApiHec evaluator $ Input (Eval expr)            ctx
             let  reply :: [BlackboardCommand] -> String -> IO (PT.Web String, PT.BlackboardOut)
                  reply ts x = return $ (Web x,BlackboardOut ts)
             case res of
                H.Plain s  -> do
                            (chkTags,chkOut) <- mconcat . catMaybes <$> mapM (check ctx expr) (fromMaybe [] $ goals markup)
                            reply chkTags $  "<span>"++s++"</span>"++chkOut 
                            -- TODO: SANITIZE s!!
                H.Html  s  -> do
                            (chkTags,chkOut) <- mconcat . catMaybes <$> mapM (check ctx expr) (fromMaybe [] $ goals markup)
                            reply chkTags $ s++chkOut -- TODO: SANITIZE s!!
                H.Error ss -> reply [] $  "<pre class='error'>"++unlines ss++"</pre>" -- TODO: SANITIZE ss!! 
                H.NonTermination -> reply [] ("<span class='warning'>Your expression is diverging (ie. does not terminate)</span>"::String)
                H.TimeOut -> reply []  ("<span class='warning'>Your expression did not terminate on time</span>"::String)
                H.ProtocolError e -> reply [] $ "<span class='warning'>Interactive interpreter is broken. Please report this to a human:"++e++"</span>"  -- TODO: SANITIZE e!!

             
        check ctx expr (Goal cond desc tag) = do
                    res <- callApiHec evaluator $ Input (Check cond expr) ctx
                    case res of
                        CheckResult True  -> return (Just ([Put tag], "<span class='success'>"++desc++"</span>"))
                        CheckResult False -> return Nothing
                        x                 -> print ("ERROR"++ show x) >> return Nothing -- TODO: Log an error!

        render (Markup markup) = return . ngDirective "console" . object $ ["examples" .= examples markup]
        additionalRoutes = noRoutes
    return Plugin{..}
