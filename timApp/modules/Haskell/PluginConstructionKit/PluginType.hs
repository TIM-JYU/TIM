{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables#-}
module PluginType where

import Data.Aeson
import GHC.Generics

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Template as TMPL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap,(!))
import Data.IORef

import Snap.Core
import Snap.Http.Server
import Snap.Util.Readable
import Snap.Util.FileServe

import qualified AnswerRequest as A
import qualified HTMLRequest   as H

import System.Directory
import UtilityPrelude

-- | Simplified interface for TIM plugins
-- type Plugin stucture state input output = PrimPlugin (structure,state) (structure,state,input) (TIMCmd state output)

data Plugin structure state input output = Plugin 
        { initial :: state
        , render  :: (structure, state) -> IO LT.Text
        , update  :: (structure, state, input) -> IO (TIMCmd state output)
        , requirements :: [Requirement]
        , additionalFiles :: [FilePath]
        , additionalRoutes :: Snap ()}

-- | Simplified interface for ordering TIM to modify its data
data TIMCmd save web = TC {save :: save, web :: web}
            deriving (Show, Generic)

ngDirective :: ToJSON a => LT.Text -> a -> LT.Text
ngDirective tag content = "<"<>tag<>" data-content='"
                             <>escape (LT.decodeUtf8 (encode content))
                             <>"'></"<>tag<>">"

escape :: LT.Text -> LT.Text
escape = LT.concatMap esc
 where
  esc '\'' = "&#39;"
  esc '\"' = "&#34;"
  esc x    = LT.singleton x

noRoutes :: Snap ()    
noRoutes = return ()

--            --
-- - Server - --
--            --

-- | Various requirements that the plugin might have.
data Requirement = JS T.Text
                 | CSS T.Text
                 | NGModule T.Text
                 deriving (Show,Generic) 

instance ToJSON Requirement where
    toJSON (JS  t)      = object ["js"  .= t]
    toJSON (CSS t)      = object ["css" .= t]
    toJSON (NGModule t) = object ["angularModule" .= t]

-- | Serve a plugin
serve :: (MonadSnap m, MonadIO m, FromJSON structure, FromJSON state, FromJSON input, ToJSON state, ToJSON output) => Plugin structure state input output -> m ()
serve plugin = route  
        [
        ("html/", method POST $ do
            req <- getBody
            writeLazyText =<< liftIO (render plugin (H.markup req, fromMaybe (initial plugin) (H.state req)))
        ),
        ("reqs/", method GET $ do
            writeLBS . encode $ 
                object [
                        "js"  .= [x | JS  x <- requirements plugin]
                       ,"css" .= [x | CSS x <- requirements plugin]
                       ,"angularModule" .= [x | NGModule x <- requirements plugin]
                       ,"type" .= ("embedded"::T.Text)
                       ]
--       Sample
--       {"js" : ["munSkripti1.js", "http://path/to/js.js"],
--       "css":["munStyylit.css", "http://path/to/css.css"],
--       "angularModule" : ["mun-moduli1", "mun-moduli2"],
--       "type": "embedded"}` 
        ),
        ("answer/", method PUT $ do
           req  <- getBody
           tims <- liftIO (update plugin (A.markup req,fromMaybe (initial plugin) (A.state req), A.input req))
           writeLBS . encode $ object ["web" .= web tims, "save" .= save tims] -- , "save-markup" .= markup ] 
        )
        ]
        <|> serveStaticFiles "." plugin
    where 

serveStaticFiles from plugin = do
        locals <- liftIO $ filterM doesFileExist $ [from++"/"++T.unpack file | CSS file <- requirements plugin]
                                                ++ [from++"/"++T.unpack file | JS  file <- requirements plugin]
        rq <- getSafePath
        when (rq`elem`locals) (serveFile rq)
        when (rq`elem`additionalFiles plugin) (serveFile rq)

experiment :: forall structure markup state input output. 
                (FromJSON structure, FromJSON state, ToJSON output, FromJSON input) => 
                    Plugin structure state input output -> structure -> Int -> IO ()
experiment plugin markup' port = do 
    state  <- newIORef (initial plugin)
    markup <- newIORef markup'
    let context "port"   = pure (T.pack $ show port)
        context "plugin" = do
                            m <- readIORef markup 
                            s <-  readIORef state
                            LT.toStrict <$> render plugin (m,s)
        context "moduleDeps" = pure . T.pack . show $ [x | NGModule x <- requirements plugin]
        context "scripts"    = pure . T.unlines 
                                        $ ["<script src='"<>x<>"'></script>" 
                                          | JS x <- requirements plugin]
        context "styles"       = pure . T.unlines 
                                        $ ["<link rel='stylesheet' type='text/css' href='"<>x<>"'>" 
                                          | CSS x <- requirements plugin]
        context "app"    = pure "MCQ"
        context x        = pure $ "??"<>x<>"??"
        routes :: Snap ()
        routes = route [
          ("/index.html", method GET $ do
              pg  <- liftIO $ TMPL.renderA defaultPage context
              writeLazyText pg
          ) ,
          ("testPlugin/answer/", method PUT $ do
             req <- getBody
             stateVal <- liftIO $ readIORef state
             tims :: TIMCmd state output <- liftIO $ do
                            m <- readIORef markup
                            s <- readIORef state
                            update plugin (m, s, fromPlainInput req)
             liftIO $ writeIORef state (save tims)
             writeLBS . encode $ object ["web" .= web tims] 
          )
          ] <|> serveDirectory "." -- serveStaticFiles "." plugin

    httpServe (setPort port mempty)
              (routes)
    where
     defaultPage = TMPL.template " \
\    <!DOCTYPE html> \
\     <html lang='en'> \
\     <head> <meta charset='utf-8'> \
\                 <script src='https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular.min.js'></script>\
\                 ${scripts}\
\                 ${styles}\
\                 <script> \
\                  var mainModule = angular.module('testApp',${moduleDeps}); \
\                 </script> \
\    </head> \
\    \
\     <body id='home' ng-app='testApp'> \
\     <h1>Test</h1> \
\     <div id='testPlugin' data-plugin='http://localhost:${port}'>\
\      $plugin \
\     </div> \
\     </body> \
\    </html> "


-- | Plain input is used to extract `{"input":..}` messages that the experimentation
--   mode needs to be able to catch so it can pretend to be TIM.
newtype PlainInput a = PI {fromPlainInput :: a}
instance FromJSON a => FromJSON (PlainInput a) where
    parseJSON (Object v) = PI <$> v .: "input"
    parseJSON _ = mzero

fromJSON' a = case fromJSON a of
    Error s   -> error s
    Success b -> b

-- | Extract a JSON value from the request body
getBody :: (MonadSnap m, FromJSON a) => m a
getBody = do
    f <- readRequestBody 100000
    case decode f of
        Nothing -> error $ "Could not decode input parameters:"++show f
        Just a  -> return a
