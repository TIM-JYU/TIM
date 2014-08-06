{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables#-}
module PluginType where

import Data.Aeson
import GHC.Generics

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Template as TMPL
import qualified Data.Text.Encoding as T
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
data Plugin structure state input = Plugin 
        { initial :: state
        , render  :: structure -> state -> LT.Text
        , update  :: structure -> state -> input -> IO [TIMCmd]
        , requirements :: [Requirement]
        , additionalFiles :: [FilePath]
        , additionalRoutes :: Snap ()}

-- | Simplified interface for ordering TIM to modify its data
data TIMCmd = Save Value 
            | SaveMarkup Value
            | Web T.Text Value
            deriving (Show, Generic)

save,saveMarkup :: (ToJSON a) => a -> TIMCmd
save       = Save       . toJSON
saveMarkup = SaveMarkup . toJSON

web :: (ToJSON a) => T.Text -> a -> TIMCmd
web key   = Web key    . toJSON

ngDirective :: ToJSON a => LB.ByteString -> a -> LB.ByteString
ngDirective tag content = "<"<>tag<>" data-content='"<>encode content<>"'></"<>tag<>">"

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
serve :: (MonadSnap m, MonadIO m, FromJSON structure, FromJSON state, FromJSON input) => Plugin structure state input -> m ()
serve plugin = route  
        [
        ("html/", method POST $ do
            req <- getBody
            writeLazyText $ render plugin (H.markup req) (fromMaybe (initial plugin) (H.state req))
        ),
        ("reqs/", method GET $ do
            writeLBS . encode $ requirements plugin
        ),
        ("answer/", method PUT $ do
           req  <- getBody
           tims <- liftIO (update plugin (A.markup req) (fromMaybe (initial plugin) (A.state req)) (A.input req))
           let web    = object [key .= val | Web key val <- tims ]
               save   = toJSON $ listToMaybe [save | Save save <- tims ]
               markup = toJSON $ listToMaybe [save | SaveMarkup save <- tims ]
           writeLBS . encode $ object ["web" .= web, "save" .= save, "save-markup" .= markup ] 
        )
        ]
        <|> serveStaticFiles plugin
    where 

serveStaticFiles plugin = do
        locals <- liftIO $ filterM doesFileExist $ [T.unpack file | CSS file <- requirements plugin]
                                                ++ [T.unpack file | JS  file <- requirements plugin]
        rq <- getSafePath
        when (rq`elem`locals) (serveFile rq)
        when (rq`elem`additionalFiles plugin) (serveFile rq)

experiment :: forall structure markup state input. (FromJSON structure, FromJSON state, FromJSON input) => Plugin structure state input -> structure -> Int -> IO ()
experiment plugin markup' port = do 
    state  <- newIORef (initial plugin)
    markup <- newIORef markup'
    let context "port"   = pure (T.pack $ show port)
        context "plugin" = LT.toStrict <$> (render plugin <$> readIORef markup <*> readIORef state)
        context "moduleDeps" = pure . T.pack . show $ [x | NGModule x <- requirements plugin]
        context "scripts"    = pure . T.unlines 
                                        $ ["<script src='"<>x<>"'></script>" 
                                          | JS x <- requirements plugin]
        context "csss"       = pure . T.unlines 
                                        $ ["<link rel='stylesheet' type='text/css' href='"<>x<>"'>" 
                                          | JS x <- requirements plugin]
        context "app"    = pure "MCQ"
        context x        = pure $ "??"<>x<>"??"
        routes :: Snap ()
        routes = route [
          ("/index.html", method GET $ do
              pg  <- liftIO $ TMPL.renderA defaultPage context
              writeLazyText pg
          ) ,
          ("answer/", method PUT $ do
             req <- getBody
             stateVal <- liftIO $ readIORef state
             tims :: [TIMCmd] <- liftIO $ do
                            m <- readIORef markup
                            s <- readIORef state
                            update plugin m s (fromPlainInput req)
             let web    = object [key .= val  | Web key val <- tims ]
             liftIO $ sequence_ [writeIORef state  (fromJSON' save) | Save save <- tims]
             liftIO $ sequence_ [writeIORef markup (fromJSON' save) | SaveMarkup save <- tims]
             writeLBS . encode $ object ["web" .= web] 
          )
          ] <|> serveStaticFiles plugin

    httpServe (setPort port mempty)
              (routes)
    where
     defaultPage = TMPL.template " \
\    <!DOCTYPE html> \
\     <html lang='en'> \
\     <head> <meta charset='utf-8'> \
\                 <script src='https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular.min.js'></script>\
\                 ${scripts}\
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
