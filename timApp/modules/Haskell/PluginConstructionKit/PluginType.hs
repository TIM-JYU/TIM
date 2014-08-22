{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables, DataKinds, MultiParamTypeClasses, FlexibleInstances, PolyKinds, DeriveFunctor, FlexibleContexts#-}
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
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Data.IORef
import Control.Monad.Trans.Either

import Snap.Core
import Snap.Http.Server
import Snap.Util.Readable
import Snap.Util.FileServe

import qualified AnswerRequest as A
import qualified HTMLRequest   as H

import System.Directory
import UtilityPrelude

-- | Simplified interface for TIM plugins
data Plugin renderP updateP outputP = Plugin 
        { render  :: renderP -> IO LT.Text
        , update  :: updateP -> IO outputP
        , requirements :: [Requirement]
        , additionalFiles :: [FilePath]
        , additionalRoutes :: Snap ()}

-- Stolen from Control.Lens
(&) :: a -> (a -> b) -> b
a & f = f a
{-# INLINE (&) #-}


-- Blackboard specialization
data BlackboardCommand = Put T.Text | Delete T.Text deriving (Eq,Ord,Show)

keyOf (Put t)    = t
keyOf (Delete t) = t

instance FromJSON BlackboardCommand where
    parseJSON (String x) 
        | "!" `T.isPrefixOf` x = pure $ Delete (T.tail x)
        | otherwise            = pure $ Put x

instance ToJSON BlackboardCommand where
    toJSON (Put t) = String t
    toJSON (Delete t) = String (T.cons '!' t)

execBBCs hm bbcs = foldl' execBBC hm bbcs

execBBC hm (Put t)    = HashSet.insert t hm
execBBC hm (Delete t) = HashSet.delete t hm


-- Generating Angular directives with embedded json

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

data Stage = Render | Update deriving (Eq,Show)

data AR (s::k) a = AR (EitherT String IO a) deriving Functor
instance Applicative (AR s)  where
    pure x = AR (pure x)
    (AR a)<*>(AR b) = AR (a<*>b) 

runAR (AR x) = runEitherT x

class Available s a where
    getIt :: s -> AR s a

instance Available Value () where
    getIt _ = pure ()

instance (Available x a, Available x b) => Available x (a,b) where
    getIt v = (,) <$> getIt v <*> getIt v

instance (Available x a, Available x b, Available x c) => Available x (a,b,c) where
    getIt v = (,,) <$> getIt v <*> getIt v <*> getIt v

instance (Available x a, Available x b, Available x c, Available x d) => Available x (a,b,c,d) where
    getIt v = (,,,) <$> getIt v <*> getIt v <*> getIt v <*> getIt v

class Reply s a where
    putIt :: s -> a -> IO s

instance Reply Value () where
    putIt s _ = pure s

instance (Reply x a, Reply x b) => Reply x (a,b) where
    putIt s (v1,v2) = putIt s v1 >>= flip putIt v2

instance (Reply x a, Reply x b, Reply x c) => Reply x (a,b,c) where
    putIt s (v1,v2,v3) = putIt s v1 >>= flip putIt v2 >>= flip putIt v3

instance (Reply x a, Reply x b, Reply x c, Reply x d) => Reply x (a,b,c,d) where
    putIt s (v1,v2,v3,v4) = putIt s v1 >>= flip putIt v2 >>= flip putIt v3 >>= flip putIt v4

newtype State  a = State a deriving (Eq,Show)
newtype Markup a = Markup a deriving (Eq,Show)
newtype Input  a = Input a deriving (Eq,Show)

newtype Save a = Save a deriving (Eq,Show)
newtype Web a  = Web a deriving (Eq,Show)
newtype Blackboard = Blackboard [BlackboardCommand]  deriving (Eq,Show)
newtype TimResult  = TR [(T.Text,Value)] deriving Show
instance ToJSON TimResult where
    toJSON (TR a) = object a

instance (ToJSON a) => Reply TimResult (Save a) where
    putIt (TR x) (Save v) = return $ TR (("save".=v):x)

instance (ToJSON a) => Reply TimResult (Web a) where
    putIt (TR x) (Web v) = return $ TR (("web".=v):x)

instance Reply TimResult Blackboard where
    putIt (TR x) (Blackboard bc) = return $ TR (("bb".=bc):x)

-- PluginSpecific
newtype TimRender = TimRender Value
newtype TimUpdate = TimUpdate Value

instance FromJSON a => Available TimRender (State a) where
    getIt (TimRender x) = State <$> getField "state" x
instance FromJSON a => Available TimUpdate (State a) where
    getIt (TimUpdate x) = State <$> getField "state" x 
instance FromJSON a => Available TimRender (Markup a) where
    getIt (TimRender x) = Markup <$> getField "markup" x 
instance FromJSON a => Available TimUpdate (Markup a) where
    getIt (TimUpdate x) = Markup <$> getField "markup" x

instance FromJSON a => Available TimUpdate (Input a) where
    getIt (TimUpdate x) = Input <$> getField "input" x 

getField f (Object v) = case HashMap.lookup "state" v of
                    Nothing -> AR $ left ("No key '"++show f++"' in "++show (Object v))
                    Just s  -> case fromJSON s of
                        Error e   -> AR $ left e
                        Success a -> AR $ right a
getField _ x = AR $ left ("Expected object, got "++show x)
--
-- | Serve a plugin
serve :: forall m renderP updateP output. 
         (MonadSnap m, Available TimRender renderP, Available TimUpdate updateP,Reply TimResult output) => Plugin renderP updateP output -> m ()
serve plugin = route  
        [
        ("html/", method POST $ do
            req :: Value <- getBody
            runArM (getIt (TimRender req) :: AR TimRender renderP)
                   (liftIO  . render plugin >=> writeLazyText)
        ),
        ("reqs/", method GET $ do
            writeLBS . encode $ 
                object [
                        "js"  .= [x | JS  x <- requirements plugin]
                       ,"css" .= [x | CSS x <- requirements plugin]
                       ,"angularModule" .= [x | NGModule x <- requirements plugin]
                       ,"type" .= ("embedded"::T.Text)
                       ]
        ),
        ("answer/", method PUT $ do
           req      <- getBody
           runArM (getIt (TimUpdate req) :: AR TimUpdate updateP)
                   $ \ps -> do
                      tims  <- liftIO (update plugin ps)
                      reply <- liftIO (putIt (TR []) tims)
                      writeLBS . encode $ reply                      
        )
        ]
        <|> serveStaticFiles "." plugin
    where 
     runArM :: forall s a. MonadSnap m => AR s a -> (a-> m ()) -> m ()
     runArM ar f = do
           eps <- liftIO $ runAR ar
           case eps of
                Left err -> modifyResponse (setResponseCode 400) >> writeLazyText "Unable to parse required parameters"
                Right ps -> f ps

-- Quick helper for building objects
ins key (First Nothing)  x = x 
ins key (First (Just v)) x = (key.=v):x 

serveStaticFiles from plugin = do
        let locals = [T.unpack file | CSS file <- requirements plugin]
                     ++ [T.unpack file | JS  file <- requirements plugin]
        rq <- getSafePath
        liftIO $ print (rq,locals,additionalFiles plugin)
        when (rq`elem`locals) (serveFile rq)
        when (rq`elem`additionalFiles plugin) (serveFile rq)
--
--experiment :: forall structure markup state input output. 
--                (FromJSON structure, FromJSON state, ToJSON output, FromJSON input) => 
--                    Plugin structure state input output -> structure -> Int -> IO ()
--experiment plugin markup' port = do 
--    state  <- newIORef (initial plugin)
--    markup <- newIORef markup' 
--    blackboard <- newIORef (mempty :: HashSet T.Text)
--    let context "port"   = pure (T.pack $ show port)
--        context "plugin" = do
--                            m <- readIORef markup 
--                            s <-  readIORef state
--                            LT.toStrict <$> render plugin (m,s)
--        context "moduleDeps" = pure . T.pack . show $ [x | NGModule x <- requirements plugin]
--        context "scripts"    = pure . T.unlines 
--                                        $ ["<script src='"<>x<>"'></script>" 
--                                          | JS x <- requirements plugin]
--        context "styles"       = pure . T.unlines 
--                                        $ ["<link rel='stylesheet' type='text/css' href='"<>x<>"'>" 
--                                          | CSS x <- requirements plugin]
--        context "app"    = pure "MCQ"
--        context x        = pure $ "??"<>x<>"??"
--        routes :: Snap ()
--        routes = route [
--          ("/index.html", method GET $ do
--              pg  <- liftIO $ TMPL.renderA defaultPage context
--              writeLazyText pg
--          ) ,
--          ("testPlugin/answer/", method PUT $ do
--             req <- getBody
--             stateVal <- liftIO $ readIORef state
--             tims :: TIMCmd state output <- liftIO $ do
--                            m <- readIORef markup
--                            s <- readIORef state
--                            update plugin (m, s, fromPlainInput req)
--             liftIO $ maybe (return ())
--                            (writeIORef state)
--                            (getFirst (_save tims))
--             writeLBS . encode . object $
--               [] & ins "web" (_web tims)
--               -- ["web" .= _web tims] 
--          )
--          ] <|> serveDirectory "." -- serveStaticFiles "." plugin
--
--    httpServe (setPort port mempty)
--              (routes)
--    where
--     defaultPage = TMPL.template " \
-- \    <!DOCTYPE html> \
-- \     <html lang='en'> \
-- \     <head> <meta charset='utf-8'> \
-- \                 <script src='https://ajax.googleapis.com/ajax/libs/angularjs/1.3.0-beta.17/angular.min.js'></script>\
-- \                 ${scripts}\
-- \                 ${styles}\
-- \                 <script> \
-- \                  var mainModule = angular.module('testApp',${moduleDeps}); \
-- \                 </script> \
-- \    </head> \
-- \    \
-- \     <body id='home' ng-app='testApp'> \
-- \     <h1>Test</h1> \
-- \     <div id='testPlugin' data-plugin='http://localhost:${port}'>\
-- \      $plugin \
-- \     </div> \
-- \     </body> \
-- \    </html> "
--
--
---- | Plain input is used to extract `{"input":..}` messages that the experimentation
----   mode needs to be able to catch so it can pretend to be TIM.
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
