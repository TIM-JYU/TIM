{-#LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables#-}
module PluginType where

import Data.Aeson
import GHC.Generics

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap,(!))

import Snap.Core
import Snap.Util.Readable
import Snap.Util.FileServe

import qualified AnswerRequest as A
import qualified HTMLRequest   as H

import System.Directory
import UtilityPrelude

data Plugin structure state input = Plugin 
        { initial :: state
        , render  :: structure -> state -> LT.Text
        , update  :: structure -> state -> input -> IO [TIMCmd]
        , requirements :: [Requirement]
        , additionalRoutes :: Snap ()}

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


data Requirement = JS T.Text
                 | CSS T.Text
                 | NGModule T.Text
                 deriving (Show,Generic) 

instance ToJSON Requirement where
    toJSON (JS  t)      = object ["js"  .= t]
    toJSON (CSS t)      = object ["css" .= t]
    toJSON (NGModule t) = object ["angularModule" .= t]

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
        <|> staticFiles
    where 
     staticFiles = do
        locals <- liftIO $ filterM doesFileExist $ [T.unpack file | CSS file <- requirements plugin]
                                                ++ [T.unpack file | JS  file <- requirements plugin]
        rq <- getSafePath
        when (rq`elem`locals) (serveFile rq)

getBody :: (MonadSnap m, FromJSON a) => m a
getBody = do
    f <- readRequestBody 100000
    case decode f of
        Nothing -> error $ "Could not decode input parameters:"++show f
        Just a  -> return a

-- requireParamE :: (MonadSnap m, Readable b) => BS.ByteString -> m b
-- requireParamE p = lift (getParam p) >>= \x -> case x of
--                     Just val -> lift $ fromBS val
--                     Nothing  -> error $ T.unpack $ "Parameter " <> T.decodeUtf8 p <> " needed"
-- 
