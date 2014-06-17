{-#LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
module Main where
import qualified Text.Pandoc as PDC
import Data.Cache.LRU.IO as LRU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import Data.Aeson
import Control.Monad.Trans
import Snap.Core
import Snap.Util.Readable
import Snap.Http.Server
import Data.Monoid

type DocID = BS.ByteString
type Block = T.Text
newtype Doc = Doc [Block]

convert :: BS.ByteString -> Doc
convert bs = case (PDC.readMarkdown PDC.def . T.unpack . T.decodeUtf8 $ bs) of
              PDC.Pandoc _ bs -> Doc $ map (T.pack . PDC.writeMarkdown PDC.def . PDC.Pandoc mempty . box) bs

box x = [x]

newtype State = State (AtomicLRU BS.ByteString Doc)

initialize :: IO State 
initialize = newAtomicLRU (Just 200) >>= return . State

-- Ops
loadDocument :: MonadIO m => DocID -> BS.ByteString -> State -> m ()
loadDocument docID bs (State st) = liftIO $ do
    let doc@(Doc d) = convert bs
    liftIO $ print (length d)
    insert docID doc st

fetchBlock :: MonadIO m => DocID -> Int -> State -> m (Maybe Block)
fetchBlock docID index (State st) = liftIO $ do
    doc <- LRU.lookup docID st
    case doc of
        Just (Doc d) 
         | index >= 0 && index < length d 
          -> return (Just (d !! index))
        _ -> return Nothing

main = do
    state <- initialize
    quickHttpServe $ route 
        [
         (":docID/:idx", method GET $ do
            docID <- requireParam "docID"
            idx   <- requireParam "idx"
            block <- fetchBlock docID idx state
            case block of 
                Nothing -> writeBS "{\"Error\":\"No block found\"}" 
                Just  r -> writeText r
         ),
         ("load/:docID/", method POST $ do
            docID <- requireParam "docID"
            bd    <- readRequestBody (1024*2000)
            liftIO $ LBS.writeFile "whatIGot" bd
            loadDocument docID (LBS.toStrict bd) state
            writeBS "{\"Ok\":\"Document loaded\"}" 
            )
        ]

requireParam p = getParam p >>= \x -> case x of
                    Just p -> fromBS p
                    Nothing -> error $ "Parameter "++show p++" needed" -- TODO
