{-#LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
module Main where
import qualified Text.Pandoc as PDC
import Data.Cache.LRU.IO as LRU
import qualified Data.Foldable as F
import qualified Data.Text as T
import Data.List
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans
import Snap.Core
import Data.Aeson
import GHC.Generics
import Data.Algorithm.Diff as D
import Data.Algorithm.Diff3 as D3
import Snap.Util.Readable
import Snap.Http.Server
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import EphemeralPrelude

newtype DocID = DocID T.Text deriving(Eq,Ord,Show)
instance Readable DocID where
    fromBS = return . DocID . T.decodeUtf8
instance ToJSON DocID where
    toJSON (DocID t) = toJSON t 

type Block = T.Text
newtype Doc = Doc {fromDoc::Seq Block}

convert :: BS.ByteString -> Doc
convert bs = case (PDC.readMarkdown PDC.def . T.unpack . T.decodeUtf8 $ bs) of
              PDC.Pandoc _ blocks -> Doc . Seq.fromList . 
                    map (T.pack . PDC.writeMarkdown PDC.def . PDC.Pandoc mempty . box) 
                    $ blocks

box :: t -> [t]
box x = [x]

newtype State = State (AtomicLRU DocID Doc)

initialize :: IO State 
initialize = newAtomicLRU (Just 200) >>= return . State

-- Diff Handling

data Labeled label a = Labeled {getLabel::label,unLabel::a} deriving (Show)

instance Eq a => Eq (Labeled x a) where
    Labeled _ a == Labeled _ b = a == b

instance Ord a => Ord (Labeled x a) where
    compare = compare `on` unLabel

labelList :: (Enum t1, Num t1) => t -> [a] -> [Labeled (t, t1) a]
labelList lbl lst = [Labeled (lbl,i) x | (i,x) <- zip [0..] lst]

-- TODO: PerformDiff is really, really slow!
performDiff :: MonadIO m => DocID -> DocID -> State -> m (Maybe [(DocID,Int)])
performDiff parentID childID (State st) = liftIO $ do
   parent <- LRU.lookup parentID st 
   child <- LRU.lookup childID st 
   case (parent,child) of
     (Just (Doc a),Just (Doc b)) 
       -> return $ Just (execDiff (labelList parentID (F.toList a)) 
                                  (labelList childID (F.toList b)))
     _ ->  return Nothing

-- TODO: PerformDiff3 is really, really slow!
performDiff3 :: MonadIO m => DocID -> DocID -> DocID -> State -> m (Maybe [D3.Hunk (DocID,Integer)])
performDiff3 parentID childID1 childID2 (State st) = liftIO $ do
   parent <- LRU.lookup parentID st 
   child1 <- LRU.lookup childID1 st 
   child2 <- LRU.lookup childID2 st 
   case (parent,child1,child2) of
     (Just (Doc a),Just (Doc b),Just (Doc c)) 
       -> return . Just . fmap (fmap getLabel) $ 
                diff3  (labelList parentID (F.toList a)) 
                       (labelList childID1 (F.toList b))
                       (labelList childID2 (F.toList c))
     _ ->  return Nothing

instance ToJSON a => ToJSON (Hunk a) where
    toJSON (LeftChange a)   = object ["Left"   .= a]
    toJSON (RightChange a)  = object ["Right"  .= a]
    toJSON (Unchanged a)    = object ["Parent" .= a]
    toJSON (Conflict a b c) = object ["Left" .= a, "Right" .= b, "Parent" .= c]

execDiff :: [Labeled (DocID,Int) Block] -> [Labeled (DocID,Int) Block] -> [(DocID,Int)]
execDiff a b = reverse $ foldl' op [] (getDiff a b)
   where 
    op acc (D.First  _) = acc
    op acc (Second bl) = getLabel bl:acc
    op acc (Both bl _) = getLabel bl:acc

-- Ops
loadDocument :: MonadIO m => DocID -> BS.ByteString -> State -> m ()
loadDocument docID bs (State st) = liftIO $ 
    LRU.insert docID (convert bs) st



fetchBlock :: MonadIO m => DocID -> Int -> State -> m (Maybe Block)
fetchBlock docID index (State st) = liftIO $ do
    doc <- LRU.lookup docID st
    case doc of
        Just (Doc d) 
         | index >= 0 && index < Seq.length d 
          -> return (Just $ Seq.index d index)
        _ -> return Nothing

replace :: MonadIO m => DocID -> Int -> BS.ByteString -> State -> m ()
replace docID index bs (State st) = liftIO $ do
    doc <- LRU.lookup docID st
    case doc of 
        Nothing       -> LRU.insert docID (convert bs) st
        Just (Doc d)  -> LRU.insert docID (Doc $ insertRange d index (fromDoc (convert bs))) st

insertRange :: Seq a -> Int -> Seq a -> Seq a
insertRange orig index source = 
    let 
     (s,e) = Seq.splitAt index orig
    in s <> source <> e

main :: IO ()
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
         (":docID/:idx", method PUT $ do
            docID <- requireParam "docID"
            idx   <- requireParam "idx"
            bd    <- readRequestBody (1024*2000)
            replace docID idx (LBS.toStrict bd) state
         ),
         ("load/:docID/", method POST $ do
            docID <- requireParam "docID"
            bd    <- readRequestBody (1024*2000)
            loadDocument docID (LBS.toStrict bd) state
            writeBS "{\"Ok\":\"Document loaded\"}" 
         ),
         ("diff/:parentID/:childID/", method GET $ do
            parentID <- requireParam "parentID"
            childID  <- requireParam "childID"
            diffed <- performDiff parentID childID state
            writeLBS (encode diffed) 
         ),
         ("diff3/:parentID/:childID1/:childID2", method GET $ do
            parentID <- requireParam "parentID"
            childID1  <- requireParam "childID1"
            childID2  <- requireParam "childID2"
            diffed <- performDiff3 parentID childID1 childID2 state
            writeLBS (encode diffed) 
         )
 
        ]

requireParam :: (MonadSnap m, Readable b) => BS.ByteString -> m b
requireParam p = getParam p >>= \x -> case x of
                    Just val -> fromBS val
                    Nothing -> error $ "Parameter "++show p++" needed" -- TODO
