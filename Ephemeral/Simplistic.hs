
{-#LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
module Main where
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import Data.Cache.LRU.IO as LRU
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.List
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Snap.Core
import Data.Ord
import Data.Aeson
import GHC.Generics
import Data.Traversable
import Data.Algorithm.Diff as D
import Data.Algorithm.Diff3 as D3
import Snap.Util.Readable
import Snap.Http.Server
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as DSB
import qualified Data.ByteString.Search as BS
import EphemeralPrelude
import Text.Blaze.Html.Renderer.Text
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Data.UUID
import Data.UUID.V4

newtype DocID = DocID T.Text deriving(Eq,Ord,Show)

instance Readable DocID where
    fromBS = return . DocID . T.decodeUtf8

instance ToJSON DocID where
    toJSON (DocID t) = toJSON t 

instance ToJSON a => ToJSON (Seq a) where
    toJSON = toJSON . F.toList

data Block = Block {markdown::T.Text, html::LT.Text, bagOfWords :: HashSet T.Text} deriving (Show,Eq)
newtype Doc = Doc {fromDoc::Seq Block} deriving (Eq,Show)

convert :: BS.ByteString -> Doc
convert bs = case (PDC.readMarkdown PDC.def . T.unpack . T.decodeUtf8 . LBS.toStrict . normaliseCRLF $ bs) of
              PDC.Pandoc _ blocks -> Doc . Seq.fromList .  map convertBlock $ blocks
            where 
                normaliseCRLF  = BS.replace "\r\n" ("\n"::BS.ByteString)
                htmlOpts = PDC.def{PDC.writerHTMLMathMethod=PDC.MathJax 
                            "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"}
                markdownOpts = PDC.def{PDC.writerSetextHeaders=False}
                -- html_opts =  PDC.def{PDC.writerExtensions = DSB.singleton PDC_Opt.Ext_markdown_in_html_blocks}
                convertBlock t = let  pdc = PDC.Pandoc mempty . box $ t
                                 in Block
                                     (T.pack       . PDC.writeMarkdown  markdownOpts  $ pdc) -- Laajennos pois: html_optsin tilalle PDC.def
                                     (renderHtml   . PDC.writeHtml      htmlOpts      $ pdc)
                                     (Set.fromList . map T.pack . words . PDC.writeMarkdown PDC.def $ pdc)
                            

box :: t -> [t]
box x = [x]

newtype State = State (AtomicLRU DocID Doc)

initialize :: IO State 
initialize = newAtomicLRU (Just 10000) >>= return . State

-- Diff Handling

data Labeled label a = Labeled {getLabel::label,unLabel::a} deriving (Show)

instance Eq a => Eq (Labeled x a) where
    Labeled _ a == Labeled _ b = a == b

instance Ord a => Ord (Labeled x a) where
    compare = compare `on` unLabel

labelList :: (Enum t1, Num t1) => t -> [a] -> [Labeled (t, t1) a]
labelList lbl lst = [Labeled (lbl,i) x | (i,x) <- zip [0..] lst]

-- Return the list of affinities for given block of a given document when compared to
-- all blocks of another document
performMatch :: MonadIO m => (DocID,Int) -> DocID -> State -> EitherT Value m [(Double,Int)]
performMatch (d1ID,i1) d2ID state = do
   Doc d2 <- fetchDoc d2ID state
   block  <- fetchBlock d1ID i1 state
   return $ zip (map (textAffinity block) (F.toList d2)) [0..]

performAffinityMap :: MonadIO m => DocID -> DocID -> State -> EitherT Value m [(Int,Int,Double)] 
performAffinityMap d1ID d2ID state = do
   d1 <- fetchDoc d1ID state
   Doc d2 <- fetchDoc d2ID state
-- blockMatch :: Doc -> Block -> Maybe (Int, Double, Double)
   return [(idx1,idx2,cfd)
          | (matchee,idx1) <- zip (F.toList d2) [0..]
          , let a@(~(Just (idx2,aff,cfd))) = blockMatch d1 matchee
          , isJust a]

-- TODO: PerformDiff is really, really slow!
performDiff :: MonadIO m => DocID -> DocID -> State -> EitherT Value m [(DocID,Int)]
performDiff parentID childID state@(State st) = do
   Doc parent <- fetchDoc parentID state 
   Doc child  <- fetchDoc childID  state 
   return $ execDiff (labelList parentID (F.toList parent)) 
                     (labelList childID  (F.toList child))


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

-- Operations

-- Load a document from a given bytestring and insert it to the LRU
loadDocument :: MonadIO m => DocID -> BS.ByteString -> State -> m ()
loadDocument docID bs (State st) = liftIO $ 
    LRU.insert docID (convert bs) st

-- Retrieve a document
fetchDoc :: MonadIO m => DocID -> State -> EitherT Value m Doc
fetchDoc docID (State st) =  hoistMaybe (jsErr $ "No such document: "<> (T.pack $ show docID))
                                        (liftIO $ LRU.lookup docID st)


-- Fetch a Block from a document according to ID.
fetchBlock :: MonadIO m => DocID -> Int -> State -> EitherT Value m Block
fetchBlock docID index state = do
    Doc d <- fetchDoc docID state
    abortIf (index < 0 && index >= Seq.length d)
            (jsErr "Index out of bounds")
    return (Seq.index d index)

-- Rename a block
-- NOTE: This is NOT Atomic
rename :: MonadIO m => DocID -> DocID -> State -> EitherT Value m ()
rename from to state@(State st) = do
    doc <- fetchDoc from state
    liftIO $ do
        LRU.insert to doc st
        LRU.delete from   st
    return ()

-- Replace a block
replace :: MonadIO m => DocID -> Int -> BS.ByteString -> State -> EitherT Value m DocID
replace docID index bs state@(State st) = do
    (Doc d) <- fetchDoc docID state
    insertNew (Doc . insertRange d index . fromDoc . convert $ bs) state

-- Insert a document into LRU with a generated docID
insertNew :: MonadIO m => Doc -> State -> m DocID
insertNew doc state@(State st) = do
    liftIO $ do
     uuid <- DocID . T.decodeUtf8 . toASCIIBytes <$> nextRandom
     LRU.insert uuid doc st
     return uuid

jsErr :: T.Text -> Value
jsErr e = object ["error" .= e] 

jsOk :: T.Text -> Value
jsOk e = object ["ok" .= e] 

abortIf :: Monad m => Bool -> e -> EitherT e m ()
abortIf True  e = left e
abortIf False e = right ()

hoistMaybe :: Monad m => e -> m (Maybe a) -> EitherT e m a
hoistMaybe str op = EitherT $ op >>= return . maybe (Left str) Right
        

-- Add a new paragraph
addParagraph :: MonadIO m => DocID -> Int -> BS.ByteString -> State -> EitherT Value m DocID
addParagraph docID idx bs state@(State st) = do
    Doc d <- fetchDoc docID state 
    let (s,e) = Seq.splitAt idx d
    insertNew (Doc $ s <> fromDoc (convert bs) <> e) state


-- Delete a paragraph
removePar :: MonadIO m => DocID -> Int -> State -> EitherT Value m DocID
removePar docID idx state = do
    Doc d <- fetchDoc docID state
    let (s,e) = Seq.splitAt idx d
    insertNew (Doc $ s <> Seq.drop 1 e) state

-- Currently acts as part of replace, removes original block.
insertRange :: Seq a -> Int -> Seq a -> Seq a
insertRange orig index source = 
    let (s,e) = Seq.splitAt index orig
    in  s <> source <> (Seq.drop 1 e)

-- Calculate similarity between two text blocks
textAffinity :: Block -> Block -> Double
textAffinity d1 d2 
 = (Set.size (Set.intersection (bagOfWords d1) (bagOfWords d2))
   `fdiv`
   Set.size  (Set.union        (bagOfWords d1) (bagOfWords d2)))
 where fdiv a b = fromIntegral a / fromIntegral b

-- Determine the closest matching block in given document.
blockMatch :: Doc -> Block -> Maybe (Int, Double, Double)
blockMatch (Doc d) block = case sortBy (compare`on`negate.fst3) . map (\(i,x) -> (textAffinity block x,i,x)) . zip [0..] . F.toList $ d of
                            ((a1,i1,blk1):(a2,_,blk2):_) -> Just (i1, a1,  if blk1==blk2 then 1 else a1-a2)
                            [(a1,i1,_)]                  -> Just (i1, a1, -1)
                            []                           -> Nothing
                        where fst3 (a,_,_) = a 


main :: IO ()
main = do
    state <- initialize
    let withDoc op = runFailing $ do
            docID <- requireParamE "docID"
            fetchDoc docID state>>=lift.op
        withBlock op = runFailing $  do
            docID <- requireParamE "docID"
            idx   <- requireParamE "idx"
            fetchBlock docID idx state >>= lift.op
        runFailing :: EitherT Value Snap a -> Snap ()
        runFailing op = eitherT (\res -> do
                                    modifyResponse (setResponseStatus 404 "Bad request")
                                    writeLBS.encode $ res) -- In case of failure, send the encoded error
                                (\_ -> return ()) -- In case of success, assume that data has been already sent
                                op

    quickHttpServe $ route 
        [
         -- Send the whole doc as markdown. Required: [?]
         (":docID", method GET . withDoc $ \(Doc d) ->
                traverse (\x -> writeText (markdown x) >> writeText "\n\n") d >> return ()),
         
         -- Send the whole doc as json containing html for the blocks. Required: [X]
         ("/json-html/:docID", method GET . withDoc $ \(Doc d) -> 
                writeLBS . encode . fmap html $ d),
         
         ("/json/:docID", method GET . withDoc $ \(Doc d) -> 
                writeLBS . encode . fmap markdown $ d),
         
         -- Send the whole doc as html. Required: [?]
         ("/html/:docID", method GET . withDoc $ \(Doc d) ->
                traverse (\x -> writeLazyText (html x) >> writeText "\n\n") d >> return ()),
         
         -- Send a single block as markdown. Required [?]
         (":docID/:idx", method GET . withBlock $ \block -> writeText (markdown block)),

         -- Send a single block as HTML. Required [X]
         (":docID/:idx/html", method GET . withBlock $ \block -> writeLazyText (html block)),


         -- Rename a document. Required [X]
         ("rename/:from/:to", method POST . runFailing $ do
            from <- requireParamE "from"
            to   <- requireParamE "to"
            rename from to state),
            

         -- Replace a block. Required [X]
         (":docID/:idx", method PUT . runFailing $ do
            docID <- requireParamE "docID"
            idx   <- requireParamE "idx"
            bd    <- lift (readRequestBody (1024*2000))
            ident <- replace docID idx (LBS.toStrict bd) state
            lift $ writeLBS . encode $ object
                ["paragraphs" .= (fmap html . fromDoc . convert . LBS.toStrict $ bd)
                ,"new_id"     .= ident]
            --let (Doc d) = convert (LBS.toStrict bd)
            --lift (writeLBS . encode . )
         ),
         -- Add a new paragraph. Required [X]
         ("/new/:docID/:idx", method POST . runFailing $ do
            docID <- requireParamE "docID"
            idx   <- requireParamE "idx"
            bd    <- lift (readRequestBody (1024*2000))
            ident <- addParagraph docID idx (LBS.toStrict bd) state
            lift $ writeLBS . encode $ object
                ["paragraphs" .= (fmap html . fromDoc . convert . LBS.toStrict $ bd)
                ,"new_id"     .= ident]
         ),
         -- Delete a paragraph. Required [X]
         (":docID/:idx", method DELETE . runFailing $ do
            docID <- requireParamE "docID"
            idx   <- requireParamE "idx"
            ident <- removePar docID idx state
            lift $ writeLBS . encode $ object
                ["new_id"     .= ident]
         ),
         -- Load an entire markdown document into cache. Required [X]
         ("load/:docID/", method POST . runFailing $ do
            docID <- requireParamE "docID"
            lift $ do
                bd    <- readRequestBody (1024*2000)
                loadDocument docID (LBS.toStrict bd) state
                writeBS "{\"Ok\":\"Document loaded\"}" 
         ),
         -- Get an affinity map for paragraph `:doc1idx` in document `doc1ID` against
         -- document `doc2ID`.
         ("match/:doc1ID/:doc1idx/:doc2ID", method GET . runFailing $ do
            d1ID  <- requireParamE "doc1ID"
            d2ID  <- requireParamE "doc2ID"
            d1idx <- requireParamE "doc1idx"
            aff   <- performMatch (d1ID,d1idx) d2ID state 
            lift (writeLBS . encode $ aff)
         ),
         -- Get a complete affinity map between documents `doc1D` and `doc2ID`.
         ("mapDocuments/:doc1ID/:doc2ID", method GET . runFailing $ do
            d1ID  <- requireParamE "doc1ID"
            d2ID  <- requireParamE "doc2ID"
            aff   <- performAffinityMap d1ID d2ID state 
            lift (writeLBS . encode $ aff)
         ),
         ("diff/:parentID/:childID/", method GET . runFailing $ do
            parentID <- requireParamE "parentID"
            childID  <- requireParamE "childID"
            diffed <- performDiff parentID childID state
            lift (writeLBS (encode diffed))
         ),
         ("diff3/:parentID/:childID1/:childID2", method GET . runFailing $ do
            parentID <- requireParamE "parentID"
            childID1 <- requireParamE "childID1"
            childID2 <- requireParamE "childID2"
            diffed <- performDiff3 parentID childID1 childID2 state
            lift (writeLBS (encode diffed))
         )
 
        ]

requireParamE :: (MonadSnap m, Readable b) => BS.ByteString -> EitherT Value m b
requireParamE p = lift (getParam p) >>= \x -> case x of
                    Just val -> lift $ fromBS val
                    Nothing  -> left . jsErr $ "Parameter " <> T.decodeUtf8 p <> " needed"

