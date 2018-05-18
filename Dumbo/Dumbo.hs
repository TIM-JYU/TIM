{-#OPTIONS_GHC -Wall #-}
{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveFoldable #-}
{-#LANGUAGE DeriveTraversable #-}
module Main where
import Data.Monoid
import Text.Pandoc.Walk
import GHC.Generics
import Control.Exception
import Data.Typeable
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import System.Directory
import qualified Data.Vector as V
import Control.Monad.Trans
import Control.Monad
import Data.Maybe
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Set as Set
import Control.Lens (transformM)
import qualified Data.ByteString.Lazy as LBS
import System.FilePath
import Snap.Core
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens
import Snap.Http.Server
import Text.Blaze.Html.Renderer.Text
import Text.TeXMath.Readers.TeX.Macros
import Control.Applicative
import qualified AsciiMath
import Tex2Svg
import Data.Digest.Pure.SHA
import Options.Generic
import Debug.Trace

tex2SvgPass :: DumboRTC -> T.Text -> (PDC.Inline -> IO PDC.Inline)
tex2SvgPass drtc mathPreamble = tex2svg (tex2svgRtc drtc) mathPreamble

convertBlock :: Conversion -> (PDC.Inline -> IO PDC.Inline) -> T.Text -> IO LT.Text
convertBlock target ms txt = do
    fmap (either (LT.pack . show) id) <$> PDC.runIO $
        PDC.readMarkdown readerOpts txt >>= make
 where
  make :: (PDC.PandocMonad m, MonadIO m) => PDC.Pandoc -> m LT.Text
  make x = case target of 
        ToHTML  -> walkM (liftIO . ms) x >>= makeHtml
        ToLatex -> makeLatex x
  makeHtml, makeLatex :: PDC.PandocMonad m => PDC.Pandoc -> m LT.Text
  makeHtml  pdc  = fmap renderHtml (PDC.writeHtml5 htmlOpts pdc)
  makeLatex pdc =  LT.fromStrict <$> PDC.writeLaTeX latexOpts pdc

readerOpts :: PDC_Opt.ReaderOptions
readerOpts =
  PDC.def
  { PDC.readerExtensions 
     = PDC_Opt.enableExtension PDC_Opt.Ext_tex_math_dollars
        $ PDC.pandocExtensions 
  }

latexOpts :: PDC_Opt.WriterOptions
latexOpts = PDC.def

htmlOpts :: PDC_Opt.WriterOptions
htmlOpts =
  PDC.def
  { PDC.writerHTMLMathMethod =
    PDC.MathJax
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  , PDC.writerExtensions = PDC_Opt.enableExtension PDC_Opt.Ext_definition_lists 
                           . PDC_Opt.enableExtension PDC_Opt.Ext_latex_macros 
                           $ PDC.pandocExtensions
  }

stripSome :: (T.Text -> T.Text -> Maybe T.Text) -> T.Text -> T.Text -> T.Text
stripSome f piece str = fromMaybe str (f piece str)

stripP :: T.Text -> T.Text
stripP = stripSome T.stripSuffix "</p>" . stripSome T.stripPrefix "<p>"

data Conversion = ToHTML | ToLatex deriving (Eq,Show)

data DumboArgs f = DC {latex     :: f ::: Maybe FilePath <?> "Absolute path to latex binary"
                      ,dvisvgm   :: f ::: Maybe FilePath <?> "Absolute path to dvisvgm binary"
                      ,cacheDir  :: f ::: FilePath <?> "Directory for cached math"
                      ,tmpDir    :: f ::: FilePath <?> "Directory to store temporary files"
                      ,port      :: f ::: Int <?> "Service port number" }
                    deriving (Generic)

instance ParseRecord (DumboArgs Wrapped)

data DumboRTC = DRTC {
                     tex2svgRtc :: Tex2SvgRuntime
                     ,portN :: Int
                     }
                    
a ?: b = fromMaybe a b


initialize :: DumboArgs Unwrapped -> IO DumboRTC
initialize dargs = do
  latexP   <- maybe findLatex mkLatex (Main.latex dargs)
  dvisvgmP <- maybe findDVISVGM mkDVISVGM (Main.dvisvgm dargs)
  let cacheFN :: Integer -> String
      cacheFN i = cacheDir dargs ++ "/" ++ triep (show i)
      triep :: String -> FilePath
      triep (a:b:c:[]) = (a:b:c:".cache")
      triep (a:b:c:xs) = (a:b:c:'/':triep xs)
      triep xs = xs
      cacheRead :: Integer -> IO (Maybe PDC.Inline)
      cacheRead i = do
        e <- doesFileExist (cacheFN i)
        if e then decode <$> LBS.readFile (cacheFN i) else pure Nothing
      cacheWrite :: Integer -> PDC.Inline -> IO ()
      cacheWrite i inline = createDirectoryIfMissing True (takeDirectory (cacheFN i)) >>
                            LBS.writeFile (cacheFN i) (encode inline)
  return $ DRTC (T2SR 
                cacheRead
                cacheWrite
                (tmpDir dargs)
                latexP
                dvisvgmP)
                (port dargs)

main :: IO ()
main = do
  args <- unwrapRecord "Dumboest markdown converter"
  rtc  <- initialize args
  httpServe (setPort (portN rtc) mempty)
    $   route
          [ ("mdkeys"    , method POST (transformKeys rtc ToHTML))
          , ("markdown"  , method POST (transformMD   rtc ToHTML))
          , ("latexkeys" , method POST (transformKeys rtc ToLatex))
          , ("latex"     , method POST (transformMD   rtc ToLatex))
          ]
    <|> ifTop (method POST (transformMD rtc ToHTML))

transformKeys rtc target = conversion (keysConvert rtc target)
transformMD rtc target   = conversion (plainConvert rtc target)
-- conversion :: (TIMIFace (V.Vector a0) -> a0 -> IO b0) -> m0 ()
keysConvert :: DumboRTC -> Conversion -> TIMIFace a -> (ConversionElement Value) -> IO Value
keysConvert rtc target timInput 
 = ((\(pass,obj) -> transformM (jsonEditing target pass) obj) . convertElement rtc (mathOption timInput) (mathPreamble timInput))

plainConvert rtc target timInput 
 = (uncurry (convertBlock target) . convertElement rtc (mathOption timInput) (mathPreamble timInput))

modifyJSON target pass str = (String . stripP . LT.toStrict) <$> (convertBlock target pass str)

jsonEditing :: Conversion -> (PDC.Inline -> IO PDC.Inline) -> Value -> IO Value
jsonEditing target pass (String str)
  | "md:" `T.isPrefixOf` str
    -- ^ Convert field from markdown to target
  = modifyJSON target pass (T.drop 3 str)
  | "am:" `T.isPrefixOf` str
  = modifyJSON target (pass <=< convertAsciiMath) (T.drop 3 str)
    -- ^ Convert field from md to target, but assume that math is asciiMath
jsonEditing _ _ x = pure x

convertAsciiMath (PDC.Math a s) =
  pure $ either (const (PDC.Math a s)) (PDC.Math a) (AsciiMath.compile s)
convertAsciiMath x = pure x

mkErr :: String -> String -> Value
mkErr s e = object ["error" .= e, "stage" .= s]

convertElement rtc globalMath globalPreamble ce 
  = case ce of
        CEBare txt 
          | (defMathOption ?: globalMath) == SVG 
              -> (tex2SvgPass rtc (defPreamble ?: globalPreamble) , txt)
          | otherwise
              -> (pure ,  txt)

        CEWrapped obj
          | (defMathOption ?: globalMath ?: mathOption obj) == SVG 
              -> (tex2SvgPass rtc (defPreamble ?: globalPreamble ?: mathPreamble obj) , content obj)
          | otherwise 
              -> (pure , content obj)

conversion :: (MonadIO m0, ToJSON b0,FromJSON a0,MonadSnap m0) => 
                (TIMIFace (V.Vector a0) -> a0 -> IO b0) -> m0 ()
conversion f = do
  bdy <- eitherDecode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
  case bdy of
    Left  err      -> writeLBS . encode $ mkErr "Decoding input" err
    Right timInput -> liftIO (V.mapM (f timInput) (content timInput)) >>= writeLBS . encode 

data MathOption = SVG | MathJAX deriving (Eq,Show)

defMathOption :: MathOption
defMathOption = MathJAX

defPreamble :: T.Text
defPreamble = ""

instance FromJSON MathOption where
    parseJSON (String txt) = case txt of
                              "svg"     -> pure SVG
                              "mathjax" -> pure MathJAX
                              _         -> fail ("Unexpected math option"++show txt)
    parseJSON invalid    = typeMismatch "MathOption" invalid

data ConversionElement el = 
                         CEBare el
                       | CEWrapped (TIMIFace el)
                       deriving (Eq,Show)

instance FromJSON el => FromJSON (ConversionElement el) where
    parseJSON o = (CEWrapped <$> parseJSON o) <|> (CEBare <$> parseJSON o)
    parseJSON invalid    = typeMismatch "ConversionElement" invalid

data TIMIFace el = TIF {content      :: el
                       ,mathOption   :: Maybe MathOption
                       ,mathPreamble :: Maybe T.Text}
                       deriving (Eq,Show,Functor,Foldable,Traversable)

instance FromJSON el => FromJSON (TIMIFace el) where
    parseJSON (Object v) = do
                            content <- v .: "content"
                            mathOpt <- optional (v .: "mathOption")
                            mathPre <- optional (v .: "mathPreamble")
                            return (TIF content mathOpt mathPre)

    parseJSON invalid    = typeMismatch "TIMIFace" invalid

                                    


