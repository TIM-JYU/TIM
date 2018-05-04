{-#OPTIONS_GHC -Wall#-}
{-#LANGUAGE OverloadedStrings, ScopedTypeVariables#-}
{-#LANGUAGE DataKinds#-}
{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE StandaloneDeriving#-}
{-#LANGUAGE TypeOperators#-}
{-#LANGUAGE DeriveGeneric#-}
{-#LANGUAGE DeriveDataTypeable#-}
{-#LANGUAGE DeriveFunctor#-}
{-#LANGUAGE DeriveFoldable#-}
{-#LANGUAGE DeriveTraversable#-}
module Main where
import Data.Monoid
import GHC.Generics
import Control.Exception
import Data.Typeable
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import System.Directory
import qualified Data.Vector as V
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Set as Set
import Control.Lens (transformM)
import qualified Data.ByteString.Lazy as LBS
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

convert :: DumboRTC ->  Conversion -> MathOption -> T.Text -> T.Text -> IO (Either String LT.Text)
convert drtc target mathOption mathPreamble t = convertBlock target mauler t
 where 
    mauler | target == ToHTML && mathOption == SVG 
             = tex2svg (cacheRead drtc) (cacheWrite drtc) (tmp drtc) (latexPath drtc) (dvisvgmPath drtc)
           | otherwise = pure

convertBlock :: Conversion -> (PDC.Inline -> IO PDC.Inline) -> T.Text -> IO (Either String LT.Text)
convertBlock target ms txt = fmap (either (Left . show) Right) <$> PDC.runIO $ do
  x <-   PDC.readMarkdown PDC.def $ txt
  make x
 where
  make :: PDC.PandocMonad m => PDC.Pandoc -> m LT.Text
  make = case target of 
        ToHTML  -> makeHtml
        ToLatex -> makeLatex
  makeHtml, makeLatex :: PDC.PandocMonad m => PDC.Pandoc -> m LT.Text
  makeHtml  pdc  = fmap renderHtml (PDC.writeHtml5 htmlOpts pdc)
  makeLatex pdc =  LT.fromStrict <$> PDC.writeLaTeX latexOpts pdc

readerOpts :: PDC_Opt.ReaderOptions
readerOpts =
  PDC.def
  { PDC.readerExtensions = PDC_Opt.enableExtension  PDC_Opt.Ext_latex_macros PDC.pandocExtensions 
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
                      ,cacheDir  :: f ::: Maybe FilePath <?> "Directory for cached math"
                      ,tmpDir    :: f ::: Maybe FilePath <?> "Directory to store temporary files"}
                    deriving (Generic)

instance ParseRecord (DumboArgs Wrapped)

data DumboRTC = DRTC {latexPath   :: LatexPath
                     ,dvisvgmPath :: DVISVGMPath    
                     ,cacheRead :: Integer -> IO (Maybe PDC.Inline)
                     ,cacheWrite :: Integer -> PDC.Inline -> IO ()
                     ,tmp :: FilePath}
                    


-- verify :: DumboArgs -> IO DumboRTC
initialize :: DumboArgs Unwrapped-> IO DumboRTC
initialize drtc = do
                latex   <- maybe findLatex   mkLatex   (latex drtc)
                dvisvgm <- maybe findDVISVGM mkDVISVGM (dvisvgm drtc)
                let cacheFN :: Integer -> String
                    cacheFN i = fromMaybe "." (cacheDir drtc) ++ "/" ++ show i
                    cacheRead :: Integer -> IO (Maybe PDC.Inline)
                    cacheRead i = do
                                e <- doesFileExist (cacheFN i)
                                if e then decode <$> LBS.readFile (cacheFN i) 
                                     else pure Nothing
                    cacheWrite :: Integer -> PDC.Inline-> IO ()
                    cacheWrite i inline = LBS.writeFile (cacheFN i) (encode inline)
                return $ DRTC latex dvisvgm cacheRead cacheWrite (fromMaybe "/tmp/" (tmpDir drtc))
                

main :: IO ()
main = do
  args <- unwrapRecord "Dumboest markdown converter"
  rtc <- initialize args
  quickHttpServe $
   route [("mdkeys", method POST (transformKeys rtc ToHTML))
         ,("markdown", method POST (doConversion rtc ToHTML))
         ,("latexkeys", method POST (transformKeys rtc ToLatex))
         ,("latex", method POST (doConversion rtc ToLatex))

         ] <|> ifTop (method POST (doConversion rtc ToHTML))
  where
    modifyJSON target c str = either
          (String . stripP  . T.pack) -- This tries to strip latex also..
          (String . stripP . LT.toStrict)
          <$> (convertBlock target c str)

    jsonEditing :: Conversion -> Value -> IO Value
    jsonEditing target (String str)
      | "md:" `T.isPrefixOf` str = modifyJSON target pure (T.drop 3 str) 
        -- ^ Convert field from markdown to target
      | "am:" `T.isPrefixOf` str = modifyJSON target convertAsciiMath (T.drop 3 str) 
        -- ^ Convert field from md to target, but assume that math is asciiMath
    jsonEditing _ x = pure x

    convertAsciiMath (PDC.Math a s) = pure $ either (const (PDC.Math a s)) (PDC.Math a) (AsciiMath.compile s) 
    convertAsciiMath x = pure x

    transformKeys rtc target = do
      bdy <- decode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      case bdy of
          Nothing -> writeLBS . encode $
            object ["error" .= ("Could not decode input" :: T.Text)]
          Just val -> liftIO (transformM (jsonEditing target) val) >>= writeLBS . encode

    -- Convert a bunch of document fragments into target language
    doConversion rtc target = do
      bdy <- eitherDecode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      let mkErr :: String -> String -> Value
          mkErr s e = object ["error" .= e,"stage" .=s]
          convertElement globalMath globalPreamble ce = case ce of
                                CEString txt -> convert rtc target 
                                                        (fromMaybe defMathOption globalMath) 
                                                        (fromMaybe defPreamble   globalPreamble) 
                                                        txt
                                CEObject obj -> convert rtc target
                                                        (fromMaybe defMathOption (mathOption  obj  <|> globalMath))
                                                        (fromMaybe defPreamble   (mathPreamble obj <|> globalPreamble))
                                                        (content obj)
                                            
                
      writeLBS $ case bdy of
            Left err       -> encode $ mkErr "Decoding input" err
            Right timInput -> encode $ V.map 
                                        ( (either (mkErr "conversion") (String . LT.toStrict) :: Either String LT.Text -> Value)
                                        . (undefined :: ConversionElement -> Either String LT.Text)) 
                                        (content timInput)  
        

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

data ConversionElement = CEString T.Text
                       | CEObject (TIMIFace T.Text)
                       deriving (Eq,Show)
                   
instance FromJSON ConversionElement where
    parseJSON (String txt) = pure (CEString txt)
    parseJSON o@(Object _) = CEObject <$> parseJSON o
    parseJSON invalid    = typeMismatch "ConversionElement" invalid

data TIMIFace el = TIF {content      :: el
                       ,mathOption   :: Maybe MathOption
                       ,mathPreamble :: Maybe T.Text}
                       deriving (Eq,Show,Functor,Foldable,Traversable)

instance FromJSON el => FromJSON (TIMIFace el) where
    parseJSON (Object v) = do
                            content      <- v .: "content"
                            mathOption   <- optional (v .: "mathOption")
                            mathPreamble <- optional (v .: "mathPreamble")
                            return (TIF content mathOption mathPreamble)

    parseJSON invalid    = typeMismatch "TIMIFace" invalid

                                    


