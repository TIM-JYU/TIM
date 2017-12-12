{-#OPTIONS_GHC -Wall#-}
{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections#-}
module Main where
import Data.Monoid
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import qualified Data.Vector as V
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Set as Set
import Control.Lens hiding ((.=))
import Snap.Core
import Data.Aeson
import Data.Aeson.Lens
import Snap.Http.Server
import Text.Blaze.Html.Renderer.Text
import Text.TeXMath.Readers.TeX.Macros
import Control.Applicative
import qualified AsciiMath

convert :: Conversion -> String -> T.Text -> Either String LT.Text
convert target macros t = do
  ms <- parsedMacros
  convertBlock target (applyThem ms) t
  where
    parsedMacros =
      case parseMacroDefinitions macros of
        (ms, "") -> Right ms
        (_, e) -> Left $ "Could not parse as a macro: " <> e
    applyThem :: [Macro] -> PDC.Inline -> PDC.Inline
    applyThem m (PDC.Math a xs) = PDC.Math a (applyMacros m xs)
    applyThem _ x = x

convertBlock :: Conversion -> (PDC.Inline -> PDC.Inline) -> T.Text -> Either String LT.Text
convertBlock target ms =
  either
    (Left . show)
    (Right . make . PDC.bottomUp ms) 
   . PDC.readMarkdown PDC.def . T.unpack
 where
  make = case target of 
        ToHTML  -> makeHtml
        ToLatex -> makeLatex
  makeHtml  = renderHtml . PDC.writeHtml htmlOpts
  makeLatex = LT.pack . PDC.writeLaTeX latexOpts

readerOpts :: PDC_Opt.ReaderOptions
readerOpts =
  PDC.def
  { PDC.readerExtensions =
    PDC.pandocExtensions `Set.union` Set.singleton PDC_Opt.Ext_latex_macros
  }

latexOpts :: PDC_Opt.WriterOptions
latexOpts = PDC.def

htmlOpts :: PDC_Opt.WriterOptions
htmlOpts =
  PDC.def
  { PDC.writerHTMLMathMethod =
    PDC.MathJax
      "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  , PDC.writerExtensions =
    PDC.pandocExtensions `Set.union` Set.singleton PDC_Opt.Ext_latex_macros
  }

stripSome :: (T.Text -> T.Text -> Maybe T.Text) -> T.Text -> T.Text -> T.Text
stripSome f piece str = fromMaybe str (f piece str)

stripP :: T.Text -> T.Text
stripP = stripSome T.stripSuffix "</p>" . stripSome T.stripPrefix "<p>"

data Conversion = ToHTML | ToLatex deriving (Eq,Show)

main :: IO ()
main =
  quickHttpServe $
   route [("mdkeys", method POST mdkeys)
         ,("markdown", method POST markd)
         ,("latexkeys", method POST latexkeys)
         ,("latex", method POST latex)

         ] <|>
   ifTop (method POST markd)
  where
    modifyJSON target c str = either
          (String . stripP  . T.pack) -- This tries to strip latex also..
          (String . stripP . LT.toStrict)
          (convertBlock target c str)

    jsonEditing target (String str)
      | "md:" `T.isPrefixOf` str = modifyJSON target id (T.drop 3 str) 
        -- ^ Convert field from markdown to target
      | "am:" `T.isPrefixOf` str = modifyJSON target convertAsciiMath (T.drop 3 str) 
        -- ^ Convert field from md to target, but assume that math is asciiMath
    jsonEditing _ x = x

    convertAsciiMath (PDC.Math a s) = either (const (PDC.Math a s)) (PDC.Math a) (AsciiMath.compile s) 
    convertAsciiMath x = x

    -- | Modify all marked fields in JSON
    mdkeys = transformKeys ToHTML
    latexkeys = transformKeys ToLatex

    transformKeys target = do
      bdy <- decode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      writeLBS . encode $
        case bdy of
          Nothing ->
            object ["error" .= ("Could not decode input" :: T.Text)]
          Just val -> transform (jsonEditing target) val

    -- Convert a bunch of document fragments into target language
    markd = doConversion ToHTML
    latex = doConversion ToLatex

    doConversion target = do
      bdy <- decode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      writeLBS $
        case bdy of
          Nothing ->
            encode $ object ["error" .= ("Could not decode input" :: T.Text)]
          Just l@(Array _) ->
            encode $ l ^.. _Array . traverse . _String . to (convert target []) .
            to (either err (String . LT.toStrict))
          Just o@(Object _) ->
            encode $
            case o ^? key "macros" . _String . to T.unpack of
              Nothing -> err $ "No macros in " <> show bdy
              Just macros ->
                let texts =
                      o ^.. key "texts" . _Array . traverse . _String .
                      to (convert target macros) .
                      to (either err (String . LT.toStrict))
                in Array (V.fromList texts)
          Just _ ->
            encode . err $ "Expected an object or array, got " <> show bdy
      where
        err e = object ["error" .= e]
                                    
                                    
                                    

