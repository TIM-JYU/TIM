{-#OPTIONS_GHC -Wall#-}
{-#LANGUAGE OverloadedStrings, ScopedTypeVariables, TupleSections#-}
module Main where
import Data.Monoid
import qualified Text.Pandoc as PDC
import qualified Text.Pandoc.Options as PDC_Opt
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Set as Set
import Control.Lens hiding ((.=))
import qualified Data.Yaml as YAML
import Snap.Core
import Data.Aeson
import Data.Aeson.Lens
import Snap.Http.Server
import Text.Blaze.Html.Renderer.Text
import Text.TeXMath.Readers.TeX.Macros
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative

convert :: String -> T.Text -> Either String LT.Text
convert macros t = do
  ms <- parsedMacros
  convertBlock (applyThem ms) t
  where
    parsedMacros =
      case parseMacroDefinitions macros of
        (ms, "") -> Right ms
        (_, e) -> Left $ "Could not parse as a macro: " <> e
    applyThem :: [Macro] -> PDC.Inline -> PDC.Inline
    applyThem m (PDC.Math a xs) = PDC.Math a (applyMacros m xs)
    applyThem _ x = x

convertBlock :: (PDC.Inline -> PDC.Inline) -> T.Text -> Either String LT.Text
convertBlock ms =
  either
    (Left . show)
    (Right . renderHtml . PDC.writeHtml htmlOpts . PDC.bottomUp ms) .
  PDC.readMarkdown PDC.def . T.unpack



readerOpts :: PDC_Opt.ReaderOptions
readerOpts =
  PDC.def
  { PDC.readerExtensions =
    PDC.pandocExtensions `Set.union` Set.singleton PDC_Opt.Ext_latex_macros
  }

htmlOpts :: PDC_Opt.WriterOptions
htmlOpts =
  PDC.def
  { PDC.writerHTMLMathMethod =
    PDC.MathJax
      "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  , PDC.writerExtensions =
    PDC.pandocExtensions `Set.union` Set.singleton PDC_Opt.Ext_latex_macros
  }

main :: IO ()
main =
  quickHttpServe $
  route [("yaml", method POST yaml), ("markdown", method POST markd)] <|>
  ifTop (method POST markd)
  where
    yamlRule (String str)
      | "MD:" `T.isPrefixOf` str =
        either
          (String . T.pack)
          (String . LT.toStrict)
          (convertBlock id (T.drop 3 str))
    yamlRule x = x
    yaml = do
      bdy <- (YAML.decode . LBS.toStrict) `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      writeBS . YAML.encode $
        case bdy of
          Nothing ->
            YAML.object ["error" .= ("Could not decode input" :: T.Text)]
          Just val -> transform yamlRule val
    markd = do
      bdy <- decode `fmap` readRequestBody 1000000000 -- Allow up to gigabyte at once..
      writeLBS $
        case bdy of
          Nothing ->
            encode $ object ["error" .= ("Could not decode input" :: T.Text)]
          Just l@(Array _) ->
            encode $ l ^.. _Array . traverse . _String . to (convert []) .
            to (either err (String . LT.toStrict))
          Just o@(Object _) ->
            encode $
            case o ^? key "macros" . _String . to T.unpack of
              Nothing -> err $ "No macros in " <> show bdy
              Just macros ->
                let texts =
                      o ^.. key "texts" . _Array . traverse . _String .
                      to (convert macros) .
                      to (either err (String . LT.toStrict))
                in Array (V.fromList texts)
          Just _ ->
            encode . err $ "Expected an object or array, got" <> show bdy
      where
        err e = object ["error" .= e]
                                    
                                    
                                    

