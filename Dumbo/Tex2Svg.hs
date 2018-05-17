{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-#OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-incomplete-patterns#-}

module Tex2Svg (tex2svg,Tex2SvgRuntime( .. ),findDVISVGM,findLatex,mkLatex,mkDVISVGM,LatexPath,DVISVGMPath,ConversionError(..),SetupError(..),MathType(..)) where
import Prelude hiding (head)
import Text.Pandoc.JSON
import System.Exit
import Data.Typeable
import Control.Exception
import System.Process
import System.Directory
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty hiding (map)
import Control.DeepSeq
import System.Directory (findExecutable,withCurrentDirectory)
import System.Timeout
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Base64.Lazy as B64
import qualified HTMLEntities.Text as UENC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Attoparsec.Text.Lazy as AP
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LT
import System.IO.Temp
import Data.Aeson (encode)
import Data.Monoid
import Numeric

specialCmd :: String
specialCmd = "\\special{dvisvgm:rawdef <style type=\"text/css\"><![CDATA[path {stroke: currentColor;stroke-width: 0.05pt;}]]></style>}"

latexTemplate :: String -> MathType -> String -> String
latexTemplate preamble mt  eqn = unlines [
         "\\documentclass{article}"
        ,"\\usepackage{amsmath}"
        ,"\\usepackage{amsfonts}"
        ,"\\usepackage{amssymb}"
        ,"\\usepackage[paperwidth=7.7in]{geometry}"
        ,preamble
        ,"\\usepackage[active,dvips,displaymath,textmath,tightpage]{preview}"
        ,"\\renewcommand{\\arraystretch}{1.5}"
        ,"\\begin{document}"
        , case mt of
            InlineMath -> "$" ++ specialCmd ++ eqn ++ "$"
            DisplayMath -> case parseOnly parseMathEnv (T.pack eqn) of
                               Right _ -> "\\begin{preview}" ++ specialCmd ++ eqn ++ "\\end{preview}"
                               Left _ -> "$$" ++ specialCmd ++ eqn ++ "$$"
        ,"\\end{document}"]

parseMathEnv :: Parser MathEnv
parseMathEnv = do
    skipSpace
    string "\\begin{"
    skipSpace
    envname <- choice $ map string ["split", "eqnarray", "equation", "multline", "gather", "align", "alignat", "flalign"]
    _ <- option "" (string "*")
    string "}"
    return $ MathEnv envname

parseBlock :: Parser EqnOut
parseBlock = do
    preLine
    _ <- procMsg
    m <- choice [skipLine *> metricsLine <* metricsLineNoDepth, metricsLineNoDepth] <?> "Metrics line"
    emptyPageNote
    o <- outputLine
    return $ EqnOut (LT.unpack (LT.fromStrict o)) m

emptyPageNote :: Parser ()
emptyPageNote = option () (skipSpace *> string "page is empty" *> skipLine)

preLine :: Parser ()
preLine = option
  ()
  ((string "pre-processing DVI file" *> skipLine) <?> "Pre-processing message")

procMsg :: Parser Int
procMsg =
  (string "processing page " *> decimal <* endOfLine) <?> "Processing message"

outputLine :: Parser T.Text
outputLine =
  (  skipSpace
    *> string "output written to "
    *> takeTill isEndOfLine
    <* endOfLine
    )
    <?> "Output line"

skipLine :: Parser ()
skipLine = (skipWhile (not . isEndOfLine) *> endOfLine) <?> "Junkline"

metricItem :: T.Text -> Parser Double
metricItem i =
  skipSpace
    *> (string i <?> "fieldname")
    *> char '='
    *> double
    <* ("pt" <?> "pt")

comma :: Parser ()
comma = (skipSpace *> char ',' *> skipSpace) <?> "a comma"

-- width=480.311pt, height=172.12pt, depth=0pt
metricsLine :: Parser Metrics
metricsLine = skipSpace *> do
  width  <- metricItem "width" <* comma
  height <- metricItem "height" <* comma
  option () (string "page is empty" *> endOfLine)
  depth <- metricItem "depth"
  endOfLine
  return $ Metrics width height (Just depth)

-- graphic size: 480.311pt x 172.12pt (168.81mm x 60.4932mm)
metricsLineNoDepth :: Parser Metrics
metricsLineNoDepth = do
  skipSpace
  string "graphic size:"
  skipSpace
  width <- double
  string "pt"
  skipSpace
  string "x"
  skipSpace
  height <- double
  string "pt"
  skipLine
  return $ Metrics width height Nothing

data MathEnv = MathEnv {name :: T.Text} deriving (Eq,Show)
data EqnOut  = EqnOut {filename::FilePath, metrics :: !Metrics} deriving (Eq,Show)
data Metrics = Metrics {width,height :: !Double, depth :: Maybe Double} deriving (Eq,Show)
newtype DVISVGMPath = DVISVGM {getDVISVGM :: FilePath} deriving (Eq,Show)
newtype LatexPath = LaTex {getLatex :: FilePath} deriving (Eq,Show)

findDVISVGM :: IO DVISVGMPath
findDVISVGM = DVISVGM <$> (fromMaybe (throw NoDVISVGM) <$> findExecutable "dvisvgm")
findLatex :: IO LatexPath
findLatex = LaTex   <$> (fromMaybe (throw NoLatex)   <$> findExecutable "xelatex")

mkDVISVGM :: FilePath -> IO DVISVGMPath 
mkDVISVGM fp = do
    e <- doesFileExist fp 
    unless e (throw NoDVISVGM)
    pure (DVISVGM fp)

mkLatex :: FilePath -> IO LatexPath
mkLatex fp = do
    e <- doesFileExist fp 
    unless e (throw NoLatex)
    pure (LaTex fp)

doConvert :: FilePath -> DVISVGMPath -> LatexPath -> String -> MathType -> String -> IO Inline
doConvert curTmpDir dvisvgm latex preamble mt eqn = do
   let fn = showDigest . sha1 . LT.encodeUtf8 . LT.pack $ eqn
       latexFile = latexTemplate preamble mt eqn
   _ <- timeoutException 2000000 LaTexTimeouted $
        readProcessException LaTexFailed curTmpDir (getLatex latex) ["-no-pdf", "-jobname="++fn,"-halt-on-error"] latexFile
   eb <- head <$> invokeDVISVGM curTmpDir dvisvgm fn mt
   svg <- BS.readFile (curTmpDir++"/"++filename eb)
   return . force . wrapMath mt $ createSVGImg eb (LBS.fromStrict svg)


data ConversionError = CouldNotParseDVISVGM String String 
                     | DVISVGMTimeouted 
                     | DVISVGMFailed ExitCode String String
                     | LaTexTimeouted
                     | LaTexFailed ExitCode String String
                     | UnexpectedError String
                 deriving (Eq,Show,Typeable)
instance Exception ConversionError


data SetupError = 
                  NoLatex
                 | NoDVISVGM
                 deriving (Eq,Show,Typeable)
instance Exception SetupError

renderConversionError :: String -> ConversionError -> Inline
renderConversionError eqn e = case e of
   CouldNotParseDVISVGM dvi err  -> mkErr "Could not parse DVISGM output" (err++"\n"++dvi)
   DVISVGMTimeouted              -> mkErr "DVISVGM timeouted" "" 
   DVISVGMFailed _ err out       -> mkErr "DVISVGM failed" (err++"\n"++out)
   LaTexTimeouted                -> mkErr "Latex timeouted" ""
   LaTexFailed _ err out         -> mkErr "Latex failed" (err++"\n"++out)
   UnexpectedError w             -> mkErr "Something unexpected happened" w
 where 
    teqn = T.pack eqn
    mkErr e diagnostics = createError teqn (T.pack $ e++"\n"++diagnostics) 

createError :: T.Text -> T.Text -> Inline
createError eqn le =  RawInline (Format "html") $
              "<details class=\"latex-error\"><summary>"++entity eqn++"</summary>"
              ++"<pre>"++entity le++"</pre></details>"
    where entity = T.unpack . UENC.text

timeoutException :: Exception e => Int -> e -> IO b -> IO b
timeoutException n exc process = do
    res <- timeout n process
    case res of
        Nothing -> throw exc
        Just f -> pure f

--doInTemporary :: (NFData a) => FilePath -> IO a -> IO a
--doInTemporary base op = evaluate . force =<< 
--                            (withTempDirectory base "temp_" (\tmpDir -> evaluate . force =<< withCurrentDirectory tmpDir op))

readProcessException :: Exception e =>
                        (ExitCode -> String -> String -> e)
                        -> FilePath -> FilePath -> [String] -> String -> IO (String, String)
readProcessException toExc curTmp process args stdinContent = do  
    let theProcess = (proc process args){cwd=Just curTmp}
    res <- readCreateProcessWithExitCode theProcess stdinContent
    case res of
        (e@(ExitFailure _),a,b) -> throw (toExc e a b)
        (ExitSuccess,a,b) -> return (a,b)

invokeDVISVGM :: FilePath -> DVISVGMPath -> FilePath -> MathType -> IO (NonEmpty EqnOut)
invokeDVISVGM curTmp dvisvgm fn mt = do
  (_, dvi) <- timeoutException 1000000 DVISVGMTimeouted $ readProcessException
    DVISVGMFailed
    curTmp
    (getDVISVGM dvisvgm)
    ["-p1-", "--output=" ++ fn ++ "_%p", "--no-fonts", "--exact", "--bbox=" ++ (if mt == InlineMath then "preview" else "1"), fn ++ ".xdv"]
    ""
  case parseOnly (many1 parseBlock) (LT.toStrict $ LT.pack dvi) of
    Right []     -> throw (UnexpectedError "many1 returned none")
    Right (x:xs) -> pure (x :| xs)
    Left  err    -> throw (CouldNotParseDVISVGM dvi err)

addStylesheet :: LBS.ByteString -> LBS.ByteString
addStylesheet svg = LBS.append (LT.encodeUtf8 . LT.pack $ "<?xml version='1.0' encoding='UTF-8'?><?xml-stylesheet href=\"/static/svgmath.css\" type=\"text/css\"?>") (LBS.dropWhile (\c -> c /= 10) svg)

createSVGImg :: EqnOut -> LBS.ByteString -> String
createSVGImg eb svg = "<img style='"
                      <> "width:" <> tm width <> "; "
                      <> "vertical-align:-"   <> tm ((/1.0) . (fromMaybe 0) . depth) <>";' "
                      <> "src=\"data:image/svg+xml;base64," <> encode svg <> "\" />"
    where 
        encode = LT.unpack . LT.decodeUtf8 . B64.encode -- .  ZLib.compress
        tm f = ($"em") . showFFloat (Just 5) . toEm . f . metrics $ eb
        toEm   = estimatePtToEm "" 

wrapMath :: MathType -> String -> Inline
wrapMath mt svg = RawInline (Format "html") $
               "<span class=\"mathp " ++
               (if mt == InlineMath then "inline" else "display") ++ "\">" ++
               svg ++ "</span>"

data Tex2SvgRuntime = T2SR {
      readCache   :: (Integer -> IO (Maybe Inline)) 
    , writeCache  :: (Integer -> Inline -> IO ())   
    , tmp         :: FilePath 
    , latex       :: LatexPath 
    , dvisvgm     :: DVISVGMPath}

estimatePtToEm :: T.Text -> (Double -> Double)
estimatePtToEm preamble = (/8.3645834169792) --TODO
            
tex2svg :: Tex2SvgRuntime ->
             T.Text -> Inline -> IO Inline
tex2svg T2SR{..} preamble (Math mt math) = do
  let cacheKey = integerDigest . sha1 . encode $ (mt,math,preamble) -- TODO: Preamble needed here
  cached <- readCache cacheKey
  case cached of
    Nothing -> do
                 withTempDirectory tmp "tex2svg." $ \curTmpDir -> do
                         svg' <- catch (doConvert curTmpDir dvisvgm latex (T.unpack preamble) mt math) 
                                       (\e -> return (renderConversionError math (e::ConversionError)))
                         writeCache cacheKey svg'
                         return svg'
    Just s -> pure s

tex2svg _ _ il = return il 


