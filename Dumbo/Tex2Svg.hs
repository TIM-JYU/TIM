{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-#OPTIONS_GHC -Wall -fno-warn-name-shadowing -fwarn-incomplete-patterns#-}

module Tex2Svg (tex2svg,Tex2SvgRuntime( .. ),mathEnvsSet,findDVISVGM,findLatex,mkLatex,mkDVISVGM,LatexPath,DVISVGMPath,ConversionError(..),SetupError(..),MathType(..)) where
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad                   (unless, void)
import           Data.Aeson                      (encode)
import           Data.Attoparsec.Text.Lazy       as AP
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base64.Lazy     as B64
import qualified Data.ByteString.Lazy            as LBS
import           Data.Digest.Pure.SHA
import           Data.List.NonEmpty              hiding (map, span)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text.Lazy.Encoding         as LT
import           Data.Typeable
import qualified HTMLEntities.Text               as UENC
import           Numeric
import           Prelude                         hiding (head, span)
import           System.Directory
import           System.Exit
import           System.IO.Temp
import           System.Process
import           System.Timeout
import           Text.Blaze                      (toValue)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html5                hiding (head, map, option,
                                                  string, style, title)
import           Text.Blaze.Html5.Attributes     (class_, src, style,
                                                  title)
import           Text.Pandoc.JSON

specialCmd :: String
specialCmd
  = "\\special{dvisvgm:rawdef <style type=\"text/css\"><![CDATA[path {stroke: currentColor;stroke-width: 0.05pt;}]]></style>}"

type MathEnvResult = Either String MathEnv

latexTemplate :: String -> MathType -> String -> MathEnvResult -> String
latexTemplate preamble _ eqn (Right (MathEnv "tikzpicture")) = unlines
  [
    -- workaround for l3kernel bug, see
    -- https://tex.stackexchange.com/questions/529129
    "\\ExplSyntaxOn"
  , "\\str_clear:N \\c_sys_backend_str"
  , "\\ExplSyntaxOff"

  , "\\documentclass[crop,dvisvgm]{standalone}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amsfonts}"
  , "\\usepackage{amssymb}"
  , "\\usepackage{tikz}"
  , preamble
  , "\\begin{document}"
  , eqn
  , "\\end{document}"
  ]

latexTemplate preamble mt eqn env = unlines
  [ "\\documentclass{article}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{amsfonts}"
  , "\\usepackage{amssymb}"
  , "\\usepackage[paperwidth=7.7in]{geometry}"
  , preamble
  , "\\usepackage[active,dvips,displaymath,textmath,tightpage]{preview}"
  , "\\renewcommand{\\arraystretch}{1.5}"
  , "\\begin{document}"
  , case mt of
    InlineMath  -> "$" ++ specialCmd ++ eqn ++ "$"
    DisplayMath -> case env of
      Right _ -> "\\begin{preview}" ++ specialCmd ++ eqn ++ "\\end{preview}"
      Left  _ -> "$$" ++ specialCmd ++ eqn ++ "$$"
  , "\\end{document}"
  ]

mathEnvs :: [T.Text]
mathEnvs =
  [ "split"
  , "eqnarray"
  , "equation"
  , "multline"
  , "gather"
  , "align"
  , "alignat"
  , "flalign"
  , "tikzpicture"
  ]

mathEnvsWithStarred :: [T.Text]
mathEnvsWithStarred = map (<> "*") mathEnvs <> mathEnvs

mathEnvsSet :: S.Set String
mathEnvsSet = S.fromList $ map T.unpack mathEnvsWithStarred

parseMathEnv :: Parser MathEnv
parseMathEnv = do
  skipSpace
  _ <- string "\\begin{"
  skipSpace
  envname <- choice $ map string mathEnvs
  _       <- option "" (string "*")
  _       <- string "}"
  return $ MathEnv envname

parseBlock :: Parser EqnOut
parseBlock = do
  preLine
  _ <- choice [void procMsg, procPdf]
  m <-
    choice [skipLine *> metricsLine <* metricsLineNoDepth, metricsLineNoDepth]
      <?> "Metrics line"
  emptyPageNote
  o <- outputLine
  return $ EqnOut (LT.unpack (LT.fromStrict o)) m

emptyPageNote :: Parser ()
emptyPageNote =
  option () (skipSpace *> string "page is empty" *> skipLine)
    <?> "Empty page message"

preLine :: Parser ()
preLine = option
  ()
  ((string "pre-processing DVI file" *> skipLine) <?> "Pre-processing message")

procMsg :: Parser Int
procMsg =
  (string "processing page " *> decimal <* endOfLine)
    <?> "Processing page message"

procPdf :: Parser ()
procPdf =
  (string "processing PDF file" *> skipLine) <?> "Processing PDF message"

outputLine :: Parser T.Text
outputLine =
  (skipSpace *> string "output written to " *> takeTill isEndOfLine <* endOfLine
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
  _ <- string "graphic size:"
  skipSpace
  width <- double
  _     <- string "pt"
  skipSpace
  _ <- string "x"
  skipSpace
  height <- double
  _      <- string "pt"
  skipLine
  return $ Metrics width height Nothing

newtype MathEnv = MathEnv {_name :: T.Text} deriving (Eq,Show)
data EqnOut  = EqnOut {filename::FilePath, metrics :: !Metrics} deriving (Eq,Show)
data Metrics = Metrics {width,_height :: !Double, depth :: Maybe Double} deriving (Eq,Show)
newtype DVISVGMPath = DVISVGM {getDVISVGM :: FilePath} deriving (Eq,Show)
newtype LatexPath = LaTex {getLatex :: FilePath} deriving (Eq,Show)

findDVISVGM :: IO DVISVGMPath
findDVISVGM =
  DVISVGM <$> (fromMaybe (throw NoDVISVGM) <$> findExecutable "dvisvgm")
findLatex :: IO LatexPath
findLatex = LaTex <$> (fromMaybe (throw NoLatex) <$> findExecutable "xelatex")

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

getLatexParams :: String -> MathEnvResult -> [String]
getLatexParams fn (Right (MathEnv "tikzpicture")) =
  ["-no-pdf", "-jobname=" ++ fn, "-halt-on-error"]
getLatexParams fn _ = ["-no-pdf", "-jobname=" ++ fn, "-halt-on-error"]

doConvert
  :: FilePath
  -> DVISVGMPath
  -> LatexPath
  -> String
  -> MathType
  -> String
  -> IO Inline
doConvert curTmpDir dvisvgm latex preamble mt eqn = do
  let fn        = showDigest . sha1 . LT.encodeUtf8 . LT.pack $ eqn
      env       = parseOnly parseMathEnv (T.pack eqn)
      latexFile = latexTemplate preamble mt eqn env
  _ <- timeoutException 5000000 LaTexTimeouted $ readProcessException
    LaTexFailed
    curTmpDir
    (getLatex latex)
    (getLatexParams fn env)
    latexFile
  eb  <- head <$> invokeDVISVGM curTmpDir dvisvgm fn mt env
  svg <- BS.readFile (curTmpDir ++ "/" ++ filename eb)
  return . force . wrapMath mt $ createSVGImg eb (LBS.fromStrict svg) eqn


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
  CouldNotParseDVISVGM dvi err ->
    mkErr "Could not parse DVISVGM output" (err ++ "\n" ++ dvi)
  DVISVGMTimeouted        -> mkErr "DVISVGM timeouted" ""
  DVISVGMFailed _ err out -> mkErr "DVISVGM failed" (err ++ "\n" ++ out)
  LaTexTimeouted          -> mkErr "Latex timeouted" ""
  LaTexFailed _ err out   -> mkErr "Latex failed" (err ++ "\n" ++ out)
  UnexpectedError w       -> mkErr "Something unexpected happened" w
 where
  teqn = T.pack eqn
  mkErr e diagnostics = createError teqn (T.pack $ e ++ "\n" ++ diagnostics)

createError :: T.Text -> T.Text -> Inline
createError eqn le =
  RawInline (Format "html")
    $  "<details class=\"latex-error\"><summary>"
    ++ entity eqn
    ++ "</summary>"
    ++ "<pre>"
    ++ entity le
    ++ "</pre></details>"
  where entity = T.unpack . UENC.text

timeoutException :: Exception e => Int -> e -> IO b -> IO b
timeoutException n exc process = do
  res <- timeout n process
  case res of
    Nothing -> throw exc
    Just f  -> pure f

--doInTemporary :: (NFData a) => FilePath -> IO a -> IO a

--doInTemporary base op = evaluate . force =<<

--                            (withTempDirectory base "temp_" (\tmpDir -> evaluate . force =<< withCurrentDirectory tmpDir op))


readProcessException
  :: Exception e
  => (ExitCode -> String -> String -> e)
  -> FilePath
  -> FilePath
  -> [String]
  -> String
  -> IO (String, String)
readProcessException toExc curTmp process args stdinContent = do
  let theProcess = (proc process args) { cwd = Just curTmp }
  res <- readCreateProcessWithExitCode theProcess stdinContent
  case res of
    (e@(ExitFailure _), a, b) -> throw (toExc e a b)
    (ExitSuccess      , a, b) -> return (a, b)

getDVISVGMParams :: String -> MathEnvResult -> MathType -> [String]
getDVISVGMParams fn (Right (MathEnv "tikzpicture")) _ =
  [ "--progress=9007199254740992"
  , "-p1-"
  , "--output=" ++ fn ++ "_%p"
  , "--no-fonts"
  , "--exact"
  , "--bbox=papersize"
  , fn ++ ".xdv"
  ]
getDVISVGMParams fn _ mt =
  [ "--progress=9007199254740992"
  , "-p1-"
  , "--output=" ++ fn ++ "_%p"
  , "--no-fonts"
  , "--exact"
  , "--bbox=" ++ (if mt == InlineMath then "preview" else "1")
  , fn ++ ".xdv"
  ]

invokeDVISVGM
  :: FilePath
  -> DVISVGMPath
  -> FilePath
  -> MathType
  -> MathEnvResult
  -> IO (NonEmpty EqnOut)
invokeDVISVGM curTmp dvisvgm fn mt env = do
  (_, dvi) <- timeoutException 5000000 DVISVGMTimeouted $ readProcessException
    DVISVGMFailed
    curTmp
    (getDVISVGM dvisvgm)
    (getDVISVGMParams fn env mt)
    ""
  case parseOnly (many1 parseBlock) (LT.toStrict $ LT.pack dvi) of
    Right []       -> throw (UnexpectedError "many1 returned none")
    Right (x : xs) -> pure (x :| xs)
    Left  err      -> throw (CouldNotParseDVISVGM dvi err)

_addStylesheet :: LBS.ByteString -> LBS.ByteString
_addStylesheet svg = LBS.append
  (LT.encodeUtf8
  . LT.pack
  $ "<?xml version='1.0' encoding='UTF-8'?><?xml-stylesheet href=\"/static/svgmath.css\" type=\"text/css\"?>"
  )
  (LBS.dropWhile (/= 10) svg)

createSVGImg :: EqnOut -> LBS.ByteString -> String -> Html
createSVGImg eb svg eqn =
  img
    ! style
        (toValue $ "width:" <> tm width <> "; " <> "vertical-align:-" <> tm
          ((/ 1.0) . fromMaybe 0 . depth)
        )
    ! src (toValue $ "data:image/svg+xml;base64," <> encode svg)
    ! title (toValue eqn)
 where
  encode = LT.unpack . LT.decodeUtf8 . B64.encode -- .  ZLib.compress

  tm f = ($ "em") . showFFloat (Just 5) . toEm . f . metrics $ eb
  toEm = estimatePtToEm ""

wrapMath :: MathType -> Html -> Inline
wrapMath mt svg =
  RawInline (Format "html")
    $ renderHtml
    $ span
    ! class_ ("mathp " <> (if mt == InlineMath then "inline" else "display"))
    $ svg

data Tex2SvgRuntime = T2SR {
      readCache  :: Integer -> IO (Maybe Inline)
    , writeCache :: Integer -> Inline -> IO ()
    , tmp        :: FilePath
    , latex      :: LatexPath
    , dvisvgm    :: DVISVGMPath}

estimatePtToEm :: T.Text -> (Double -> Double)
estimatePtToEm _preamble = (/ 8.3645834169792) --TODO


tex2svg :: Tex2SvgRuntime -> T.Text -> Inline -> IO Inline
tex2svg T2SR {..} preamble (Math mt math) = do
  let cacheKey = integerDigest . sha1 . encode $ (mt, math, preamble) -- TODO: Preamble needed here

  cached <- readCache cacheKey
  case cached of
    Nothing -> withTempDirectory tmp "tex2svg." $ \curTmpDir -> do
      svg' <- catch
        (doConvert curTmpDir dvisvgm latex (T.unpack preamble) mt math)
        (\e -> return (renderConversionError math (e :: ConversionError)))
      writeCache cacheKey svg'
      return svg'
    Just s -> pure s

tex2svg _ _ il = return il
