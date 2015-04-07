{-#LANGUAGE OverloadedStrings, TupleSections #-}
module SecSplit where
import Data.Attoparsec.Text as A hiding (feed)
import Control.Applicative
import Data.List
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid

type SrcPos = Sum Int
feed :: SrcPos -> T.Text -> SrcPos
feed pos x = pos <> Sum (T.length x) 
feeds :: SrcPos -> [T.Text] -> SrcPos
feeds pos = foldl' feed pos 

data Tokened = Heading SrcPos Int T.Text 
             | Body SrcPos T.Text
             deriving (Eq,Show,Ord)

body :: T.Text -> Tokened
body content = Body (mempty`feed`content) content

contents :: Tokened -> T.Text
contents (Heading _ _ t) = t
contents (Body  _ t) = t

foldPos :: [Tokened] -> [Tokened]
foldPos  = snd . (foldl' (\(sp,xs) x -> case x of
                      Heading sp' i t -> (sp<>sp',Heading (sp<>sp') i t:xs)
                      Body sp' t      -> (sp<>sp',Body    (sp<>sp') t  :xs))
                  (mempty,mempty))


printDocument :: [Tokened] -> IO ()
printDocument = mapM_ (\x -> case x of
                        Heading s i t -> putStr ("HEADING "<>show i<>" "<>show s)>>T.putStrLn t
                        Body s t      -> putStrLn ("BODY "<>show s)>>T.putStrLn t)

eol :: Parser T.Text
eol = (string "\n") <|> (string "\r\n")

document :: Parser [Tokened]
document = do 
            parts <- many (heading <|> codeBlock <|> notHeading) <|> (endOfInput>>return [])
            return $ reverse $ foldPos parts

--- | Parse a block of code into a Body element
codeBlock :: Parser Tokened
codeBlock = do
    (sym,st) <- (('~',) <$> "~~~") <|> (('`',) <$> "```")
    additional <- A.takeWhile (== sym)
    restOfHeader <- mappend <$> takeTill isEndOfLine <*> eol
    let blockTerm = do
                term   <- string (st <> additional)
                extras <- A.takeWhile (== sym)
                end <- eol
                return (term<>extras<>end)
    let bodyContent = do
                content <- takeTill (==sym)
                more <- eitherP blockTerm (T.cons <$> char sym <*> bodyContent)
                case more of
                    Left  theEnd  -> return (content<>theEnd)
                    Right hadMore -> return (content<>hadMore) 
    bodyContent' <- bodyContent
    return $ body (st<>additional<>restOfHeader<>bodyContent')

heading :: Parser Tokened
heading = do
    level  <- takeWhile1 (=='#')
    header <- takeTill isEndOfLine 
    e      <- eol
    return $ Heading (mempty `feed` level `feed` header `feed` e) (T.length level) (level<>header<>e)

line :: Parser (SrcPos,T.Text)
line = do
    crud     <- takeTill (=='\n')
    e        <- eol<|>(endOfInput*>return "")
    when (T.isPrefixOf "#" crud) (fail "Unexpected header")
    when (T.isPrefixOf "~~~" crud) (fail "Unexpected codeblock")
    when (T.isPrefixOf "```" crud) (fail "Unexpected codeblock")
    when (T.null crud&&T.null e) (fail "expected a line")
    return (mempty`feed`crud`feed`e,crud<>e) 

notHeading :: Parser Tokened
notHeading = do
    (sp, readlines) <- mconcat <$> many1 line
    return $ Body sp readlines

collate :: [Tokened] -> Tokened
collate  = foldl' (\(Body sp t) x -> case x of
                      Heading _ _ _  -> error "Internal failure"
                      Body sp' t'     -> Body    (sp<>sp') (t<>t'))
                  (Body mempty mempty)
