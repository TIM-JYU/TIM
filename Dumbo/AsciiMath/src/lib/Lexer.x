{
{-# LANGUAGE DeriveDataTypeable #-}
module Lexer (get_tokens, Token(..), Position(..), LexicalError(..)) where
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Exception (Exception)
import System.IO.Unsafe (unsafePerformIO)
import System.Exit (exitFailure)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Typeable
import Prelude hiding (abs, EQ, LT, GT)
import qualified Data.Bits

}

-- Macros definitions
$alpha = [a-zA-Z]
$digit = [0-9]

$escaped = [\< \> \; \']

@ldel = "(" | "[" | "{" | "(:" | "{:"
@rdel = ")" | "]" | "}" | ":)" | ":}"
@sym1 = "+" | "*" | "-" | "/" | "@" | "|" | "," | \. | \\ | \^
      | = | \_ | $escaped | "!"
@ident = $alpha+
@text = (@ident | ' ')+

-- The lexer rules
tokens :-
  <0>       $white+     ;
  <0>       @ident      { \_ s -> (check_kw s, 0) }
  <0>       $digit+     { \_ s -> (NUM (read s), 0) }
  <0>       @ldel       { \_ s -> (LDEL s, 0) }
  <0>       @rdel       { \_ s -> (RDEL s, 0) }
  <0>       @sym1       { \_ s -> (check_sym1 s, 0) }
  <0>       \\\         { cst SPACE }
  <0>       "+-"        { cst ADDSUB }
  <0>       "**"        { cst MMUL }
  <0>       "//"        { cst SSLASH }
  <0>       \\\\        { cst BBSLASH }
  <0>       "-:"        { cst DIV }
  <0>       "o+"        { cst OPLUS }
  <0>       "o."        { cst ODOT }
  <0>       \^\^        { cst WEDGE }
  <0>       "O/"        { cst VOID }
  <0>       "/_"        { cst ANGLE }
  <0>       ":."        { cst THEREFORE }
  <0>       "|~"        { cst LCEIL }
  <0>       "~|"        { cst RCEIL }
  <0>       "!="        { cst NEQ }
  <0>       "<="        { cst LE }
  <0>       ">="        { cst GE }
  <0>       "-<"        { cst PREC }
  <0>       ">-"        { cst SUCC }
  <0>       "-="        { cst MOD }
  <0>       "~="        { cst CONGR }
  <0>       "~~"        { cst APPROX }
  <0>       "=>"        { cst IMPLIES }
  <0>       "->"        { cst TO }
  <0>       "***"       { cst MMMUL }
  <0>       \^\^\^      { cst WWEDGE }
  <0>       "|__"       { cst LFLOOR }
  <0>       "__|"       { cst RFLOOR }
  <0>       "!in"       { cst NOTIN }
  <0>       "_|_"       { cst FALSUM }
  <0>       "|--"       { cst TURNSTILE }
  <0>       "|->"       { cst MAPSTO }
  <0>       "|=="       { cst TTURNSTILE }
  <0>       \"          { \_ _ -> (WHITE, string) }
  <string>  @text       { \_ s -> (RAW s, string) }
  <string>  \"          { cst WHITE }

{
-- Token type
data Token =
  RAW String
  | WHITE
  | LETTER Char | LETTERS_ String -- Temporary token
  | NUM Int
  | LDEL String
  | RDEL String
  | SLASH | UNDERSCORE | SUPER
  -- Greek letters
  | GREEK String 
  -- Standard functions
  | STDFUN String
  -- Unary ops
  | SQRT | TEXT | BB | BBB | UCC | TT | FR | SF
  --Binary ops
  | FRAC | ROOT | STACKREL
  -- Operation symbols
  | ADD | SUB | MUL | MMUL | MMMUL | SSLASH | BBSLASH
  | TIMES | DIV | COMP | OPLUS | OTIMES | ODOT
  | SUM | PROD | WEDGE | WWEDGE | VV | VVV | NN | NNN | UU | UUU
  -- Miscellaneous symbols
  | INT | OINT | DEL | GRAD | ADDSUB | VOID | INFTY | ALEPH
  | ANGLE | THEREFORE | ABS | CDOTS | VDOTS | DDOTS | BSLASH
  | QUAD | DIAMOND | SQUARE | LFLOOR | RFLOOR | LCEIL | RCEIL
  | CC | ENSNN | QQ | RR | ZZ | SPACE
  -- Relation symbols
  | EQ | NEQ | LT | GT | LE | GE | PREC | SUCC
  | IN | NOTIN | SUBSET | SUPSET | SUBSETE | SUPSETE
  | MOD | CONGR | APPROX | PROP
  -- Logical symbols
  | AND | OR | NOT | IMPLIES | IF | IFF | FORALL | EXISTS
  | FALSUM | TAUT | TURNSTILE | TTURNSTILE
  -- Arrows
  | UARR | DARR | LARR | TO
  | MAPSTO | HARR | LLARR
  -- Accents
  | HAT | BAR | UL | VEC | DOTOP | DDOT 
  -- Additionnal tokens 
  | COMMA | DOT | SEMICOLON | QUOTE | FACTO
  deriving (Show)

cst :: t -> (Position -> String -> (t, Int))
cst x = (\_ _ -> (x, 0))

-- Maps of keywords
kws :: M.Map String Token
kws = M.fromList [
  -- Unary ops
  ("sqrt", SQRT), ("text", TEXT),
  ("bb", BB),     ("bbb", BBB),  ("cc", UCC),
  ("tt", TT),     ("fr", FR),    ("sf", SF),
  -- Binary ops
  ("frac", FRAC),
  ("root", ROOT),
  ("stackrel", STACKREL),
  -- Operation symbols
  ("xx", TIMES), ("ox", OTIMES), ("sum", SUM), ("prod", PROD),
  ("vv", VV), ("vvv", VVV), ("nn", NN), ("nnn", NNN), ("uu", UU), ("uuu", UUU),
  -- Miscellaneous symbols
  ("int", INT), ("oint", OINT), ("del", DEL), ("grad", GRAD),
  ("oo", INFTY), ("aleph", ALEPH),
  ("cdots", CDOTS), ("vdots", VDOTS), ("ddots", DDOTS),
  ("quad", QUAD), ("diamond", DIAMOND), ("square", SQUARE),
  ("CC", CC), ("NN", ENSNN), ("QQ", QQ), ("RR", RR), ("ZZ", ZZ),
  -- Relation symbols
  ("in", IN), ("sub", SUBSET), ("sup", SUPSET),
  ("sube", SUBSETE), ("supe", SUPSETE), ("prop", PROP),
  -- Logical symbols
  ("and", AND), ("or", OR), ("not", NOT), ("if", IF), ("iff", IFF),
  ("AA", FORALL), ("EE", EXISTS), ("TT", TAUT),
  -- Arrows
  ("uarr", UARR), ("darr", DARR), ("rarr", TO), ("larr", LARR),
  ("harr", HARR), ("rArr", IMPLIES), ("lArr", LLARR), ("hArr", IFF),
  -- Accents
  ("hat", HAT), ("bar", BAR), ("ul", UL),
  ("vec", VEC), ("dot", DOTOP), ("ddot", DDOT)]

greek_letters :: S.Set String
greek_letters = S.fromList [
  "alpha", "beta", "chi", "delta", "Delta",
  "epsilon", "varepsilon", "eta", "gamma", "Gamma",
  "iota", "kappa", "lambda", "Lambda", "mu", "nu",
  "omega", "Omega", "phi", "Phi", "varphi", "pi", "Pi",
  "psi", "Psi", "rho", "sigma", "Sigma", "tau", "theta", "Theta",
  "vartheta", "upsilon", "xi", "Xi", "zeta"]

std_fun :: S.Set String
std_fun = S.fromList [
  "sin", "cos", "tan", "csc", "sec", "cot",
  "sinh", "cosh", "tanh", "log", "ln", "exp", "det", "dim", "lim", "mod",
  "gcd", "lcm", "min", "max"]

-- Checks wether a string has a meaning or is just a sequence of variables
check_kw :: String -> Token
check_kw s = case M.lookup s kws of
    Just tok -> tok
    Nothing ->
        if S.member s greek_letters then
          GREEK s
        else if S.member s std_fun then
            STDFUN s
          else
            LETTERS_ s

sym1 :: M.Map String Token
sym1 = M.fromList [
  ("+", ADD), ("-", SUB), ("*", MUL), ("\\", BSLASH), ("/", SLASH),
  ("@", COMP), ("|", ABS), ("_", UNDERSCORE), ("^", SUPER), 
  ("=", EQ), ("<", LT), (">", GT), (",", COMMA), (".", DOT), ("\\", BSLASH),
  ("^", SUPER), ("=", EQ), ("_", UNDERSCORE), (";", SEMICOLON), ("'", QUOTE),
  ("!", FACTO)]

-- Associates a Token to matched characters
check_sym1 :: String -> Token
check_sym1 s = case M.lookup s sym1 of
    Just tok -> tok
    Nothing -> error ("'" ++ s ++ "' is supposed to be recognised")

-- The main function
get_tokens :: String -> Either LexicalError [(Token, Position)]
get_tokens = alexScanTokens


-- beyond these line : the lexer definition -----------------------------------


-----------------------------
-- Some useful definitions --
-----------------------------

type Byte = Word8

-- Location
-- abs, line, column, length
data Position =
  Position !Int !Int !Int
  | PositionElement !Int !Int !Int !Int
  deriving (Show, Eq)

-- Lexical Error
data LexicalError = LexicalError String Position deriving (Typeable)
instance Show LexicalError where
          show (LexicalError msg (PositionElement _ l c len)) =
            unsafePerformIO $ do {
            putStr ("Line "
              ++ (show l) ++ ", characters "
              ++ (show c) ++ "-" ++ (show $ c + len) ++ ":\n"
              ++ "lexical error near: \"" ++ msg ++ "\"\n");
            exitFailure;
            }
          show (LexicalError msg (Position _ l c)) = 
            unsafePerformIO $ do {
            putStr ("Line " ++ show l
              ++ ", characters " ++ show c ++ "-" ++ show (c+1) ++ ":\n"
              ++ "lexical error near: " ++ msg ++ "\n");
            exitFailure;
            }
instance Exception LexicalError

-- Shortens strings
cut :: String -> String
cut s = case splitAt 20 s of
    (txt, "") -> txt
    (txt, _) -> take 17 txt ++ "..."

--------------------
-- The Lexer core --
--------------------

-- Type of input
type AlexInput = (Position,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- rest of the bytes for the current char
                  String)       -- current input string

-- Alex' misc functions
ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_,_) = c

-- Encoding
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord 
 where
  go oc
   | oc <= 0x7f   = [oc]
   | oc <= 0x7ff  = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                    , 0x80 + oc Data.Bits..&. 0x3f
                    ]   
   | oc <= 0xffff = [ 0xe0 + (oc `Data.Bits.shiftR` 12) 
                    , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                    , 0x80 + oc Data.Bits..&. 0x3f
                    ]   
   | otherwise    = [ 0xf0 + (oc `Data.Bits.shiftR` 18) 
                    , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                    , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                    , 0x80 + oc Data.Bits..&. 0x3f
                    ] 

-- Position initialisation
alexStartPos :: Position
alexStartPos = Position 0 1 1

-- Position handling in alex lexer
alexMove :: Position -> Char -> Position
alexMove (Position a l c) '\t' = Position (a+1) l (((c+7) `div` 8)*8+1)
alexMove (Position a l _) '\n' = Position (a+1) (l+1) 1
alexMove (Position a l c) _    = Position (a+1) l (c+1)
alexMove (PositionElement _ _ _ _) _ =
  error "alexMove is not supposed to receive such an argument."

-- An elementary step in the scanning process
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s)) =
  let p' = alexMove p c in
  let (b:bs) = utf8Encode c in
  p' `seq`  Just (b, (p', c, bs, s))

unletters :: String -> Position -> [(Token, Position)]
unletters "" _ = []
unletters (c:s) (PositionElement abs line col len) =
    let hd = (LETTER c, PositionElement abs line col 1) in
    hd:(unletters s $ PositionElement (abs+1) line (col+1) (len-1))
unletters (c:s) (Position abs line col) = 
    let hd = (LETTER c, Position abs line col) in
    hd:(unletters s $ Position (abs+1) line (col+1))

cat :: [a] -> Either e [a] -> Either e [a]
cat _ (Left e) = Left e
cat l (Right l') = Right $ l ++ l'

-- The scanner
alexScanTokens :: String -> Either LexicalError [(Token, Position)]
alexScanTokens s = go (alexStartPos,'\n',[],s) 0
  where go inp@(pos,_,_,str) sc =
          case alexScan inp sc of
              AlexEOF -> Right []
              AlexError (errPos,_,_,remaining) ->
                let msg = cut remaining in
                Left $ LexicalError msg errPos
              AlexSkip inp' _ -> go inp' sc
              AlexToken inp' len act ->
                let (tok, new_sc) = act pos (take len str) in
                let (Position abs line col) = pos in
                let elt_pos = PositionElement abs line col len in
                case tok of
                    WHITE -> go inp' new_sc
                    LETTERS_ w -> cat (unletters w elt_pos) $ go inp' new_sc
                    _ -> cat [(tok, elt_pos)] $ go inp' new_sc
}

