{
module Parser (parseAscii) where

import Prelude hiding (EQ, LT, GT)

import Ast
import Exception
import Lexer
}

%name parseAscii
%tokentype { (Token, Position) }
%monad { Either AsciimathException } { thenE } { \x -> Right x }

%token
  RAW         { (RAW _, _) }
  LETTER      { (LETTER _, _) }
  NUM         { (NUM _, _) }
  LDEL        { (LDEL _, _) }
  RDEL        { (RDEL _, _) }
  '/'         { (SLASH, _) }
  '_'         { (UNDERSCORE, _) }
  '^'         { (SUPER, _) }
  GREEK       { (GREEK _, _) }
  STDFUN      { (STDFUN _, _) }
  SQRT        { (SQRT, _) }
  TEXT        { (TEXT, _) }
  BB          { (BB, _) }
  BBB         { (BBB, _) }
  UCC         { (UCC, _) }
  TT          { (TT, _) }
  FR          { (FR, _) }
  SF          { (SF, _) }
  FRAC        { (FRAC, _) }
  ROOT        { (ROOT, _) }
  STACKREL    { (STACKREL, _) }
  ADD         { (ADD, _) }
  SUB         { (SUB, _) }
  MUL         { (MUL, _) }
  MMUL        { (MMUL, _) }
  MMMUL       { (MMMUL, _) }
  SSLASH      { (SSLASH, _) }
  BBSLASH     { (BBSLASH, _) }
  TIMES       { (TIMES, _) }
  DIV         { (DIV, _) }
  COMP        { (COMP, _) }
  OPLUS       { (OPLUS, _) }
  OTIMES      { (OTIMES, _) }
  ODOT        { (ODOT, _) }
  SUM         { (SUM, _) }
  PROD        { (PROD, _) }
  WEDGE       { (WEDGE, _) }
  WWEDGE      { (WWEDGE, _) }
  VV          { (VV, _) }
  VVV         { (VVV, _) }
  NN          { (NN, _) }
  NNN         { (NNN, _) }
  UU          { (UU, _) }
  UUU         { (UUU, _) }
  INT         { (INT, _) }
  OINT        { (OINT, _) }
  DEL         { (DEL, _) }
  GRAD        { (GRAD, _) }
  ADDSUB      { (ADDSUB, _) }
  VOID        { (VOID, _) }
  INFTY       { (INFTY, _) }
  ALEPH       { (ALEPH, _) }
  ANGLE       { (ANGLE, _) }
  THEREFORE   { (THEREFORE, _) }
  ABS         { (ABS, _) }
  CDOTS       { (CDOTS, _) }
  VDOTS       { (VDOTS, _) }
  DDOTS       { (DDOTS, _) }
  BSLASH      { (BSLASH, _) }
  QUAD        { (QUAD, _) }
  SPACE       { (SPACE, _) }
  DIAMOND     { (DIAMOND, _) }
  SQUARE      { (SQUARE, _) }
  LFLOOR      { (LFLOOR, _) }
  RFLOOR      { (RFLOOR, _) }
  LCEIL       { (LCEIL, _) }
  RCEIL       { (RCEIL, _) }
  CC          { (CC, _) }
  ENSNN       { (ENSNN, _) }
  QQ          { (QQ, _) }
  RR          { (RR, _) }
  ZZ          { (ZZ, _) }
  EQ          { (EQ, _) }
  NEQ         { (NEQ, _) }
  LT          { (LT, _) }
  GT          { (GT, _) }
  LE          { (LE, _) }
  GE          { (GE, _) }
  PREC        { (PREC, _) }
  SUCC        { (SUCC, _) }
  IN          { (IN, _) }
  NOTIN       { (NOTIN, _) }
  SUBSET      { (SUBSET, _) }
  SUPSET      { (SUPSET, _) }
  SUBSETE     { (SUBSETE, _) }
  SUPSETE     { (SUPSETE, _) }
  MOD         { (MOD, _) }
  CONGR       { (CONGR, _) }
  APPROX      { (APPROX, _) }
  PROP        { (PROP, _) }
  AND         { (AND, _) }
  OR          { (OR, _) }
  NOT         { (NOT, _) }
  IMPLIES     { (IMPLIES, _) }
  IF          { (IF, _) }
  IFF         { (IFF, _) }
  FORALL      { (FORALL, _) }
  EXISTS      { (EXISTS, _) }
  FALSUM      { (FALSUM, _) }
  TAUT        { (TAUT, _) }
  TURNSTILE   { (TURNSTILE, _) }
  TTURNSTILE  { (TTURNSTILE, _) }
  UARR        { (UARR, _) }
  DARR        { (DARR, _) }
  LARR        { (LARR, _) }
  TO          { (TO, _) }
  MAPSTO      { (MAPSTO, _) }
  HARR        { (HARR, _) }
  LLARR       { (LLARR, _) }
  TILDE       { (TILDE, _) }
  HAT         { (HAT, _) }
  BAR         { (BAR, _) }
  UL          { (UL, _) }
  VEC         { (VEC, _) }
  DOTOP       { (DOTOP, _) }
  DDOT        { (DDOT, _) }
  COMMA       { (COMMA, _) }
  DOT         { (DOT, _) }
  SEMICOLON   { (SEMICOLON, _) }
  QUOTE       { (QUOTE, _) }
  FACTO       { (FACTO, _) }

%%

code:
                { [] }
    | expr code { $1 : $2 }

expr:
    simpleExpr  { let (SimpleExpr _ pos) = $1 in Expr (Simple $1) pos }
    | simpleExpr '/' simpleExpr
                { let (SimpleExpr _ p1) = $1 in
                  let (SimpleExpr _ p3) = $3 in
                  Expr (Frac $1 $3) (pextr p1 p3) }
    | simpleExpr '_' simpleExpr
                { let (SimpleExpr _ p1) = $1 in
                  let (SimpleExpr _ p2) = $3 in
                  Expr (Under $1 $3) (pextr p1 p2) }
    | simpleExpr '^' simpleExpr
                { let (SimpleExpr _ p1) = $1 in
                  let (SimpleExpr _ p2) = $3 in
                  Expr (Super $1 $3) (pextr p1 p2) }
    | simpleExpr '_' simpleExpr '^' simpleExpr
                { let (SimpleExpr _ p1) = $1 in
                  let (SimpleExpr _ p5) = $5 in
                  Expr (SubSuper $1 $3 $5) (pextr p1 p5) }

const:
    LETTER        { let (LETTER s, p) = $1 in Constant (Letter s) p }
    | NUM         { let (NUM n, p) = $1 in Constant (Number n) p }
    | GREEK       { let (GREEK s, p) = $1 in Constant (GreekLetter s) p }
    | STDFUN      { let (STDFUN s, p) = $1 in Constant (StdFun s) p }
    -- Operation symbols
    | ADD         { cst $1 Add }
    | SUB         { cst $1 Sub }
    | MUL         { cst $1 Mul }
    | MMUL        { cst $1 Mmul }
    | MMMUL       { cst $1 Mmmul }
    | SSLASH      { cst $1 Sslash }
    | BBSLASH     { cst $1 Bbslash }
    | TIMES       { cst $1 Times }
    | DIV         { cst $1 Div }
    | COMP        { cst $1 Comp }
    | OPLUS       { cst $1 Oplus }
    | OTIMES      { cst $1 Otimes }
    | ODOT        { cst $1 Odot }
    | SUM         { cst $1 Sum }
    | PROD        { cst $1 Prod }
    | WEDGE       { cst $1 Wedge }
    | WWEDGE      { cst $1 Wwedge }
    | VV          { cst $1 Vv }
    | VVV         { cst $1 Vvv }
    | NN          { cst $1 Nn }
    | NNN         { cst $1 Nnn }
    | UU          { cst $1 Uu }
    | UUU         { cst $1 Uuu }
    -- Miscellaneous symbols
    | INT         { cst $1 Inte }
    | OINT        { cst $1 Oint }
    | DEL         { cst $1 Del }
    | GRAD        { cst $1 Grad }
    | ADDSUB      { cst $1 Addsub }
    | VOID        { cst $1 Void }
    | INFTY       { cst $1 Infty }
    | ALEPH       { cst $1 Aleph }
    | ANGLE       { cst $1 Angle }
    | THEREFORE   { cst $1 Therefore }
    | ABS         { cst $1 Abs }
    | CDOTS       { cst $1 Cdots }
    | VDOTS       { cst $1 Vdots }
    | DDOTS       { cst $1 Ddots }
    | BSLASH      { cst $1 Bslash }
    | QUAD        { cst $1 Quad }
    | SPACE       { cst $1 Space }
    | DIAMOND     { cst $1 Diamond }
    | SQUARE      { cst $1 Square }
    | LFLOOR      { cst $1 Lfloor }
    | RFLOOR      { cst $1 Rfloor }
    | LCEIL       { cst $1 Lceil }
    | RCEIL       { cst $1 Rceil }
    | CC          { cst $1 Cc }
    | ENSNN       { cst $1 Ensnn }
    | QQ          { cst $1 Qq }
    | RR          { cst $1 Rr }
    | ZZ          { cst $1 Zz }
    -- Relation symbols
    | EQ          { cst $1 Eq }
    | NEQ         { cst $1 Neq }
    | LT          { cst $1 Lt }
    | GT          { cst $1 Gt }
    | LE          { cst $1 Le }
    | GE          { cst $1 Ge }
    | PREC        { cst $1 Prec }
    | SUCC        { cst $1 Succ }
    | IN          { cst $1 In }
    | NOTIN       { cst $1 Notin }
    | SUBSET      { cst $1 Subset }
    | SUPSET      { cst $1 Supset }
    | SUBSETE     { cst $1 Subsete }
    | SUPSETE     { cst $1 Supsete }
    | MOD         { cst $1 Mod }
    | CONGR       { cst $1 Congr }
    | APPROX      { cst $1 Approx }
    | PROP        { cst $1 Prop }
    -- Logical symbols
    | AND         { cst $1 And }
    | OR          { cst $1 Or }
    | NOT         { cst $1 Not }
    | IMPLIES     { cst $1 Implies }
    | IF          { cst $1 If }
    | IFF         { cst $1 Iff }
    | FORALL      { cst $1 Forall }
    | EXISTS      { cst $1 Exists }
    | FALSUM      { cst $1 Falsum }
    | TAUT        { cst $1 Taut }
    | TURNSTILE   { cst $1 Turnstile }
    | TTURNSTILE  { cst $1 Tturnstile }
    -- Arrows
    | UARR        { cst $1 Uarr }
    | DARR        { cst $1 Darr }
    | LARR        { cst $1 Larr }
    | TO          { cst $1 To }
    | MAPSTO      { cst $1 Mapsto }
    | HARR        { cst $1 Harr }
    | LLARR       { cst $1 Llarr }
    -- Additionnal tokens
    | COMMA       { cst $1 Comma }
    | DOT         { cst $1 Dot }
    | SEMICOLON   { cst $1 Semicolon }
    | QUOTE       { cst $1 Quote }
    | FACTO       { cst $1 Facto }

op1:
    SQRT        { op1 $1 Usqrt }
    | TEXT      { op1 $1 Utext }
    | BB        { op1 $1 Ubb }
    | BBB       { op1 $1 Ubbb }
    | UCC       { op1 $1 Ucc }
    | TT        { op1 $1 Utt }
    | FR        { op1 $1 Ufr }
    | SF        { op1 $1 Usf }
    | TILDE     { op1 $1 Utilde }
    | HAT       { op1 $1 Uhat }
    | BAR       { op1 $1 Ubar }
    | UL        { op1 $1 Uul }
    | VEC       { op1 $1 Uvec }
    | DOTOP     { op1 $1 Udot }
    | DDOT      { op1 $1 Uddot }

op2:
    FRAC        { op2 $1 BFrac }
    | ROOT      { op2 $1 BRoot }
    | STACKREL  { op2 $1 BStackRel }

lDel : LDEL     { let (LDEL s, p) = $1 in LBracket (ldel s) p }

rDel : RDEL     { let (RDEL s, p) = $1 in RBracket (rdel s) p }

simpleExpr:
    const       { let (Constant _ p) = $1 in
                  SimpleExpr (SEConst $1) p }
    | lDel code rDel
                { let (LBracket _ p1) = $1 in
                  let (RBracket _ p3) = $3 in
                  SimpleExpr (Delimited $1 $2 $3) (pextr p1 p3) }
    | op1 simpleExpr
                { let (UnaryOp _ p1) = $1 in
                  let (SimpleExpr _ p2) = $2 in
                  SimpleExpr (UnaryApp $1 $2) (pextr p1 p2) }
    | op2 simpleExpr simpleExpr
                { let (BinaryOp _ p1) = $1 in
                  let (SimpleExpr _ p3) = $3 in
                  SimpleExpr (BinaryApp $1 $2 $3) (pextr p1 p3) }
    | RAW       { let (RAW s, p) = $1 in SimpleExpr (Raw s) p }

{

thenE :: Either AsciimathException a -> (a -> Either AsciimathException b) -> Either AsciimathException b
thenE (Left err) _ = Left err
thenE (Right x) f = f x

happyError tokens =
  let (tok, pos) = head tokens in
  Left $ LexicalError (show tok) pos

-- Conversion
cst :: (Token, Position) -> Constant_ -> Constant
cst (_, p) c = Constant c p

op1 :: (Token, Position) -> UnaryOp_ -> UnaryOp
op1 (_, p) u = UnaryOp u p

op2 :: (Token, Position) -> BinaryOp_ -> BinaryOp
op2 (_, p) b = BinaryOp b p

-- builds a new position which begining is the begining of the first argument
-- and which ending is the ending of the second argument
pextr :: Position -> Position -> Position
pextr (PositionElement a1 l c len1) (PositionElement a2 _ _ len2) =
    PositionElement a1 l c (len1 + (a2 - a1 - 1) + len2)

-- Conversion
rdel :: String -> RBracket_
rdel ")" = RPar
rdel "]" = RCro
rdel "}" = RBra
rdel ":)" = RChe
rdel ":}" = RBraCons

ldel :: String -> LBracket_
ldel "(" = LPar
ldel "[" = LCro
ldel "{" = LBra
ldel "(:" = LChe
ldel "{:" = LBraCons
}
