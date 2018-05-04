module Ast where
import Lexer (Position(..))

-- Constants : variables, numbers, etc.
data Constant_ =
  Letter Char
  | Number Int
  | GreekLetter String
  | StdFun String
  -- Operation symbols
  | Add | Sub | Mul | Mmul | Mmmul | Sslash | Bbslash
  | Times | Div | Comp | Oplus | Otimes | Odot
  | Sum | Prod | Wedge | Wwedge | Vv | Vvv | Nn | Nnn | Uu | Uuu
  -- Miscellaneous symbols
  | Inte | Oint | Del | Grad | Addsub | Void | Infty | Aleph
  | Angle | Therefore | Abs | Cdots | Vdots | Ddots | Bslash
  | Quad | Diamond | Square | Lfloor | Rfloor | Lceil | Rceil
  | Cc | Ensnn | Qq | Rr | Zz | Space
  -- Relation symbols
  | Eq | Neq | Lt | Gt | Le | Ge | Prec | Succ
  | In | Notin | Subset | Supset | Subsete | Supsete
  | Mod | Congr | Approx | Prop
  -- Logical symbols
  | And | Or | Not | Implies | If | Iff | Forall | Exists
  | Falsum | Taut | Turnstile | Tturnstile
  -- Arrows
  | Uarr | Darr | Larr | To
  | Mapsto | Harr | Llarr
  -- Additionnal symbols
  | Comma | Dot | Semicolon | Quote | Facto
  deriving (Show, Eq)
data Constant = Constant Constant_ Position deriving (Show, Eq)

-- Unary operators
data UnaryOp_ =
  Usqrt | Utext
  | Ubb | Ubbb | Ucc | Utt | Ufr | Usf
  | Utilde | Uhat | Ubar | Uul | Uvec | Udot | Uddot
  deriving (Show, Eq)
data UnaryOp = UnaryOp UnaryOp_ Position deriving (Show, Eq)

-- Binary operators
data BinaryOp_ = BFrac | BRoot | BStackRel deriving (Show, Eq)
data BinaryOp = BinaryOp BinaryOp_ Position deriving (Show, Eq)

-- Left brackets
data LBracket_ = LPar | LCro | LBra | LChe | LBraCons deriving (Show, Eq)
data LBracket = LBracket LBracket_ Position deriving (Show, Eq)

-- Right brackets
data RBracket_ = RPar | RCro | RBra | RChe | RBraCons deriving (Show, Eq)
data RBracket = RBracket RBracket_ Position deriving (Show, Eq)

-- Matrix type
data MatrixType = RawMatrix | ColMatrix deriving (Show, Eq)

-- Simple expressions
data SimpleExpr_ =
  SEConst Constant
  | Delimited LBracket Code RBracket
  | Matrix MatrixType [[Code]]
  | UnaryApp UnaryOp SimpleExpr
  | BinaryApp BinaryOp SimpleExpr SimpleExpr
  | Raw String  -- raw text, redered in a \textrm
  deriving(Show, Eq)
data SimpleExpr = SimpleExpr SimpleExpr_ Position deriving (Show, Eq)

-- Global expressions
data Expr_ =
  Simple SimpleExpr
  | Frac SimpleExpr SimpleExpr
  | Under SimpleExpr SimpleExpr
  | Super SimpleExpr SimpleExpr
  | SubSuper SimpleExpr SimpleExpr SimpleExpr
  deriving (Show, Eq)
data Expr = Expr Expr_ Position deriving (Show, Eq)

-- Whole asciimath code
type Code = [Expr]
