module TeXWriter (writeTeX) where
import Ast
import Data.List (intercalate)

cmd_ :: String -> String
cmd_ = ("\\" ++)

cmd :: String -> String
cmd = cmd_ . (++ " ")

cmdargs :: String -> [String] -> String
cmdargs c args =
  let args' = map (\a -> "{" ++ a ++ "}") args in
  concat $ (cmd_ c):args'

writeConst :: Constant -> String
writeConst (Constant cst _) = writeConst_ cst
              -- Operation symbols
        where writeConst_ (Letter c) = [c]
              writeConst_ (Number n) = show n
              writeConst_ (GreekLetter s) = cmd s
              writeConst_ (StdFun s) = cmd s
              writeConst_ Add = "+"
              writeConst_ Sub = "-"
              writeConst_ Mul = cmd "cdot"
              writeConst_ Mmul = cmd "ast"
              writeConst_ Mmmul = cmd "star"
              writeConst_ Sslash = "/"
              writeConst_ Bbslash = cmd "backslash"
              writeConst_ Times = cmd "times"
              writeConst_ Div = cmd "div"
              writeConst_ Comp = cmd "circ"
              writeConst_ Oplus = cmd "oplus"
              writeConst_ Otimes = cmd "otimes"
              writeConst_ Odot = cmd "odot"
              writeConst_ Sum = cmd "sum"
              writeConst_ Prod = cmd "prod"
              writeConst_ Wedge = cmd "wedge"
              writeConst_ Wwedge = cmd "bigwedge"
              writeConst_ Vv = cmd "vee"
              writeConst_ Vvv = cmd "bigvee"
              writeConst_ Nn = cmd "cap"
              writeConst_ Nnn = cmd "bigcap"
              writeConst_ Uu = cmd "cup"
              writeConst_ Uuu = cmd "bigcup"
              -- Miscellaneous symbols
              writeConst_ Inte = cmd "int"
              writeConst_ Oint = cmd "oint"
              writeConst_ Del = cmd "partial"
              writeConst_ Grad = cmd "nabla"
              writeConst_ Addsub = cmd "pm"
              writeConst_ Void = cmd "emptyset"
              writeConst_ Infty = cmd "infty"
              writeConst_ Aleph = cmd "aleph"
              writeConst_ Angle = cmd "angle"
              writeConst_ Therefore = cmd "therefore"
              writeConst_ Abs = "|"
              writeConst_ Cdots = cmd "cdots"
              writeConst_ Vdots = cmd "vdots"
              writeConst_ Ddots = cmd "ddots"
              writeConst_ Bslash = "\\"
              writeConst_ Quad = cmd "quad"
              writeConst_ Space = cmd_ " "
              writeConst_ Diamond = cmd "diamond"
              writeConst_ Square = cmd "square"
              writeConst_ Lfloor = cmd "lfloor"
              writeConst_ Rfloor = cmd "rfloor"
              writeConst_ Lceil = cmd "lceil"
              writeConst_ Rceil = cmd "rceil"
              writeConst_ Cc = cmdargs "mathbb" ["C"]
              writeConst_ Ensnn = cmdargs "mathbb" ["N"]
              writeConst_ Qq = cmdargs "mathbb" ["Q"]
              writeConst_ Rr = cmdargs "mathbb" ["R"]
              writeConst_ Zz = cmdargs "mathbb" ["Z"]
              -- Relation symbols
              writeConst_ Eq = "="
              writeConst_ Neq = cmd "neq"
              writeConst_ Lt = "<"
              writeConst_ Gt = ">"
              writeConst_ Le = cmd "leqslant"
              writeConst_ Ge = cmd "geqslant"
              writeConst_ Prec = cmd "prec"
              writeConst_ Succ = cmd "succ"
              writeConst_ In = cmd "in"
              writeConst_ Notin = cmd_ "not" ++ cmd "in"
              writeConst_ Subset = cmd "subset"
              writeConst_ Supset = cmd "supset"
              writeConst_ Subsete = cmd "subseteq"
              writeConst_ Supsete = cmd "supseteq"
              writeConst_ Mod = cmd "equiv"
              writeConst_ Congr = cmd "cong"
              writeConst_ Approx = cmd "approx"
              writeConst_ Prop = cmd "propto"
              -- Logical symbols
              writeConst_ And = cmdargs "textrm" ["and"]
              writeConst_ Or = cmdargs "textrm" ["or"]
              writeConst_ Not = cmd "neg"
              writeConst_ Implies = cmd "Rightarrow"
              writeConst_ If = cmdargs "textrm" ["if"]
              writeConst_ Iff = cmd "Leftrightarrow"
              writeConst_ Forall = cmd "forall"
              writeConst_ Exists = cmd "exists"
              writeConst_ Falsum = cmd "perp"
              writeConst_ Taut = cmd "top"
              writeConst_ Turnstile = cmd "vdash"
              writeConst_ Tturnstile = cmd "models"
              -- Arrows
              writeConst_ Uarr = cmd "uparrow"
              writeConst_ Darr = cmd "downarrow"
              writeConst_ Larr = cmd "leftarrow"
              writeConst_ To = cmd "to"
              writeConst_ Mapsto = cmd "mapsto"
              writeConst_ Harr = cmd "leftrightarrow"
              writeConst_ Llarr = cmd "Leftarrow"
              -- Additionnal symbols
              writeConst_ Comma = ","
              writeConst_ Dot = "."
              writeConst_ Semicolon = ";"
              writeConst_ Quote = "'"
              writeConst_ Facto = "!"

-- Writes a unary operator
writeUnaryOp :: UnaryOp -> String
writeUnaryOp (UnaryOp u _) = writeUnaryOp_ u
        where writeUnaryOp_ Usqrt = "sqrt"
              writeUnaryOp_ Utext = "textrm"
              writeUnaryOp_ Ubb = "boldsymbol"
              writeUnaryOp_ Ubbb = "mathbb"
              writeUnaryOp_ Ucc = "mathcal"
              writeUnaryOp_ Utt = "texttt"
              writeUnaryOp_ Ufr = "mathfrak"
              writeUnaryOp_ Usf = "mathsf"
              writeUnaryOp_ Uhat = "hat"
              writeUnaryOp_ Ubar = "overline"
              writeUnaryOp_ Uul = "underline"
              writeUnaryOp_ Uvec = "vec"
              writeUnaryOp_ Udot = "dot"
              writeUnaryOp_ Uddot = "ddot"

-- Writes the delimitors
writeLBracket :: LBracket -> String
writeRBracket :: RBracket -> String
writeLBracket (LBracket l _) = cmd_ "left" ++ aux l
    where aux LPar = "("
          aux LCro = "["
          aux LBra = "\\{"
          aux LChe = cmd "langle"
          aux LBraCons = "."
writeRBracket (RBracket r _) = cmd_ "right" ++ aux r 
   where  aux RPar = ")"
          aux RCro = "]"
          aux RBra = "\\}"
          aux RChe = cmd "rangle"
          aux RBraCons = "."

-- Usefull map
mmap :: (a -> b) -> [[a]] -> [[b]]
mmap f m = map (map f) m

-- Writes a simple expression
writeSimpleExpr :: SimpleExpr -> String
writeSimpleExpr (SimpleExpr expr _) = writeSimpleExpr_ expr
        where writeSimpleExpr_ (SEConst c) = writeConst c
              writeSimpleExpr_ (Matrix t css) =
                let mt = (if t == RawMatrix then "bmatrix" else "pmatrix") in
                let textMatrix = mmap writeCode css in
                let ls = map (intercalate " & ") textMatrix in
                let text = intercalate " \\\\ " ls in
                cmdargs "begin" [mt] ++ text ++ cmdargs "end" [mt]
              writeSimpleExpr_ (Delimited l e r) = 
                writeLBracket l ++ writeCode e ++ writeRBracket r
              writeSimpleExpr_ (UnaryApp o e) =
                cmdargs (writeUnaryOp o) [writeSimpleExprND e]
              writeSimpleExpr_ (BinaryApp (BinaryOp BFrac _) e1 e2) =
                cmdargs "frac" [writeSimpleExprND e1, writeSimpleExprND e2]
              writeSimpleExpr_ (BinaryApp (BinaryOp BRoot _) e1 e2) =
                cmdargs ("sqrt[" ++ writeSimpleExpr e1 ++ "]") [writeSimpleExpr e2]
              writeSimpleExpr_ (BinaryApp (BinaryOp BStackRel _) e1 e2) =
                cmdargs "stackrel" [writeSimpleExpr e1, writeSimpleExpr e2]
              writeSimpleExpr_ (Raw s) = cmdargs "textrm" [s]

-- Writes a simple expression after removing the embracing delimiters if present
writeSimpleExprND :: SimpleExpr -> String
writeSimpleExprND (SimpleExpr (Delimited _ e _) _) = writeCode e
writeSimpleExprND e = writeSimpleExpr e

-- Writes an expression
writeExpr :: Expr -> String
writeExpr (Expr e _) = writeExpr_ e
        where writeExpr_ (Simple se) = writeSimpleExpr se
              writeExpr_ (Frac e1 e2) =
                cmdargs "frac" [writeSimpleExprND e1, writeSimpleExprND e2]
              writeExpr_ (Under e1 e2) =
                writeSimpleExpr e1 ++ "_{" ++ writeSimpleExprND e2 ++ "}"
              writeExpr_ (Super e1 e2) = 
                writeSimpleExpr e1 ++ "^{" ++ writeSimpleExprND e2 ++ "}"
              writeExpr_ (SubSuper e1 e2 e3) =
                writeSimpleExpr e1 ++
                "_{" ++ writeSimpleExprND e2 ++ "}" ++
                "^{" ++ writeSimpleExprND e3 ++ "}"

-- Writes a code block
writeCode :: Code -> String
writeCode = foldr (\e s -> writeExpr e ++ s) ""

-- The main writer
writeTeX :: Code -> String
writeTeX = writeCode

