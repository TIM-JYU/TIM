module Passes (matrix) where
import Ast

---------------------------------
-- Pass 1 : Recognise matrices --
---------------------------------

-- main function
matrix :: Code -> Code
matrix = map matrixExpr

matrixExpr :: Expr -> Expr
matrixExpr (Expr e pos) = Expr (matrixExpr_ e) pos
matrixExpr_ :: Expr_ -> Expr_
matrixExpr_ (Simple e) = Simple $ matrixSE e
matrixExpr_ (Frac e1 e2) = Frac (matrixSE e1) (matrixSE e2)
matrixExpr_ (Under e1 e2) = Under (matrixSE e1) (matrixSE e2)
matrixExpr_ (Super e1 e2) = Super (matrixSE e1) (matrixSE e2)
matrixExpr_ (SubSuper e1 e2 e3) = SubSuper (matrixSE e1) (matrixSE e2) (matrixSE e3)

matrixSE :: SimpleExpr -> SimpleExpr
matrixSE (SimpleExpr e pos) = SimpleExpr (matrixSE_ e) pos
matrixSE_ :: SimpleExpr_ -> SimpleExpr_
matrixSE_ (Delimited (LBracket LCro lpos) c (RBracket RCro rpos)) =
    case parseSeq c of
        Nothing -> Delimited (LBracket LCro lpos)
                             (matrix c)
                             (RBracket RCro rpos)
        Just m -> Matrix RawMatrix m
matrixSE_ (Delimited (LBracket LPar lpos) c (RBracket RPar rpos)) =
    case parseSeq c of
        Nothing -> Delimited (LBracket LPar lpos)
                             (matrix c)
                             (RBracket RPar rpos)
        Just m -> Matrix ColMatrix m
matrixSE_ (UnaryApp o e) = UnaryApp o (matrixSE e)
matrixSE_ (BinaryApp o e1 e2) = BinaryApp o (matrixSE e1) (matrixSE e2)
matrixSE_ x = x

-- Usefull predicates
com :: SimpleExpr -> Bool
com (SimpleExpr (SEConst (Constant Comma _)) _) = True
com _ = False
comma :: Expr -> Bool
comma (Expr (Simple e) _) | com e = True
comma _ = False

-- like unwords but cuts at the symbols matching p
split :: (a -> Bool) -> [a] -> [[a]]
split p l = case break p l of
    (_, []) -> [l]
    (l1, l2) -> l1 : (split p $ drop 1 l2)

-- First step of parseSeq : match a sequence of comma-separated bracketed
-- expression
unbracket :: [Code] -> Maybe [(LBracket_, Code)]
unbracket [] = Just []
unbracket ([(Expr (Simple (SimpleExpr (Delimited lb c rb) _)) _)]:cs) =
    case (lb, rb, unbracket cs) of
        (LBracket LCro _, RBracket RCro _, Just l) -> Just ((LCro, c):l)
        (LBracket LPar _, RBracket RPar _, Just l) -> Just ((LPar, c):l)
        _ -> Nothing
unbracket _ = Nothing

parseSeq1 :: Code -> Maybe [(LBracket_, Code)]
parseSeq1 = unbracket . split comma

-- Second step of parseSeq : check if all the delimiters used are similars
parseSeq2 :: Maybe [(LBracket_, Code)] -> Maybe [[Code]]
parseSeq2 Nothing = Nothing
parseSeq2 (Just cs) =
  let (lb, _) = head cs in
  if all (\(lb', _) -> lb' == lb) cs then
    let res = map ((split comma) . snd) cs in
    let n = length . head $ res in
    if all (\l -> n == length l) res then
      Just res
      else
        Nothing
  else
    Nothing

-- ParseSeq
parseSeq :: Code -> Maybe [[Code]]
parseSeq = parseSeq2 . parseSeq1
