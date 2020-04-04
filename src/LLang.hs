module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..), Result (..))
import Data.Map (Map (..), insert, (!))
import Expr (parseExpr, parseStr, parseIdent)
import Control.Applicative ((<|>), many)

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Map Var Int, input :: [Int], output :: [Int] }
  deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: Expr, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseAssign :: Parser String String LAst
parseAssign = do
    parseStr "Assign "
    x <- parseIdent
    parseStr " ("
    expr <- parseExpr
    parseStr ")"
    return Assign {var = x, expr = expr}


parseRead :: Parser String String LAst
parseRead = do
    parseStr "Read "
    x <- parseIdent
    return Read {var = x}


parseWrite :: Parser String String LAst
parseWrite = do
    parseStr "Write ("
    expr <- parseExpr
    parseStr ")"
    return Write {expr = expr}


parseSeq :: Parser String String LAst
parseSeq = do
    parseStr "{ "
    statements <- many $ parseStatement <* parseStr "; "
    parseStr "}"
    return Seq {statements = statements}


parseIf :: Parser String String LAst
parseIf = do
    parseStr "If ("
    expr <- parseExpr
    parseStr ") "
    l1 <- parseSeq
    parseStr " "
    l2 <- parseSeq
    return If {cond = expr, thn = l1, els = l2}


parseWhile :: Parser String String LAst
parseWhile = do
    parseStr "While ("
    expr <- parseExpr
    parseStr ") "
    l <- parseSeq
    return While {cond = expr, body = l}


parseStatement :: Parser String String LAst
parseStatement = parseAssign <|> parseRead <|> parseWrite <|> parseSeq <|> parseIf <|> parseWhile


getExprVars :: Expr -> [Var]
getExprVars (BinOp op e1 e2) = getExprVars e1 ++ getExprVars e2
getExprVars (UnaryOp op e) = getExprVars e
getExprVars (Num _) = []
getExprVars (Ident x) = [x]


getVars :: LAst -> [Var]
getVars (If {cond = cond, thn = thn, els = els}) = condVars ++ bodyVars where
    bodyVars = getVars thn ++ getVars els
    condVars = getExprVars cond
getVars (While {cond = cond, body = body}) = condVars ++ bodyVars where
    bodyVars = getVars body
    condVars = getExprVars cond
getVars (Assign {var = x, expr = expr}) = x : (getExprVars expr)
getVars (Read {var = x}) = [x]
getVars (Write {expr = expr}) = getExprVars expr
getVars (Seq {statements = stmts}) = concat $ getVars <$> stmts


unique :: (Eq a) => [a] -> [a]
unique = foldr (\x list -> if elem x list then list else x:list) []


addVarDefinitions :: LAst -> LAst
addVarDefinitions a@(Seq {statements = stmts}) =
    Seq {statements = asgmts ++ stmts} where
        asgmts = (\x -> Assign{var = x, expr = Num 0}) <$> unique (getVars a)


parseL :: Parser String String LAst
parseL = addVarDefinitions <$> parseSeq

eval :: String -> Configuration -> Configuration
eval program conf = let Success "" last = runParser parseL program in
  evalLAst last conf where
    evalLAst :: LAst -> Configuration -> Configuration
    evalLAst (Seq {statements = stmts}) conf' = foldl (flip evalLAst) conf' stmts
    evalLAst (Assign {var = var, expr = expr})
      (Conf {subst = subst, input = inp, output = outp}) =
        Conf {subst = insert var (evalExpr expr subst) subst, input = inp, output = outp}
    evalLAst (Read {var = var})
      (Conf {subst = subst, input = (i:inp), output = outp}) =
        Conf {subst = insert var i subst, input = inp, output = outp}
    evalLAst (Write {expr = expr})
      (Conf {subst = subst, input = inp, output = outp}) =
        Conf {subst = subst, input = inp, output = outp ++ [(evalExpr expr subst)]}
    evalLAst while@(While {cond = cond, body = body})
      conf'@(Conf {subst = subst, input = inp, output = outp}) =
        let c = evalExpr cond subst in
          if c /= 0 then evalLAst while (evalLAst body conf') else conf'
    evalLAst if'@(If {cond = cond, thn = thn, els = els})
      conf'@(Conf {subst = subst, input = inp, output = outp}) =
        let c    = evalExpr cond subst
            body = if c /= 0 then thn else els in
          evalLAst body conf'
    evalLAst _ _ = error "failed to evaluate"

    evalExpr :: Expr -> Map Var Int -> Int
    evalExpr (Num x)            c = x
    evalExpr (Ident x)          c = c ! x
    evalExpr (UnaryOp Minus x)  c = - evalExpr x c
    evalExpr (UnaryOp Not x)    c = fromEnum $ evalExpr x c == 0
    evalExpr (BinOp Plus x y)   c = evalExpr x c + evalExpr y c
    evalExpr (BinOp Mult x y)   c = evalExpr x c * evalExpr y c
    evalExpr (BinOp Minus x y)  c = evalExpr x c - evalExpr y c
    evalExpr (BinOp Div x y)    c = evalExpr x c `div` evalExpr y c
    evalExpr (BinOp Pow x y)    c = evalExpr x c ^ evalExpr y c
    evalExpr (BinOp Equal x y)  c = fromEnum $ evalExpr x c == evalExpr y c
    evalExpr (BinOp Nequal x y) c = fromEnum $ evalExpr x c /= evalExpr y c
    evalExpr (BinOp Ge x y)     c = fromEnum $ evalExpr x c >= evalExpr y c
    evalExpr (BinOp Gt x y)     c = fromEnum $ evalExpr x c >  evalExpr y c
    evalExpr (BinOp Le x y)     c = fromEnum $ evalExpr x c <  evalExpr y c
    evalExpr (BinOp Lt x y)     c = fromEnum $ evalExpr x c <= evalExpr y c
    evalExpr (BinOp Or x y)     c = fromEnum $ (evalExpr x c /= 0) || (evalExpr y c /= 0)
    evalExpr (BinOp And x y)    c = fromEnum $ (evalExpr x c /= 0) && (evalExpr y c /= 0)
