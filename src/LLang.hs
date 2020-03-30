module LLang where

import AST (AST (..), Operator (..))
import Combinators (Parser (..))
import Expr (parseExpr, parseStr, parseIdent)
import Control.Applicative ((<|>), many)

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
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


parseL :: Parser String String LAst
parseL = parseSeq
