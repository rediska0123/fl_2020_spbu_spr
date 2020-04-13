module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), Result (..), fail',
                                      runParser, satisfy, stream, success,
                                      symbol, parseStr, manyWithSep)
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit, isLetter)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary


plus :: Parser String String Operator
plus   = Plus   <$ parseStr "+"
minus  = Minus  <$ parseStr "-"
mult   = Mult   <$ parseStr "*"
div'   = Div    <$ parseStr "/"
pow    = Pow    <$ parseStr "^"
equal  = Equal  <$ parseStr "=="
nequal = Nequal <$ parseStr "/="
ge     = Ge     <$ parseStr ">="
gt     = Gt     <$ parseStr ">"
le     = Le     <$ parseStr "<="
lt     = Lt     <$ parseStr "<"
not'   = Not    <$ parseStr "!"
and'   = And    <$ parseStr "&&"
or'    = Or     <$ parseStr "||"


evalExpr :: Subst -> AST -> Maybe Int
evalExpr c (Num x)            = Just x
evalExpr c (Ident x)          = Map.lookup x c
evalExpr c (UnaryOp Minus x)  =  (*(-1)) <$> evalExpr c x
evalExpr c (UnaryOp Not x)    = fromEnum <$> (==0) <$> evalExpr c x
evalExpr c (BinOp Plus x y)   = (+) <$> evalExpr c x <*> evalExpr c y
evalExpr c (BinOp Mult x y)   = (*) <$> evalExpr c x <*> evalExpr c y
evalExpr c (BinOp Minus x y)  = (-) <$> evalExpr c x <*> evalExpr c y
evalExpr c (BinOp Div x y)    = div <$> evalExpr c x <*> evalExpr c y
evalExpr c (BinOp Pow x y)    = (^) <$> evalExpr c x <*> evalExpr c y
evalExpr c (BinOp Equal x y)  = fromEnum <$> ((==) <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Nequal x y) = fromEnum <$> ((/=) <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Ge x y)     = fromEnum <$> ((>=) <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Gt x y)     = fromEnum <$> ((>)  <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Le x y)     = fromEnum <$> ((<=) <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Lt x y)     = fromEnum <$> ((<)  <$> evalExpr c x <*> evalExpr c y)
evalExpr c (BinOp Or x y)     = fromEnum <$> ((||) <$> ((/=0) <$> evalExpr c x) <*> ((/=0) <$> evalExpr c y))
evalExpr c (BinOp And x y)    = fromEnum <$> ((&&) <$> ((/=0) <$> evalExpr c x) <*> ((/=0) <$> evalExpr c y))


foldExpr :: AST -> (String -> a -> a) -> (AST -> a -> a) -> a -> a
foldExpr cur@(BinOp op e1 e2) vf af i = s where
    i' = af cur i
    f  = foldExpr e1 vf af i'
    s  = foldExpr e2 vf af f
foldExpr cur@(UnaryOp op e) vf af i = f where
    i' = af cur i
    f  = foldExpr e vf af i'
foldExpr cur@(Num x) vf af i = af cur i
foldExpr cur@(Ident x) vf af i = vf x $ af cur i
foldExpr cur@(FunctionCall f args) vf af i = p where
    i' = af cur i
    p  = foldl (\x arg -> foldExpr arg vf af x) i' args


uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr ops elem binc unc =
  foldr f elem ops where
    f (op, Unary) elem1 =
      (unc <$> op <*> elem1) <|> elem1

    f (op, Binary NoAssoc) elem1 = do
      x <- elem1
      ((\o -> binc o x) <$> op <*> elem1) <|> return x

    f (op, Binary assoc) elem1 = do
        x <- elem1
        let all = many ((,) <$> op <*> elem1) in do
          (fold' assoc x) <$> all

    fold' LeftAssoc x =
      foldl (\v1 (op, v2) -> binc op v1 v2) x
    fold' RightAssoc x = \list -> snd $
      foldr1 (\(op1, v1) (op2, v2) -> (op1, binc op2 v1 v2)) ((undefined, x):list)


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr [(or', Binary RightAssoc),
                      (and', Binary RightAssoc),
                      (not', Unary),
                      (equal <|> nequal <|> ge <|> gt <|> le <|> lt, Binary NoAssoc),
                      (plus <|> minus, Binary LeftAssoc),
                      (mult <|> div', Binary LeftAssoc),
                      (minus, Unary),
                      (pow, Binary RightAssoc)]
                     (Num <$> parseNum <|>
                      FunctionCall <$> parseIdent <* symbol '(' <*> manyWithSep (parseStr ", ") parseExpr <* symbol ')' <|>
                      Ident <$> parseIdent <|>
                      symbol '(' *> parseExpr <* symbol ')')
                     BinOp UnaryOp

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = (:) <$> (satisfy isLetter <|> symbol '_') <*>
    many (satisfy isLetter <|> symbol '_' <|> satisfy isDigit)

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

compute :: AST -> Int
compute (Num x)            = x
compute (UnaryOp Minus x)  = - compute x
compute (UnaryOp Not x)    = fromEnum $ compute x == 0
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = compute x ^ compute y
compute (BinOp Equal x y)  = fromEnum $ compute x == compute y
compute (BinOp Nequal x y) = fromEnum $ compute x /= compute y
compute (BinOp Ge x y)     = fromEnum $ compute x >= compute y
compute (BinOp Gt x y)     = fromEnum $ compute x > compute y
compute (BinOp Le x y)     = fromEnum $ compute x < compute y
compute (BinOp Lt x y)     = fromEnum $ compute x <= compute y
compute (BinOp Or x y)     = fromEnum $ (compute x /= 0) || (compute y /= 0)
compute (BinOp And x y)    = fromEnum $ (compute x /= 0) && (compute y /= 0)

