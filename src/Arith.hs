module Arith where 

import Text.Printf (printf)
import Text.Read
import Data.Char (isDigit, digitToInt)
import qualified Sum (parseNum, splitOn) 

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

-- "1+2*3+4*2" -> 15
data Operator = Plus 
              | Mult 
              | Minus 
              | Div
              deriving (Eq)

data AST = BinOp Operator AST AST 
         | Num  Int 
         deriving (Eq)

-- Преобразует дерево в строку
-- Между числами и знаками операций по одному пробелу
-- BinOp Plus (Num 13) (Num 42) -> "13 42 +"
toPostfix :: AST -> String
toPostfix (Num x) = show x
toPostfix (BinOp op x y) = (toPostfix x) ++ " " ++ (toPostfix y) ++ " " ++ show op

-- Парсит выражение в постфиксной записи 
-- Выражение принимается только целиком (не максимально длинный префикс)
-- Между числами и знаками операций по одному пробелу
-- "13 42 +" -> Just (BinOp Plus (Num 13) (Num 42))
-- "1 2 3 +" -> Nothing
-- "1 2 + *" -> Nothing 
fromPostfix :: String -> Maybe AST 
fromPostfix input = helper (words input) [] where
    op c = let Just (res, "") = parseOp c in res
    helper [] [res]       = Just res
    helper [] stack       = Nothing
    helper (w:xs) stack = case ((readMaybe w)::Maybe Int) of
        Just num -> helper xs ((Num num):stack)
        Nothing -> case stack of
            a:b:as -> helper xs ((BinOp (op w) b a):as)
            _      -> Nothing

-- Парсит левую скобку
parseLbr :: String -> Maybe ((), String)
parseLbr ('(':xs) = Just ((), xs)
parseLbr _        = Nothing

-- Парсит правую скобку
parseRbr :: String -> Maybe ((), String)
parseRbr (')':xs) = Just ((), xs)
parseRbr _        = Nothing

parseExpr :: String -> Maybe (AST, String)
parseExpr input = parseSum input

parseNum :: String -> Maybe (AST, String)
parseNum input = 
    let (num, rest) = span isDigit input in
        case num of
        [] -> case parseLbr input of
            Just (_, xs) -> do
                (ast, ys) <- parseExpr xs
                (_, zs) <- parseRbr ys
                return (ast, zs)
            Nothing -> Nothing
        xs -> Just (Num $ parseNum_ xs, rest)

parseOp :: String -> Maybe (Operator, String)
parseOp ('+':xs) = Just (Plus, xs)
parseOp ('*':xs) = Just (Mult, xs)
parseOp ('-':xs) = Just (Minus, xs)
parseOp ('/':xs) = Just (Div, xs)
parseOp _ = Nothing 

parseMult :: String -> Maybe (AST, String)
parseMult input = do
    (num, rest) <- parseNum input 
    case parseOp rest of 
      Just (op, rest') | op == Mult || op == Div -> do
        (r, rest'') <- parseMult rest'  
        return (BinOp op num r, rest'') 
      _ -> return (num, rest)
  

parseSum :: String -> Maybe (AST, String)
parseSum input = do 
  (l, rest) <- parseMult input 
  case parseOp rest of 
    Just (op, rest') | op == Plus || op == Minus -> do
      (r, rest'') <- parseSum rest'  
      return (BinOp op l r, rest'') 
    _ -> return (l, rest)

evaluate :: String -> Maybe Int
evaluate input = do 
    (ast, rest) <- parseExpr input 
    return $ compute ast 

compute :: AST -> Int 
compute (Num x) = x 
compute (BinOp Plus x y) = compute x + compute y 
compute (BinOp Mult x y) = compute x * compute y 
compute (BinOp Minus x y) = compute x - compute y 
compute (BinOp Div x y) = compute x `div` compute y 

instance Show Operator where 
  show Plus = "+"
  show Mult = "*"
  show Minus = "-" 
  show Div = "/"

instance Show AST where
  show  = printf "\n%s" . go 0 
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $ 
        case t of 
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+1)
