module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f (Parser runp) = Parser $ \inp ->
    case runp inp of
      Failure err      -> Failure err
      Success inp1 res -> Success inp1 $ f res

instance Applicative (Parser error input) where
  pure x = Parser $ \inp -> Success inp x
  (Parser r1) <*> (Parser r2) = Parser $ \inp ->
    case r1 inp of
      Failure err    -> Failure err
      Success inp1 f ->
        case r2 inp1 of
          Failure err    -> Failure err
          Success inp2 x -> Success inp2 $ f x

instance Monad (Parser error input) where
  (Parser r1) >>= f = Parser $ \inp ->
    case r1 inp of
      Failure err    -> Failure err
      Success inp1 x -> runParser (f x) inp1

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \inp -> Failure mempty
  (Parser r1) <|> (Parser r2) = Parser $ \inp -> case r1 inp of
    Failure _  -> r2 inp
    x          -> x

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (many (sep *> elem))

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Show a => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success = pure

--- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure

