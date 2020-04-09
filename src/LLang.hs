module LLang where


import AST (AST (..), Operator (..))
import Combinators (Parser (..), Result (..))
import Data.Map (Map (..), insert, (!), member, empty)
import Expr (parseExpr, parseStr, parseIdent)
import Control.Applicative ((<|>), many)
import Text.Printf (printf)
import Data.List (intercalate)

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
  deriving (Eq)

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


initialConf :: [Int] -> Configuration
initialConf input = Conf empty input []


eval :: LAst -> Configuration -> Maybe Configuration
eval (Seq {statements = stmts}) conf' = foldl (\c p -> c >>= (eval p)) (Just conf') stmts
eval (Assign {var = var, expr = expr}) (Conf {subst = subst, input = inp, output = outp}) =
  case evalExpr expr subst of
    Nothing -> Nothing
    Just x  -> Just $ Conf {subst = insert var x subst, input = inp, output = outp}
eval (Read {var = var}) (Conf {subst = subst, input = (i:inp), output = outp}) =
    Just $ Conf {subst = insert var i subst, input = inp, output = outp}
eval (Write {expr = expr}) (Conf {subst = subst, input = inp, output = outp}) =
  case evalExpr expr subst of
    Nothing -> Nothing
    Just x  -> Just $ Conf {subst = subst, input = inp, output = x:outp}
eval while@(While {cond = cond, body = body}) conf'@(Conf {subst = subst, input = inp, output = outp}) =
    case evalExpr cond subst of
      Nothing -> Nothing
      Just c  -> case eval body conf' of
        Nothing -> Nothing
        Just b  -> if c /= 0 then eval while b else Just conf'
eval if'@(If {cond = cond, thn = thn, els = els}) conf'@(Conf {subst = subst, input = inp, output = outp}) =
    case evalExpr cond subst of
      Nothing -> Nothing
      Just c -> let body = if c /= 0 then thn else els in eval body conf'
eval _ _ = Nothing

evalExpr :: Expr -> Map Var Int -> Maybe Int
evalExpr (Num x)            c = Just x
evalExpr (Ident x)          c = if member x c then Just(c ! x) else Nothing
evalExpr (UnaryOp Minus x)  c =  (*(-1)) <$> evalExpr x c
evalExpr (UnaryOp Not x)    c = fromEnum <$> (==0) <$> evalExpr x c
evalExpr (BinOp Plus x y)   c = (+) <$> evalExpr x c <*> evalExpr y c
evalExpr (BinOp Mult x y)   c = (*) <$> evalExpr x c <*> evalExpr y c
evalExpr (BinOp Minus x y)  c = (-) <$> evalExpr x c <*> evalExpr y c
evalExpr (BinOp Div x y)    c = div <$> evalExpr x c <*> evalExpr y c
evalExpr (BinOp Pow x y)    c = (^) <$> evalExpr x c <*> evalExpr y c
evalExpr (BinOp Equal x y)  c = fromEnum <$> ((==) <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Nequal x y) c = fromEnum <$> ((/=) <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Ge x y)     c = fromEnum <$> ((>=) <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Gt x y)     c = fromEnum <$> ((>)  <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Le x y)     c = fromEnum <$> ((<=) <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Lt x y)     c = fromEnum <$> ((<)  <$> evalExpr x c <*> evalExpr y c)
evalExpr (BinOp Or x y)     c = fromEnum <$> ((||) <$> ((/=0) <$> evalExpr x c) <*> ((/=0) <$> evalExpr y c))
evalExpr (BinOp And x y)    c = fromEnum <$> ((&&) <$> ((/=0) <$> evalExpr x c) <*> ((/=0) <$> evalExpr y c))

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id in

        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
      ident = (+1)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
