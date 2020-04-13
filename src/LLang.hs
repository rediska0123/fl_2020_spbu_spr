module LLang where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..), parseStr, manyWithSep,
                              ErrorMsg (..), Result (..), fail')
import           Expr        (evalExpr, foldExpr, parseIdent,
                              parseExpr)
import           Data.List   (intercalate, (\\))
import qualified Data.Map    as Map
import qualified Data.Set as Set
import           Text.Printf (printf)
import           Control.Applicative

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
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


parseReturn :: Parser String String LAst
parseReturn = do
    parseStr "Return ("
    expr <- parseExpr
    parseStr ")"
    return Return {expr = expr}


parseSeq' :: Parser String String LAst -> Parser String String LAst
parseSeq' prsStmt = do
    parseStr "{ "
    statements <- many $ prsStmt <* parseStr "; "
    parseStr "}"
    return Seq {statements = statements}


parseIf' :: Parser String String LAst -> Parser String String LAst
parseIf' prsSq = do
    parseStr "If ("
    expr <- parseExpr
    parseStr ") "
    l1 <- prsSq
    parseStr " "
    l2 <- prsSq
    return If {cond = expr, thn = l1, els = l2}


parseWhile' :: Parser String String LAst -> Parser String String LAst
parseWhile' prsSq = do
    parseStr "While ("
    expr <- parseExpr
    parseStr ") "
    l <- prsSq
    return While {cond = expr, body = l}


parseSeq :: Parser String String LAst
parseSeq = parseSeq' parseStatement

parseIf :: Parser String String LAst
parseIf = parseIf' parseSeq

parseWhile :: Parser String String LAst
parseWhile = parseWhile' parseSeq

parseStatement :: Parser String String LAst
parseStatement =
    parseAssign <|>
    parseRead <|>
    parseWrite <|>
    parseSeq <|>
    parseIf <|>
    parseWhile


parseFunctionSeq :: Parser String String LAst
parseFunctionSeq = parseSeq' parseFunctionStatement

parseFunctionIf :: Parser String String LAst
parseFunctionIf = parseIf' parseFunctionSeq

parseFunctionWhile :: Parser String String LAst
parseFunctionWhile = parseWhile' parseFunctionSeq

parseFunctionStatement :: Parser String String LAst
parseFunctionStatement =
    parseAssign <|>
    parseRead <|>
    parseWrite <|>
    parseFunctionSeq <|>
    parseFunctionIf <|>
    parseFunctionWhile <|>
    parseReturn


fold :: LAst -> (Var -> a -> a) -> (AST -> a -> a) -> a -> a
fold (If {cond = cond, thn = thn, els = els}) vf af i =
    elsf
  where
    condf = foldExpr cond vf af i
    thnf  = fold thn vf af condf
    elsf  = fold els vf af thnf
fold (While {cond = cond, body = body}) vf af i =
    bodyf
  where
    condf = foldExpr cond vf af i
    bodyf = fold body vf af condf
fold (Assign {var = x, expr = expr}) vf af i =
    foldExpr expr vf af (vf x i)
fold (Read {var = x}) vf af i =
    vf x i
fold (Write {expr = expr}) vf af i =
    foldExpr expr vf af i
fold (Seq {statements = stmts}) vf af i =
    foldl (\x stmt -> fold stmt vf af x) i stmts
fold (Return {expr = expr}) vf af i =
    foldExpr expr vf af i


getVars :: LAst -> [Var]
getVars last = fold last (:) (flip const) []

-- Возвращает все вызову функций --- (имя функции,
-- количесво аргументов, с которыми он вызвана)
getFCalls :: LAst -> [(String, Int)]
getFCalls last = fold last (flip const) (\ast list ->
  case ast of
    FunctionCall f args -> (f, length args): list
    otherwise           -> list
  ) []


unique :: (Eq a) => [a] -> [a]
unique = foldr (\x list -> if elem x list then list else x:list) []


addVarDefinitionsExcept :: [Var] -> LAst -> LAst
addVarDefinitionsExcept except_vars a@(Seq {statements = stmts}) =
    Seq {statements = asgmts ++ stmts} where
        asgmts = (\x -> Assign {var = x, expr = Num 0}) <$> (unique (getVars a) \\ except_vars)


parseL :: Parser String String LAst
parseL = addVarDefinitionsExcept [] <$> parseSeq


addReturn :: LAst -> LAst
addReturn (Seq {statements = stmts}) =
    Seq {statements = stmts ++ [Return (Num 0)]}

parseDef :: Parser String String Function
parseDef = do
    parseStr "Def "
    name <- parseIdent
    parseStr "("
    args <- manyWithSep (parseStr ", ") parseIdent
    parseStr ") "
    body <- parseFunctionSeq
    return Function {name = name, args = args, funBody = (addVarDefinitionsExcept args) $ addReturn body}


-- Возвращает ошибку, если есть одиаковые имена функций,
-- вызовы несуществующих функций или в функцию передано неправильное
-- число аргументов
checkDuplicatesAndCalls :: Program -> Maybe String
checkDuplicatesAndCalls Program {functions = fs, main = m} = let
    fnames = (\Function {name = n, args = a, funBody = b} -> n) <$> fs
    fdefs  = (\Function {name = n, args = a, funBody = b} -> (n, length a)) <$> fs
    fcalls = (concat (getFCalls <$> (\Function {name = n, args = a, funBody = b} -> b) <$> fs)) ++ getFCalls m
    check :: (String, Int) -> Maybe String
    check (f, a) = if elem (f, a) fdefs then Nothing else
      Just $ "Wrong function name or number of arguments in " ++ f ++ "(" ++ show a ++ " arguments); "
  in
    if length fnames /= length (Set.fromList fnames) then Just "Duplicate function definitions" else
    foldl (\f d -> case check d of
      Nothing  -> f
      Just err -> case f of
        Nothing   -> Just err
        Just err1 -> Just (err ++ err1)) Nothing fcalls

parseProg :: Parser String String Program
parseProg = do
    pr <- parseProgHelper
    case checkDuplicatesAndCalls pr of
      Nothing  -> return pr
      Just err -> fail' err
  where
    parseProgHelper :: Parser String String Program
    parseProgHelper = do
        funcs <- many (parseDef <* parseStr "; ")
        main <- parseL
        return Program {functions = funcs, main = main}

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval (Seq {statements = stmts}) conf' = foldl (\c p -> c >>= (eval p)) (Just conf') stmts
eval (Assign {var = var, expr = expr}) (Conf {subst = subst, input = inp, output = outp}) =
  case evalExpr subst expr of
    Nothing -> Nothing
    Just x  -> Just $ Conf {subst = Map.insert var x subst, input = inp, output = outp}
eval (Read {var = var}) (Conf {subst = subst, input = (i:inp), output = outp}) =
    Just $ Conf {subst = Map.insert var i subst, input = inp, output = outp}
eval (Write {expr = expr}) (Conf {subst = subst, input = inp, output = outp}) =
  case evalExpr subst expr of
    Nothing -> Nothing
    Just x  -> Just $ Conf {subst = subst, input = inp, output = x:outp}
eval while@(While {cond = cond, body = body}) conf'@(Conf {subst = subst, input = inp, output = outp}) =
    case evalExpr subst cond of
      Nothing -> Nothing
      Just c  -> case eval body conf' of
        Nothing -> Nothing
        Just b  -> if c /= 0 then eval while b else Just conf'
eval if'@(If {cond = cond, thn = thn, els = els}) conf'@(Conf {subst = subst, input = inp, output = outp}) =
    case evalExpr subst cond of
      Nothing -> Nothing
      Just c -> let body = if c /= 0 then thn else els in eval body conf'
eval _ _ = Nothing


instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Eq Function where
  (==) a b = (show a) == (show b)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Eq Program where
  (==) a b = (show a) == (show b)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
