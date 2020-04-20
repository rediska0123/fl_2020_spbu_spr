module LEval where

import LLang (Program (..), Configuration (..), eval, Function (..), parseProg)
import Combinators (InputStream (..), Result (..), runParser)
import qualified Data.Map    as Map

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funcs main) inp =
  eval main (Conf
    Map.empty
    inp
    []
    (Map.fromList (fmap (\f@(Function name _ _ _) -> (name, f)) funcs))
  )


parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg str inp =
  let Success (InputStream st pos) prog = (runParser parseProg str)
      len = length st in
    case pos of
      len       -> evalProg prog inp
