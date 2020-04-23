module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser,
                                   toStream, symbol, InputStream (..),
                                   Position (..), ErrorMsg (..))
import qualified Data.Map         as Map
import           Data.Map            (empty, fromList)
import           Debug.Trace      (trace)
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf, parseL, Function (..), Program (..),
                                   parseAssign, parseRead, parseWrite, parseSeq,
                                   parseIf, parseWhile, parseStatement,
                                   getVars, eval, parseDef, parseProg,
                                   getFCalls)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)


isFailure (Failure _) = True
isFailure  _          = False

checkParses :: (Eq r, Show r) => Parser String String r -> String -> r -> Assertion
checkParses parser prog last = do
    case runParser parser prog of
       Success (InputStream "" _) _ -> 1    @?= 1
       otherwise                    -> prog @?= "IS NOT PARSING!"


unit_parseAssign :: Assertion
unit_parseAssign = do
    checkParses parseAssign "Assign x (13)" (Assign {var = "x", expr = (Num 13)})
    checkParses parseAssign "Assignx(13)" (Assign {var = "x", expr = (Num 13)})
    checkParses parseAssign "\nAssignx  (  13\n)" (Assign {var = "x", expr = (Num 13)})
    checkParses parseAssign "Assign _ (13 * 42)" (Assign {var = "_", expr = (BinOp Mult (Num 13) (Num 42))})
    assertBool "" $ isFailure $ runParser parseAssign "Assign x 13"
    assertBool "" $ isFailure $ runParser parseAssign "kekos abrikos"


unit_parseRead :: Assertion
unit_parseRead = do
    checkParses parseRead "Read _x" (Read {var = "_x"})
    checkParses parseRead "Readkek" (Read {var = "kek"})
    checkParses parseRead "   ReadRead" (Read {var = "Read"})
    assertBool "" $ isFailure $ runParser parseRead "Assign x (13)"
    assertBool "" $ isFailure $ runParser parseRead "Read (x)"


unit_parseWrite :: Assertion
unit_parseWrite = do
    checkParses parseWrite "Write (_x)" (Write {expr = (Ident "_x")})
    checkParses parseWrite " Write(  k )" (Write {expr = (Ident "k")})
    checkParses parseWrite "Write (3-z)" (Write {expr = (BinOp Minus (Num 3) (Ident "z"))})
    assertBool "" $ isFailure $ runParser parseWrite "Write 3"


unit_parseSeq :: Assertion
unit_parseSeq = do
    checkParses parseSeq "{}" (Seq {statements = []})
    checkParses parseSeq "{ }" (Seq {statements = []})
    checkParses parseSeq "{ Read x; }" (Seq {statements = [Read {var = "x"}]})
    checkParses parseSeq "  {Readx ;} " (Seq {statements = [Read {var = "x"}]})
    checkParses parseSeq "{ Read x; Write (x); }" (Seq {statements = [Read {var = "x"}, Write {expr = (Ident "x")}]})
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x }"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x; Write (x) }"


unit_parseIf :: Assertion
unit_parseIf = do
    checkParses parseIf "If (2<3) { } { }" (If {
       cond = (BinOp Lt (Num 2) (Num 3)),
       thn = Seq{statements = []},
       els = Seq{statements = []}
    })
    checkParses parseIf " If(2 < 3){\n}{}" (If {
       cond = (BinOp Lt (Num 2) (Num 3)),
       thn = Seq{statements = []},
       els = Seq{statements = []}
    })
    checkParses parseIf "If (1) { Read x; } { Write (x); }" (If {
       cond = (Num 1),
       thn = Seq{statements = [Read {var = "x"}]},
       els = Seq{statements = [Write {expr = (Ident "x")}]}
    })
    assertBool "" $ isFailure $ runParser parseIf "If () { } { }"
    assertBool "" $ isFailure $ runParser parseIf "If 1 { } { }"
    assertBool "" $ isFailure $ runParser parseIf "if (1) { } { }"
    assertBool "" $ isFailure $ runParser parseIf "If (1) { Read x; }"
    assertBool "" $ isFailure $ runParser parseIf "If (1) { Read x; } { Write (x) }"


unit_parseWhile :: Assertion
unit_parseWhile = do
    checkParses parseWhile "While (2<3) { }" (While {
       cond = (BinOp Lt (Num 2) (Num 3)),
       body = Seq{statements = []}
    })
    checkParses parseWhile "  While(2\n<  3){} " (While {
       cond = (BinOp Lt (Num 2) (Num 3)),
       body = Seq{statements = []}
    })
    checkParses parseWhile "While (1) { Read x; }" (While {
       cond = (Num 1),
       body = Seq{statements = [Read {var = "x"}]}
    })
    assertBool "" $ isFailure $ runParser parseWhile "While () { }"
    assertBool "" $ isFailure $ runParser parseWhile "while (1) { }"
    assertBool "" $ isFailure $ runParser parseWhile "While (0) { Read x }"


unit_getVars :: Assertion
unit_getVars = do
    let Success _ last = runParser parseSeq "{ }" in
        getVars last @?= []
    let Success _ last = runParser parseSeq "{ Assign x (10+10); Read y; Write (d); }" in
        getVars last @?= ["d", "y", "x"]
    let Success _ last = runParser parseSeq "{ If (y+x>10) { Assign a (8); } { Write (b*6); }; }" in
        getVars last @?= ["b", "a", "x", "y"]
    let Success _ last = runParser parseSeq "{ While (y+x>10) { Assign a (8); }; }" in
        getVars last @?= ["a", "x", "y"]


unit_getFCalls :: Assertion
unit_getFCalls = do
    let Success _ last = runParser parseSeq "{ Assign x (f(2, g(4, 5), kek(mem()))); Write (f(x)); }" in
        getFCalls last @?= [("f", 1), ("mem", 0), ("kek", 1), ("g", 2), ("f", 3)]


unit_parseL :: Assertion
unit_parseL = do
    checkParses parseL "{ }" (Seq {statements = []})
    checkParses parseL "{ Assign x (10+10); While (x>10) { Assign x (x-1); Write (x); }; }" (Seq {statements = [
       Assign {var = "x", expr = Num 0},
       Assign {var = "x", expr = BinOp Plus (Num 10) (Num 10)},
       While {
         cond = BinOp Gt (Ident "x") (Num 10),
         body = Seq {statements = [
           Assign {var = "x", expr = BinOp Minus (Ident "x") (Num 1)},
           Write {expr = Ident "x"}
         ]}
       }
    ]})

    checkParses parseL "{ Assign x (10+10); While (x>10) { If (x>15) { Read x; } { }; }; }" (Seq {statements = [
       Assign {var = "x", expr = Num 0},
       Assign {var = "x", expr = BinOp Plus (Num 10) (Num 10)},
       While {
         cond = BinOp Gt (Ident "x") (Num 10),
         body = Seq {statements = [
           If {
             cond = BinOp Gt (Ident "x") (Num 15),
             thn = Seq {statements = [
               Read {var = "x"}
             ]},
             els = Seq {statements = []}
           }
         ]}
       }
    ]})

    let program = "{Assignx(10-x*7);If(a*3==5){Ready;}{Write(z);};While(b*10+c){Readc;Readd;};}"
        Success _ (Seq {statements = stmts}) = runParser parseSeq program
      in
        checkParses parseL program (Seq {statements = [
            Assign {var = "d", expr = Num 0},
            Assign {var = "c", expr = Num 0},
            Assign {var = "b", expr = Num 0},
            Assign {var = "z", expr = Num 0},
            Assign {var = "y", expr = Num 0},
            Assign {var = "a", expr = Num 0},
            Assign {var = "x", expr = Num 0}
        ] ++ stmts})

    let program = " {If(y +y >y){ Assign y(8); }{ Assign y (4); }; } "
        Success _ (Seq {statements = stmts}) = runParser parseSeq program
      in
        checkParses parseL program (Seq {statements = [
            Assign {var = "y", expr = Num 0}
        ] ++ stmts})

    checkParses parseL "{ Write (x); }" (Seq {statements = [
        Assign {var = "x", expr = Num 0},
        Write {expr = Ident "x"}
    ]})

    assertBool "" $ isFailure $ runParser parseL "{ Write (x) }"
    assertBool "" $ isFailure $ runParser parseL "If (1) { } { }"
    assertBool "" $ isFailure $ runParser parseL "While (1) { }"
    assertBool "" $ isFailure $ runParser parseL "Read x"


unit_eval :: Assertion
unit_eval = do
    let Success _ last = runParser parseL "{ Read x; While (x>0) { Write (2*x); Assign x (x-1); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf (fromList [("x", 0)]) [] [2, 4..20] Map.empty)

    let Success _ last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf (fromList [("x", 9), ("y", 0)]) [] [20] Map.empty)

    let Success _ last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [0]) @?=
      Just (Conf (fromList [("x", 0), ("y", 5)]) [] [] Map.empty)

    let Success _ last = runParser parseL "{ Assign f0 (0); Assign f1 (1); Read n; While (n>1) { Assign t (f0); Assign f0 (f1); Assign f1 (t+f1); Assign n (n-1); }; Write (f1); }"
    eval last
      (initialConf [12]) @?=
      Just (Conf (fromList [("n", 1), ("f0", 89), ("f1", 144), ("t", 55)]) [] [144] Map.empty)


unit_parseDef :: Assertion
unit_parseDef = do
    checkParses parseDef "Def func(a, bc, d) { Write (a); } Returns (e);" (Function {
      name = "func",
      args = ["a", "bc", "d"],
      funBody = Seq {statements = [Assign "e" (Num 0), Write (Ident "a")]},
      returnExpr = Ident "e"
    })
    checkParses parseDef "Def _() { } Returns (0);" (Function {
      name = "_",
      args = [],
      funBody = Seq {statements = []},
      returnExpr = Num 0
    })
    checkParses parseDef "Def func(a, bc, d) Returns (a+bc*r);" (Function {
      name = "func",
      args = ["a", "bc", "d"],
      funBody = Seq {statements = [Assign "r" (Num 0)]},
      returnExpr = BinOp Plus (Ident "a") (BinOp Mult (Ident "bc") (Ident "r"))
    })
    checkParses parseDef "Def F(a, bc, d) { If (a>0) { Assign e (bc+4); } { Assign e (3); }; } Returns (d*e);" (Function {
      name = "F",
      args = ["a", "bc", "d"],
      funBody = Seq {statements = [
        Assign "e" (Num 0),
        If {cond = BinOp Gt (Ident "a") (Num 0),
          thn = Assign "e" (BinOp Plus (Ident "bc") (Num 4)),
          els = Assign "e" (Num 3)}
      ]},
      returnExpr = BinOp Mult (Ident "d") (Ident "e")
    })
    checkParses parseDef "  Def_(x){Assigny(z + x) ;}Returns(x + y) ; " (Function {
      name = "_",
      args = ["x"],
      funBody = Seq {statements = [
          Assign "z" (Num 0),
          Assign "y" (Num 0),
          Assign "y" (BinOp Plus (Ident "z") (Ident "x"))
      ]},
      returnExpr = BinOp Plus (Ident "x") (Ident "y")
    })


unit_parseProg :: Assertion
unit_parseProg = do
    checkParses parseProg "{ Write (x); }" (Program {
      functions = [],
      main = Seq {statements = [Assign "x" (Num 0), Write (Ident "x")]}
    })
    checkParses parseProg "Def f(x) { Write (x); Write (y); } Returns (0); { Read x; Write (f(x+2)); }" (Program {
      functions = [Function {name = "f", args = ["x"],
        funBody = Seq {statements = [
          Assign "y" (Num 0),
          Write (Ident "x"),
          Write (Ident "y")
        ]},
        returnExpr = Num 0
      }],
      main = Seq {statements = [
        Assign "x" (Num 0),
        Read "x",
        Write {expr = FunctionCall "f" [(BinOp Plus (Ident "x") (Num 2))]}
      ]}
    })
    checkParses parseProg " Deff (x ,y){}Returns( 0 ) ;Defg ( ){}\nReturns(f (2 ,2 ) );{Write( g() ) ;} " (Program {
      functions = [Function {name = "f", args = ["x", "y"],
          funBody = Seq {statements = []},
          returnExpr = Num 0
      }, Function {name = "g", args = [],
          funBody = Seq {statements = []},
          returnExpr = FunctionCall "f" [Num 2, Num 2]
      }],
      main = Seq {statements = [
        Write {expr = FunctionCall "g" []}
      ]}
    })
    assertBool "" $ isFailure $ runParser parseProg "{ Write (f(2)); }"
    assertBool "" $ isFailure $ runParser parseProg "Def f(x, y) { }; { Write (f(2)); }"
    assertBool "" $ isFailure $ runParser parseProg "Def f(x) { } Returns 0; { Write (f(2, 3)); }"
    assertBool "" $ isFailure $ runParser parseProg "{ Returns (2); }"


checkPos pos prog lasts = case runParser parseProg prog of
      Success (InputStream lasts' pos') _ -> (pos', lasts') @?= (pos, lasts)
      otherwise                          -> prog @?= "IS NOT PASRING!" 


unit_progPosition :: Assertion
unit_progPosition = do
    checkPos (Position 4 0) "{\n\tRead n;\n\tWrite(n);\n}\n" ""
    checkPos (Position 2 4) "{\n\tRead n;}\n\t👌" "👌"
    checkPos (Position 3 0) "{}\n\n\n" ""
    checkPos (Position 2 8) "{}\n\n\t \t" ""


unit_errors :: Assertion
unit_errors = do
    case runParser parseStatement "" of
      Success _ _  -> "not parsing" @?= "parsing"
      Failure msgs -> msgs @?= [ErrorMsg [
         "Expected \"Assign\"",
         "Expected \"Read\"",
         "Expected \"Write\"",
         "Expected \"{\"",
         "Expected \"If\"",
         "Expected \"While\""
        ] (Position 0 0)]
    case runParser parseStatement "{\n\tR\n}\n" of
      Success _ _  -> "not parsing" @?= "parsing"
      Failure msgs -> msgs @?= [ErrorMsg [
         "Expected \"Assign\"",
         "Expected \"Read\"",
         "Expected \"Write\"",
         "Expected \"If\"",
         "Expected \"While\""
        ] (Position 0 0), ErrorMsg[
         "Expected \"}\""
        ] (Position 1 4)]
    case runParser parseStatement "\n \n  \tRead" of
      Success _ _  -> "not parsing" @?= "parsing"
      Failure msgs -> msgs @?= [ErrorMsg [
         "Expected \"Assign\"",
         "Expected \"Write\"",
         "Expected \"{\"",
         "Expected \"If\"",
         "Expected \"While\""
        ] (Position 2 4), ErrorMsg[
         "Expected identifier",
         "Predicate failed",
         "Expected symbol: \'_\'"
        ] (Position 2 8)]
    case runParser parseStatement "\tWrite (~);" of
     Success _ _  -> "not parsing" @?= "parsing"
     Failure msgs -> case msgs !! 1 of
       ErrorMsg l (Position 0 11) ->
         if not (any (== "Expected expression") l) then (show msgs) @?= "no 'Expected expression' err"
         else if not (any (== "Expected number") l) then (show msgs) @?= "no 'Expected number' err"
         else 1 @?= 1
       _ -> (show msgs) @?= "has Position 0 11"
