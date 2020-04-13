module Test.LLang where

import           AST
import           Combinators      (Parser (..), Result (..), runParser, toStream, symbol)
import qualified Data.Map         as Map
import           Data.Map            (empty, fromList)
import           Debug.Trace      (trace)
import           LLang            (Configuration (..), LAst (..), eval,
                                   initialConf, parseL, Function (..), Program (..),
                                   parseAssign, parseRead, parseWrite, parseSeq,
                                   parseIf, parseWhile, parseStatement,
                                   getVars, eval, parseDef, parseProg,
                                   parseFunctionSeq, getFCalls)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

-- f x y = read z ; return (x + z * y)
-- g x = if (x) then return x else return x*13
-- {read x; read y; write (f x y); write (g x)}"

prog =
  Program
    [ Function "f" ["x", "y"] (Seq [Read "z", Return (BinOp Plus (Ident "x") (Ident "y"))])
    , Function "g" ["x"] (If (Ident "x") (Return (Ident "x")) (Return (BinOp Mult (Ident "x") (Num 13))))
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "x"
    , If (BinOp Gt (Ident "x") (Num 13))
         (Seq [(Write (Ident "x"))])
         (Seq [(While (BinOp Lt (Ident "x") (Num 42))
                (Seq [ Assign "x"
                        (BinOp Mult (Ident "x") (Num 7))
                     , Write (Ident "x")
                     ]
                )
         )])
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (Seq [(While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )])
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Seq [(Write (Num 1))])
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing




isFailure (Failure _) = True
isFailure  _          = False

checkParses :: (Eq r, Show r) => Parser String String r -> String -> r -> Assertion
checkParses parser prog last = do
    runParser parser prog @?= Success (toStream "" (length prog)) last


unit_parseAssign :: Assertion
unit_parseAssign = do
    checkParses parseAssign "Assign x (13)" (Assign {var = "x", expr = (Num 13)})
    checkParses parseAssign "Assign _ (13*42)" (Assign {var = "_", expr = (BinOp Mult (Num 13) (Num 42))})
    assertBool "" $ isFailure $ runParser parseAssign "Assign x 13"
    assertBool "" $ isFailure $ runParser parseAssign "kekos abrikos"


unit_parseRead :: Assertion
unit_parseRead = do
    checkParses parseRead "Read _x" (Read {var = "_x"})
    assertBool "" $ isFailure $ runParser parseRead "Assign x (13)"
    assertBool "" $ isFailure $ runParser parseRead "Read (x)"


unit_parseWrite :: Assertion
unit_parseWrite = do
    checkParses parseWrite "Write (_x)" (Write {expr = (Ident "_x")})
    checkParses parseWrite "Write (3-z)" (Write {expr = (BinOp Minus (Num 3) (Ident "z"))})
    assertBool "" $ isFailure $ runParser parseWrite "Write 3"


unit_parseSeq :: Assertion
unit_parseSeq = do
    checkParses parseSeq "{ }" (Seq {statements = []})
    checkParses parseSeq "{ Read x; }" (Seq {statements = [Read {var = "x"}]})
    checkParses parseSeq "{ Read x; Write (x); }" (Seq {statements = [Read {var = "x"}, Write {expr = (Ident "x")}]})
    assertBool "" $ isFailure $ runParser parseSeq "{}"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x }"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x;}"
    assertBool "" $ isFailure $ runParser parseSeq "{Read x; }"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x; Write (x) }"


unit_parseIf :: Assertion
unit_parseIf = do
    checkParses parseIf "If (2<3) { } { }" (If {
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
    assertBool "" $ isFailure $ runParser parseIf "If (1) { }{ }"
    assertBool "" $ isFailure $ runParser parseIf "If (1) {} { }"
    assertBool "" $ isFailure $ runParser parseIf "If (1) { } {}"
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
    checkParses parseWhile "While (1) { Read x; }" (While {
       cond = (Num 1),
       body = Seq{statements = [Read {var = "x"}]}
    })
    assertBool "" $ isFailure $ runParser parseWhile "While () { }"
    assertBool "" $ isFailure $ runParser parseWhile "While (1) {}"
    assertBool "" $ isFailure $ runParser parseWhile "while (1) { }"
    assertBool "" $ isFailure $ runParser parseWhile "While (0) { Read x }"


unit_getVars :: Assertion
unit_getVars = do
    let Success _ last = runParser parseSeq "{ }" in
        getVars last @?= []
    let Success _ last = runParser parseSeq "{ Assign x (10+10); Read y; Write (d); }" in
        getVars last @?= ["d", "y", "x"]
    let Success _ last = runParser parseFunctionSeq "{ If (y+x>10) { Assign a (8); } { Return (b*6); }; }" in
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

    let program = "{ Assign x (10-(x)*7); If (a*3==5) { Read y; } { Write (z); }; While (b*10+c) { Read c; Read d; }; }"
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

    let program = "{ If (y+y>y) { Assign y (8); } { Assign y (4); }; }"
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
    assertBool "" $ isFailure $ runParser parseL "{}"


unit_eval :: Assertion
unit_eval = do
    let Success _ last = runParser parseL "{ Read x; While (x>0) { Write (2*x); Assign x (x-1); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf {subst = fromList [("x", 0)], input = [], output = [2, 4..20]})

    let Success _ last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf {subst = fromList [("x", 9), ("y", 0)], input = [], output = [20]})

    let Success _ last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [0]) @?=
      Just (Conf {subst = fromList [("x", 0), ("y", 5)], input = [], output = []})

    let Success _ last = runParser parseL "{ Assign f0 (0); Assign f1 (1); Read n; While (n>1) { Assign t (f0); Assign f0 (f1); Assign f1 (t+f1); Assign n (n-1); }; Write (f1); }"
    eval last
      (initialConf [12]) @?=
      Just (Conf {subst = fromList [("n", 1), ("f0", 89), ("f1", 144), ("t", 55)], input = [], output = [144]})--}


unit_parseDef :: Assertion
unit_parseDef = do
    checkParses parseDef "Def func(a, bc, d) { Write (a); }" (Function {
      name = "func",
      args = ["a", "bc", "d"],
      funBody = Seq {statements = [Write (Ident "a"), Return (Num 0)]}
    })
    checkParses parseDef "Def _() { }" (Function {
      name = "_",
      args = [],
      funBody = Seq {statements = [Return (Num 0)]}
    })
    checkParses parseDef "Def F(a, bc, d) { If (a>0) { Return (bc+4); } { Assign d (3); }; }" (Function {
      name = "F",
      args = ["a", "bc", "d"],
      funBody = Seq {statements = [
        If {cond = BinOp Gt (Ident "a") (Num 0),
          thn = Return (BinOp Plus (Ident "bc") (Num 4)),
          els = Assign "d" (Num 3)},
        Return (Num 0)
      ]}
    })
    checkParses parseDef "Def _(x) { Assign y (z+x); Return (x+y); }" (Function {
      name = "_",
      args = ["x"],
      funBody = Seq {statements = [
          Assign "z" (Num 0),
          Assign "y" (Num 0),
          Assign "y" (BinOp Plus (Ident "z") (Ident "x")),
          Return (BinOp Plus (Ident "x") (Ident "y")),
          Return (Num 0)
      ]}
    })


unit_parseProg :: Assertion
unit_parseProg = do
    checkParses parseProg "{ Write (x); }" (Program {
      functions = [],
      main = Seq {statements = [Assign "x" (Num 0), Write (Ident "x")]}
    })
    checkParses parseProg "Def f(x) { Write (x); Write (y); }; { Read x; Write (f(x+2)); }" (Program {
      functions = [Function {name = "f", args = ["x"],
          funBody = Seq {statements = [
          Assign "y" (Num 0),
          Write (Ident "x"),
          Write (Ident "y"),
          Return (Num 0)
        ]}
      }],
      main = Seq {statements = [
        Assign "x" (Num 0),
        Read "x",
        Write {expr = FunctionCall "f" [(BinOp Plus (Ident "x") (Num 2))]}
      ]}
    })
    checkParses parseProg "Def f(x, y) { }; Def g() { Return (f(2, 2)); }; { Write (g()); }" (Program {
      functions = [Function {name = "f", args = ["x", "y"],
          funBody = Seq {statements = [ Return (Num 0) ]}
      }, Function {name = "g", args = [],
          funBody = Seq {statements = [ Return (FunctionCall "f" [Num 2, Num 2]), Return (Num 0) ]}
      }],
      main = Seq {statements = [
        Write {expr = FunctionCall "g" []}
      ]}
    })
    assertBool "" $ isFailure $ runParser parseProg "{ Write (f(2)); }"
    assertBool "" $ isFailure $ runParser parseProg "Def f(x, y) { }; { Write (f(2)); }"
    assertBool "" $ isFailure $ runParser parseProg "Def f(x) { }; { Write (f(2, 3)); }"
    assertBool "" $ isFailure $ runParser parseProg "{ Return 2; }"
