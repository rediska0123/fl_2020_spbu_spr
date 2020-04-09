module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           LLang               (LAst (..), parseAssign, parseRead, parseWrite,
                                      parseSeq, parseIf, parseWhile, parseStatement,
                                      parseL, getVars, eval, Configuration (..), initialConf)
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)
import           Data.Map            (empty, fromList)
import qualified Data.Map         as Map

isFailure (Failure _) = True
isFailure  _          = False


unit_parseAssign :: Assertion
unit_parseAssign = do
    runParser parseAssign "Assign x (13)" @?= Success "" (Assign {var = "x", expr = (Num 13)})
    runParser parseAssign "Assign _ (13*42)" @?= Success "" (Assign {var = "_", expr = (BinOp Mult (Num 13) (Num 42))})
    assertBool "" $ isFailure $ runParser parseAssign "Assign x 13"
    assertBool "" $ isFailure $ runParser parseAssign "kekos abrikos"


unit_parseRead :: Assertion
unit_parseRead = do
    runParser parseRead "Read _x kek" @?= Success " kek" (Read {var = "_x"})
    assertBool "" $ isFailure $ runParser parseRead "Assign x (13)"
    assertBool "" $ isFailure $ runParser parseRead "Read (x)"


unit_parseWrite :: Assertion
unit_parseWrite = do
    runParser parseWrite "Write (_x) kek" @?= Success " kek" (Write {expr = (Ident "_x")})
    runParser parseWrite "Write (3-z) kek" @?= Success " kek" (Write {expr = (BinOp Minus (Num 3) (Ident "z"))})
    assertBool "" $ isFailure $ runParser parseWrite "Write 3"


unit_parseSeq :: Assertion
unit_parseSeq = do
    runParser parseSeq "{ }" @?= Success "" (Seq {statements = []})
    runParser parseSeq "{ Read x; }" @?= Success "" (Seq {statements = [Read {var = "x"}]})
    runParser parseSeq "{ Read x; Write (x); }" @?= Success "" (Seq {statements = [Read {var = "x"}, Write {expr = (Ident "x")}]})
    assertBool "" $ isFailure $ runParser parseSeq "{}"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x }"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x;}"
    assertBool "" $ isFailure $ runParser parseSeq "{Read x; }"
    assertBool "" $ isFailure $ runParser parseSeq "{ Read x; Write (x) }"


unit_parseIf :: Assertion
unit_parseIf = do
    runParser parseIf "If (2<3) { } { }" @?= Success "" (If {
       cond = (BinOp Lt (Num 2) (Num 3)),
       thn = Seq{statements = []},
       els = Seq{statements = []}
    })
    runParser parseIf "If (1) { Read x; } { Write (x); }" @?= Success "" (If {
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
    runParser parseWhile "While (2<3) { }" @?= Success "" (While {
       cond = (BinOp Lt (Num 2) (Num 3)),
       body = Seq{statements = []}
    })
    runParser parseWhile "While (1) { Read x; }" @?= Success "" (While {
       cond = (Num 1),
       body = Seq{statements = [Read {var = "x"}]}
    })
    assertBool "" $ isFailure $ runParser parseWhile "While () { }"
    assertBool "" $ isFailure $ runParser parseWhile "While (1) {}"
    assertBool "" $ isFailure $ runParser parseWhile "while (1) { }"
    assertBool "" $ isFailure $ runParser parseWhile "While (0) { Read x }"


unit_getVars :: Assertion
unit_getVars = do
    let Success "" last = runParser parseSeq "{ }" in
        getVars last @?= []
    let Success "" last = runParser parseSeq "{ Assign x (10+10); Read y; Write (d); }" in
        getVars last @?= ["x", "y", "d"]
    let Success "" last = runParser parseSeq "{ If (y+x>10) { Assign a (8); } { Assign b (4); }; }" in
        getVars last @?= ["y", "x", "a", "b"]
    let Success "" last = runParser parseSeq "{ While (y+x>10) { Assign a (8); }; }" in
        getVars last @?= ["y", "x", "a"]


unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "{ }" @?= Success "" (Seq {statements = []})
    runParser parseL "{ Assign x (10+10); While (x>10) { Assign x (x-1); Write (x); }; }" @?= Success "" (Seq {statements = [
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

    runParser parseL "{ Assign x (10+10); While (x>10) { If (x>15) { Read x; } { }; }; }" @?= Success "" (Seq {statements = [
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
        Success "" (Seq {statements = stmts}) = runParser parseSeq program
      in
        runParser parseL program @?= Success "" (Seq {statements = [
            Assign {var = "x", expr = Num 0},
            Assign {var = "a", expr = Num 0},
            Assign {var = "y", expr = Num 0},
            Assign {var = "z", expr = Num 0},
            Assign {var = "b", expr = Num 0},
            Assign {var = "c", expr = Num 0},
            Assign {var = "d", expr = Num 0}
        ] ++ stmts})

    let program = "{ If (y+y>y) { Assign y (8); } { Assign y (4); }; }"
        Success "" (Seq {statements = stmts}) = runParser parseSeq program
      in
        runParser parseL program @?= Success "" (Seq {statements = [
            Assign {var = "y", expr = Num 0}
        ] ++ stmts})

    runParser parseL "{ Write (x); }" @?= Success "" (Seq {statements = [
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
    let Success "" last = runParser parseL "{ Read x; While (x>0) { Write (2*x); Assign x (x-1); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf {subst = fromList [("x", 0)], input = [], output = [2, 4..20]})

    let Success "" last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [10]) @?=
      Just (Conf {subst = fromList [("x", 9), ("y", 0)], input = [], output = [20]})

    let Success "" last = runParser parseL "{ Read x; If (x>0) { Write (2*x); Assign x (x-1); } { Assign y (3*x+5); }; }"
    eval last
      (initialConf [0]) @?=
      Just (Conf {subst = fromList [("x", 0), ("y", 5)], input = [], output = []})

    let Success "" last = runParser parseL "{ Assign f0 (0); Assign f1 (1); Read n; While (n>1) { Assign t (f0); Assign f0 (f1); Assign f1 (t+f1); Assign n (n-1); }; Write (f1); }"
    eval last
      (initialConf [12]) @?=
      Just (Conf {subst = fromList [("n", 1), ("f0", 89), ("f1", 144), ("t", 55)], input = [], output = [144]})


stmt1 :: LAst
stmt1 =
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

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
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
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
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
         (Write (Num 1))
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
