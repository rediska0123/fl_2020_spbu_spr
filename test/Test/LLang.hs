module Test.LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           LLang               (LAst (..), parseAssign, parseRead, parseWrite,
                                      parseSeq, parseIf, parseWhile, parseStatement, parseL)
import           Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

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


unit_parseL :: Assertion
unit_parseL = do
    runParser parseL "{ }" @?= Success "" (Seq {statements = []})
    runParser parseL "{ Assign x (10+10); While (x>10) { Assign x (x-1); Write (x); }; }" @?= Success "" (Seq {statements = [
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
    assertBool "" $ isFailure $ runParser parseL "{ Write (x) }"
    assertBool "" $ isFailure $ runParser parseL "If (1) { } { }"
    assertBool "" $ isFailure $ runParser parseL "While (1) { }"
    assertBool "" $ isFailure $ runParser parseL "Read x"
    assertBool "" $ isFailure $ runParser parseL "{}"
