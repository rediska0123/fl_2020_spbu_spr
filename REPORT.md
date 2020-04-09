# Отчет

## https://github.com/neckbosov/fl_2020_spbu_spr

### Программы

##### Числа Фибоначчи
```
*LLang Combinators> let prog = "n = 0; read(n); f0 = 0; f1 = 1; while (n>1) { tmp = f0; f0 = f1; f1 = f1+tmp; n = n-1; } write(f1);"
*LLang Combinators> let (Success "" ast) = runParser parseL prog
*LLang Combinators> ast
Seq {statements = [Assign {var = "n", expr = 
0},Read {var = "n"},Assign {var = "f0", expr = 
0},Assign {var = "f1", expr = 
1},While {cond = 
>
|_n
|_1, body = Seq {statements = [Assign {var = "tmp", expr = 
f0},Assign {var = "f0", expr = 
f1},Assign {var = "f1", expr = 
+
|_f1
|_tmp},Assign {var = "n", expr = 
-
|_n
|_1}]}},Write {expr = 
f1}]}
*LLang Combinators> eval ast $ initialConf [12]
Just (Conf {subst = fromList [("f0",89),("f1",144),("n",1),("tmp",55)], input = [], output = [144]})
```

##### Факториал
```
*LLang Combinators> let prog = "n = 0; read(n); res = 1; while (n>1) {res = res*n; n = n-1;} write(res);"
*LLang Combinators> let (Success "" ast) = runParser parseL prog
*LLang Combinators> ast
Seq {statements = [Assign {var = "n", expr = 
0},Read {var = "n"},Assign {var = "res", expr = 
1},While {cond = 
>
|_n
|_1, body = Seq {statements = [Assign {var = "res", expr = 
*
|_res
|_n},Assign {var = "n", expr = 
-
|_n
|_1}]}},Write {expr = 
res}
*LLang Combinators> eval ast $ initialConf [6]
Just (Conf {subst = fromList [("n",1),("res",720)], input = [], output = [720]})
```

* Багов не нашла.

* Документацию и язык обсудили: мне в начале показалось, что документация в некоторых местах немного заумная, но автор убедил меня в том, что некоторым людям такое может быть более понятно.


## https://github.com/myannyax/fl_2020_spbu_spr

### Программы

##### С^n_k
```
*LLang Combinators> let prog = "{res := 1; n := 0; k := 0; read(n); read(k); a := n-k;  while(n>k){res := res*n; n := n-1;}; while(a>1){res := res/a; a := a-1;}; write(res);}"
*LLang Combinators> let (Success "" ast) = runParser parseL prog
*LLang Combinators> ast
Seq {statements = [Assign {var = "res", expr = 
1},Assign {var = "n", expr = 
0},Assign {var = "k", expr = 
0},Read {var = "n"},Read {var = "k"},Assign {var = "a", expr = 
-
|_n
|_k},While {cond = 
>
|_n
|_k, body = Seq {statements = [Assign {var = "res", expr = 
*
|_res
|_n},Assign {var = "n", expr = 
-
|_n
|_1}]}},While {cond = 
>
|_a
|_1, body = Seq {statements = [Assign {var = "res", expr = 
/
|_res
|_a},Assign {var = "a", expr = 
-
|_a
|_1}]}},Write {expr = 
res}]}
```

##### a^b
```
*LLang Combinators> let prog = "{a:=0; b:=0; res:=1; read(a); read(b); while(b>0){if(b-(b/2)*2==1){ res:=res*a; b:=b-1; }else{ a:=a*a; b:=b/2; };};}"
*LLang Combinators> let (Success "" ast) = runParser parseL prog
*LLang Combinators> ast
Seq {statements = [Assign {var = "a", expr = 
0},Assign {var = "b", expr = 
0},Assign {var = "res", expr = 
1},Read {var = "a"},Read {var = "b"},While {cond = 
>
|_b
|_0, body = Seq {statements = [If {cond = 
==
|_-
| |_b
| |_*
| | |_/
| | | |_b
| | | |_2
| | |_2
|_1, thn = Seq {statements = [Assign {var = "res", expr = 
*
|_res
|_a},Assign {var = "b", expr = 
-
|_b
|_1}]}, els = Seq {statements = [Assign {var = "a", expr = 
*
|_a
|_a},Assign {var = "b", expr = 
/
|_b
|_2}]}}]}}]}
```
* Баг: https://github.com/myannyax/fl_2020_spbu_spr/issues/1

* Обсудили документацию, в итоге вроде решили, что можно дописать в документацию, разрешаются ли пробелы внутри арифметических выражений, и что делается, если переменная используется без предварительного объявления.
