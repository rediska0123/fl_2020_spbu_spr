## Отчет

### https://github.com/neckbosov/fl_2020_spbu_spr

##### Факториал
```
> let prog = "fun fact(n) { res = 1; while (n > 0) { res = res*n; n = n-1; } return res; } n = 0; read(n); write(fact(n));"
> runParser parseProg prog
Parsing succeeded!
Result:
fact("n") =
|_res := 1
|_while (n > 0)
|_do
|_|_res := (res * n)
|_|_n := (n - 1)

|_return 
res

n := 0
read n
write fact(n)
Suffix:	InputStream {stream = "", curPos = Position {line = 0, col = 108}}
> parseAndEvalProg prog [4]
Just (Conf {subst = fromList [("n",4)], input = [], output = [24], defs = ...)
```

##### НОД
```
> let prog = "fun gcd(a,b) { res=0; if(b==0){res=b;}else{res=gcd(b,a+b);} return res; } a=0;b=0; read(a);read(b); write(gcd(a,b));"
> runParser parseProg prog
Parsing succeeded!
Result:
gcd("a", "b") =
|_res := 0
|_if (b = 0)
|_then
|_|_res := b
|_else
|_|_res := gcd(b, (a + b))

|_return 
res

a := 0
b := 0
read a
read b
write gcd(a, b)
Suffix:	InputStream {stream = "", curPos = Position {line = 0, col = 116}}

```

##### Фибоначчи
```
> let prog = "fun fib(n){ res=0;if(n<=1){res=1;}else{res=fib(n-1)+fib(n-2);}return res; } read(n); write(fib(n));"
> runParser parseProg prog
Parsing succeeded!
Result:
fib("n") =
|_res := 0
|_if (n <= 1)
|_then
|_|_res := 1
|_else
|_|_res := (fib((n - 1)) + fib((n - 2)))

|_return 
res

read n
write fib(n)
Suffix:	InputStream {stream = "", curPos = Position {line = 0, col = 99}}
```

Баг: https://github.com/neckbosov/fl_2020_spbu_spr/issues/1

В целом привычный удобный синтаксис. Прикольно, что не нужно писать ; после if, в отличие от некоторых других LLang.

### https://github.com/myannyax/fl_2020_spbu_spr

##### Перевернуть массив (до первого нуля)
```
> let prog = ".rev.(){read(x);a:=0;if(!(x==0)){a:=.rev.();}else{};write(x);..return..(a);} {_:=.rev.();}"
> runParser parseProg prog
Parsing succeeded!
Result:
rev() =
|_read x
|_a := 0
|_if (! (x = 0))
|_then
|_|_a := rev()
|_else
|_
|_write x
|_return a


_ := rev()
Suffix:	InputStream {stream = "", curPos = 90}
```

##### Считать число в двоичной системе длины `n` справа-налево.
```
> let prog = ".rd.(n){a:=0;if(n==0){}else{read(b);a:=2*.rd.(n-1)+b;};..return..(b);} {read(n);_:=.rd.(n);}"
> runParser parseProg prog
Parsing succeeded!
Result:
rd("n") =
|_a := 0
|_if (n = 0)
|_then
|_
|_else
|_|_read b
|_|_a := ((2 * rd((n - 1))) + b)
|_return b


read n
_ := rd(n)
Suffix:	InputStream {stream = "", curPos = 92}
```

##### Минимальный делитель числа
```
> let f = ".f.(n){i:=2;res:=n;while(res==n&&i*i<=n){if((n/i)*i==n){res:=i;}else{};i:=i+1;};..return..(res);} "
> let main = "{read(n);write(.f.(n));}"
> let prog = f++main
> runParser parseProg prog
Parsing succeeded!
Result:
f("n") =
|_i := 2
|_res := n
|_while ((res = n) && ((i * i) <= n))
|_do
|_|_if (((n / i) * i) = n)
|_|_then
|_| |_res := i
|_|_else
|_
|_|_i := (i + 1)
|_return res


read n
write f(n)
Suffix:	InputStream {stream = "", curPos = 122}
```

* Мне кажется стало бы удобнее разбираться, если добавить в документацию примеров
* Я не очень поняла по документации, можно ли писать объявления функций после определения `main`, видимо судя по работе парсера нельзя
* Остальное хорошо)00
