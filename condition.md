例外処理
=======

一般言語での try-catch-finally に相当する機能は、ISLisp では小さな機能へ分割されている。

with-hander
-----------

`(with-handler HANDLER FORM*)`

`FORM*` 中で例外が発生した時、HANDLER の関数を呼び出す。

```lisp
(with (lambda (c)
       (format (error-output) "ERROR!"))
 (div 2 0)
 )
```

OKI ISLisp で実行すると、`HANDLER` が確かに呼ばれて `ERROR!` が表示される。だが、その後、さらに `Error at WITH-HANDLER Handler return normally` とエラーがでてしまう。

こうならないためには、`HANDLER` 内で

- `(with-handler)` の外のブロックへ処理を移動する (**非局所脱出**)
- 例外が発生した場所で処理を継続する(**継続**)

のいずれかをおこなう。

他の言語のように非局所脱出は `(with-handler)` には組み込まれておらず、明示的におこなわないといけない。

非局所脱出
----------

| 1. 全体ブロック          | 2. 脱出                          | スコープ
|--------------------------|----------------------------------|---------
| `(block NAME FORM*)`     | `(return-from NAME RESULT-FORM)` | 静的
| `(catch TAG-FORM FORM*)` | `(throw NAME RESULT-FORM)`       | 動的

※  他、`(tagbody)` &amp; `(go)` というものもあるが、ここでは省略する

1.の `FORM*` 中で 2. が実行されると、1. のブロックの後に処理が移動する。その際、ブロック全体の値として 2. の `RESULT-FORM` が使われる。

##### `(block)` &amp; `(return-from)` の例

```lisp
(block main
 (with-handler (lambda (c) (return-from main "OK"))
  (with-handler (lambda (c))
   (div 2 0))))
```

##### `(catch)` &amp; `(throw)` の例

```lisp
(catch 'err
 (with-handler (lambda (c) (throw 'err "OK"))
  (with-handler (lambda (c))
   (div 2 0))))
```

両者は似ているが、静的動的の違いがある。

##### `(block)` &amp; `(return-from)` の場合

```lisp
(defun foo ()
 (with-handler (lambda (c) (return-from main "OK"))
  (with-handler (lambda (c))
   (div 2 0))))
(block main
    (foo))
```

→ `Error at RETURN-FROM` `Block Tag not found: MAIN` というエラーになる。

##### `(catch)` &amp; `(throw)` の場合

```lisp
(defun foo ()
 (with-handler (lambda (c) (throw 'err "OK"))
  (with-handler (lambda (c))
   (div 2 0))))
(catch 'err
    (foo))
```

→ 結果は `OK` となる

`(block)`,`(return-from)` はC言語の break/return 的な用途に使うもので、C++/Javaの throw 的な大域脱出は文字どおりの `(throw)` を使うと考えた方がよいだろう。

継続
----

例外が発生した時、例外によっては、処理を中断せずに継続させられる場合がある。

```lisp
(with-handler
 (lambda (c)
  (continue-condition c 10))
 (div 2 0))
```

OKI ISLisp だと `<division-by-zero>` は継続可能な例外ではないため、`Error at CONTINUE-CONDITION` `Condition is not continuable` となってしまうが、gmnlisp だと継続可能なので `10` になる。

継続エラーを明示的に起こすコマンド `(cerror)` で OKI ISLisp でも試してみると：

```lisp
(with-handler
 (lambda (c)
  (continue-condition c "CONTINUED"))
 (cerror "CONTINUE-STRING" "ERROR-STRING"))
```

→ 無事、`(with-handler...)` の値は `"CONTINUED"` になる

なお、例外が継続可能かどうか判別する関数 `(condition-continuable CONDITION)` というものもあり、これを使うと継続できるときだけ継続ということもできる

```lisp
(catch 'fail
 (with-handler
  (lambda (c)
   (if (condition-continuable c)
    (continue-condition c "CONTINUED"))
   (throw 'fail "FAIL-CONTINUE"))
  (cerror "CONTINUE-STRING" "ERROR-STRING"))
 )
```

→ `"CONTINUED"`

```lisp
(catch 'fail
 (with-handler
  (lambda (c)
   (if (condition-continuable c)
    (continue-condition c "TRY-CONTINUE"))
   (throw 'fail "FAIL-CONTINUE"))
  (div 3 0))
 )
```

→ `"FAIL-CONTINUE"`
