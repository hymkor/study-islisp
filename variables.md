変数・定数
==========

定数
----

`(defconstant NAME FORM)`

定数を宣言する

```Lisp
ISLisp>(defconstant X 1.1)
X
ISLisp>X
1.1
```

`(setq)` で変更できない

```Lisp
ISLisp>(setq X 2)
> Error at SETQ
> Can't modify Constant: X
>>> ISLisp Debugger menu:
>>>            1    return to toplevel.
>>>           :h    show debugger command help.
```

`(defglobal)` で上書きできない

```Lisp
ISLisp>(defglobal X 3)
> Error at DEFGLOBAL
> Can't modify Constant: X
>>> ISLisp Debugger menu:
>>>            1    return to toplevel.
>>>           :h    show debugger command help.
```

`(let)` で同じ名前の変数を宣言するのは可能

```Lisp
ISLisp>(let ((X 2))
X)
2
```

`(defconstant)` で再宣言することは可能

```Lisp
ISLisp>(defconstant X 2)
X
ISLisp>X
2
```

既存のグローバル変数を上書きすることも可能

```Lisp
ISLisp>(defglobal Y 1)
Y
ISLisp>(defconstant Y 2)
Y
ISLisp>Y
2
```
