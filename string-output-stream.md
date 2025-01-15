create-string-output-string と get-output-strem-string
------------------------------------------------------

strings.Builder のような関数セット

- `(create-string-output-string)` で、文字列バッファへ出力するストリームを作成する
- `(get-output-stream-string S)` でストリーム S から文字列を取り出し、ストリームのバッファをクリアする

```lisp
$ ISLisp.exe
> ISLisp  Version 0.80 (1999/02/25)
>
ISLisp>(let ((s (create-string-output-stream)))
  (format s "first~%")
  (format (error-output) "1: ~S~%" (get-output-stream-string s))
  (format s "second~%")
  (format (error-output) "2: ~S~%" (get-output-stream-string s))
  )
1: "first
"
2: "second
"
NIL
ISLisp>
```
