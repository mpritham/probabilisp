import Control.Monad hiding (join)
import Control.Monad.Except hiding (join)
import Control.Monad.State hiding (join)
import Core
import Eval
import ParserCombinators
import Prob
import Runtime
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit

withRuntime :: [String] -> [String]
withRuntime codes = aux runtime codes
  where
    aux runtime [] = []
    aux runtime (code : codes) =
      case run rawExprP code of -- Parse
        [(expr, s)] ->
          case runExcept $ runStateT (eval expr) runtime of -- Eval
            Left err -> [err_type err] -- Diagnostics
            Right (val, env) -> show val : aux env codes
        [] -> ["ohno"]

testsArithRuntime =
  [ testCase "+" (withRuntime ["(+)"] @?= ["0"]),
    testCase "-" (withRuntime ["(-)"] @?= ["0"]),
    testCase "*" (withRuntime ["(*)"] @?= ["1"]),
    testCase "+ arith" (withRuntime ["(+ 3 2 5)"] @?= ["10"]),
    testCase "- arith" (withRuntime ["(- 3 4 5)"] @?= ["-6"]),
    testCase "* arith" (withRuntime ["(* 7 8 10)"] @?= ["560"])
  ]

testsBoolRuntime =
  [ testCase "and 1" (withRuntime ["(and #t #t #t #t)"] @?= ["#t"]),
    testCase "and 2" (withRuntime ["(and #t #t #t #t)"] @?= ["#t"]),
    testCase "and 3" (withRuntime ["(and #t #t 'nil)"] @?= ["#t"]),
    testCase "and 4" (withRuntime ["(and 't 't #f #t)"] @?= ["#f"]),
    testCase "and 5" (withRuntime ["(and 't 't 't 't 't 't 't 't 't 't #f #t)"] @?= ["#f"]),
    testCase "and 6" (withRuntime ["(and 3 4 2 't)"] @?= ["#t"]),
    testCase "and 7" (withRuntime ["(and 3 4 2 3 4 2 3 4 2 3 4 2 3 4 2 't)"] @?= ["#t"]),
    testCase "or 1" (withRuntime ["(or #t #t #t #t)"] @?= ["#t"]),
    testCase "or 2" (withRuntime ["(or #t #t #t #f)"] @?= ["#t"]),
    testCase "or 3" (withRuntime ["(or 'nil 3 5 #t)"] @?= ["#t"]),
    testCase "and" (withRuntime ["(and)"] @?= ["#t"]),
    testCase "or" (withRuntime ["(or)"] @?= ["#f"])
  ]

testsCompRuntime =
  [ testCase "<" (withRuntime ["(<)"] @?= ["#t"]),
    testCase ">" (withRuntime ["(>)"] @?= ["#t"]),
    testCase "<=" (withRuntime ["(<=)"] @?= ["#t"]),
    testCase ">=" (withRuntime ["(>=)"] @?= ["#t"]),
    testCase "=" (withRuntime ["(=)"] @?= ["#t"]),
    testCase "< comp 1" (withRuntime ["(< 3 4 5)"] @?= ["#t"]),
    testCase "< comp 2" (withRuntime ["(< 3 3 5)"] @?= ["#f"]),
    testCase "> comp 1" (withRuntime ["(> 3 4 5)"] @?= ["#f"]),
    testCase "> comp 2" (withRuntime ["(> 100 10 3 1 0)"] @?= ["#t"]),
    testCase "= comp 1" (withRuntime ["(= 3 3 3)"] @?= ["#t"]),
    testCase "= comp 2" (withRuntime ["(= 3 3 1)"] @?= ["#f"])
  ]

testsListOps =
  [ testCase "car" (withRuntime ["(car '(10))"] @?= ["10"]),
    testCase "cdr 1" (withRuntime ["(cdr '(10 20 30))"] @?= ["(20 30)"]),
    testCase "cdr 2" (withRuntime ["(cdr '(10))"] @?= ["()"]),
    testCase "cons 1" (withRuntime ["(cons 10 20)"] @?= ["(10 . 20)"]),
    testCase "cons 2" (withRuntime ["(cons '() '(. ()))"] @?= ["(())"]),
    testCase "car cons 1" (withRuntime ["(car (cons 10 (cons 20 '())))"] @?= ["10"]),
    testCase "car cons 2" (withRuntime ["(car (cons 2 3))"] @?= ["2"]),
    testCase "cdr cons" (withRuntime ["(cdr (cons 2 3))"] @?= ["3"]),
    testCase "list 1" (withRuntime ["(list 'a 'b 'z 'd 'q)"] @?= ["(a b z d q)"]),
    testCase "list 2" (withRuntime ["(list (+ 10 20) (+ 30 40))"] @?= ["(30 70)"]),
    testCase "cons list" (withRuntime ["(cons 10 (list 20 30))"] @?= ["(10 20 30)"])
  ]

testsUnaryRuntime =
  [ testCase "not 1" (withRuntime ["(not)"] @?= ["ERROR: unexpected_args"]),
    testCase "not 2" (withRuntime ["(not 77 2)"] @?= ["ERROR: unexpected_args"]),
    testCase "not 3" (withRuntime ["(not #t)"] @?= ["#f"]),
    testCase "not 4" (withRuntime ["(not #t)"] @?= ["#f"]),
    testCase "not 5" (withRuntime ["(not '#f)"] @?= ["#t"]),
    testCase "not 6" (withRuntime ["(not #f)"] @?= ["#t"]),
    testCase "not 7" (withRuntime ["(not #f #t)"] @?= ["ERROR: unexpected_args"]),
    testCase "not 8" (withRuntime ["(not #f #t 3)"] @?= ["ERROR: unexpected_args"]),
    testCase "not 9" (withRuntime ["(not #f 3)"] @?= ["ERROR: unexpected_args"]),
    testCase "not 10" (withRuntime ["(not 42)"] @?= ["#f"])
  ]

testsEquality =
  [ testCase "=" (withRuntime ["(=)"] @?= ["#t"]),
    testCase "eq?" (withRuntime ["(eq?)"] @?= ["#t"]),
    testCase "= 1" (withRuntime ["(= 10 20)"] @?= ["#f"]),
    testCase "= 2" (withRuntime ["(= 10 10)"] @?= ["#t"]),
    testCase "= 3" (withRuntime ["(= 10 10 10)"] @?= ["#t"]),
    testCase "= 4" (withRuntime ["(= 10 10 10 10 10 10 10 10 10 10 10 10 ((lambda (x) 10) 1) 10 10 10)"] @?= ["#t"]),
    testCase "= 5" (withRuntime ["(= 10 10 10 10 30 10 10 10)"] @?= ["#f"]),
    testCase "= 6" (withRuntime ["(= 'a 'a)"] @?= ["ERROR: type_error"]),
    testCase "= define 1" (withRuntime ["(define x ((lambda () (+ 20 100))))", "(= ''120 ''120)"] @?= ["", "ERROR: type_error"]),
    testCase "= define 2" (withRuntime ["(define x ((lambda () (+ 20 100))))", "(define y x)", "(= x 120)", "(= x y)", "(= 'y 'y)"] @?= ["", "", "#t", "#t", "ERROR: type_error"]),
    testCase "= lambda" (withRuntime ["(= (lambda (x) (x)) (lambda (x) (x)))"] @?= ["ERROR: type_error"]),
    testCase "eq? 1" (withRuntime ["(eq? 'a 'a)"] @?= ["#t"]),
    testCase "eq? 2" (withRuntime ["(eq? 'a 'a 'a)"] @?= ["#t"]),
    testCase "eq? 3" (withRuntime ["(eq? 'a 'a 'b 'a (car (cons 'a 'b)))"] @?= ["#f"]),
    testCase "eq? 4" (withRuntime ["(eq? '() '())"] @?= ["#f"]),
    testCase "eq? define" (withRuntime ["(define a (- (- 10)))", "(eq? 10 a)"] @?= ["", "#t"]),
    testCase "eq? lambda 1" (withRuntime ["(eq? (lambda () ()) (lambda (f) (lambda () ())))"] @?= ["#f"]),
    testCase "eq? lambda 2" (withRuntime ["(eq? ((lambda () 1)) (((lambda () (lambda () 1)))))"] @?= ["#t"])
  ]

testsModulo =
  [ testCase "modulo 1" (withRuntime ["(modulo)"] @?= ["ERROR: unexpected_args"]),
    testCase "modulo 2" (withRuntime ["(modulo 1)"] @?= ["ERROR: unexpected_args"]),
    testCase "modulo 3" (withRuntime ["(modulo 4 3)"] @?= ["1"]),
    testCase "modulo 4" (withRuntime ["(modulo 9 3)"] @?= ["0"]),
    testCase "modulo 5" (withRuntime ["(modulo 9 (- 2))"] @?= ["-1"]),
    testCase "modulo 6" (withRuntime ["(modulo (- 9) 2)"] @?= ["1"])
  ]

testsTypePreds =
  [ testCase "symbol? 1" (withRuntime ["(symbol? 'a)"] @?= ["#t"]),
    testCase "symbol? 2" (withRuntime ["(symbol? 'b)"] @?= ["#t"]),
    testCase "symbol? 3" (withRuntime ["(symbol?)"] @?= ["ERROR: unexpected_args"]),
    testCase "symbol? 4" (withRuntime ["(symbol? 3)"] @?= ["#f"]),
    testCase "symbol? 5" (withRuntime ["(symbol? (car ((lambda (x) (cons x '(3 5))) 'y)))"] @?= ["#t"]),
    testCase "list? 1" (withRuntime ["(list? '(3 5))"] @?= ["#t"]),
    testCase "list? 2" (withRuntime ["(list? '())"] @?= ["#t"]),
    testCase "list? 3" (withRuntime ["(list? 3)"] @?= ["#f"]),
    testCase "list? 4" (withRuntime ["(list? 3 5)"] @?= ["ERROR: unexpected_args"]),
    testCase "list? 5" (withRuntime ["(list?)"] @?= ["ERROR: unexpected_args"]),
    testCase "pair? 1" (withRuntime ["(pair?)"] @?= ["ERROR: unexpected_args"]),
    testCase "pair? 2" (withRuntime ["(pair? 3)"] @?= ["#f"]),
    testCase "pair? 3" (withRuntime ["(pair? ((lambda (x) (cons x '(3 5))) 'x))"] @?= ["#t"]),
    testCase "pair? 4" (withRuntime ["(pair? '())"] @?= ["#f"]),
    testCase "pair? 5" (withRuntime ["(pair? 1 2)"] @?= ["ERROR: unexpected_args"]),
    testCase "number? 1" (withRuntime ["(number? '(3))"] @?= ["#f"]),
    testCase "number? 2" (withRuntime ["(number? ((lambda () 3)))"] @?= ["#t"]),
    testCase "number? 3" (withRuntime ["(number? 10)"] @?= ["#t"]),
    testCase "number? 4" (withRuntime ["(number? #t)"] @?= ["#f"]),
    testCase "number? 5" (withRuntime ["(number?)"] @?= ["ERROR: unexpected_args"]),
    testCase "number? 6" (withRuntime ["(define n ((lambda (x) (+ 1 x)) 3))", "(number? n)"] @?= ["", "#t"]),
    testCase "boolean? 1" (withRuntime ["(boolean? (lambda (x) 10))"] @?= ["#f"]),
    testCase "boolean? 2" (withRuntime ["(boolean? #f)"] @?= ["#t"]),
    testCase "boolean? 3" (withRuntime ["(boolean? 'True)", "(boolean? ''#t)", "(boolean? ''#f)"] @?= ["#f", "#f", "#f"]),
    testCase "boolean? 4" (withRuntime ["(boolean?)"] @?= ["ERROR: unexpected_args"]),
    testCase "boolean? 5" (withRuntime ["(boolean? 3 #f)"] @?= ["ERROR: unexpected_args"]),
    testCase "null? 1" (withRuntime ["(null? '())"] @?= ["#t"]),
    testCase "null? 2" (withRuntime ["(null? '('()))"] @?= ["#f"]),
    testCase "null? 3" (withRuntime ["(null? '(3 5))"] @?= ["#f"]),
    testCase "null? 4" (withRuntime ["(null? '(. (. ())))"] @?= ["#t"]),
    testCase "null? 5" (withRuntime ["(null? (cons '() '(. ())))"] @?= ["#f"]),
    testCase "null? 6" (withRuntime ["(null? '() '())"] @?= ["ERROR: unexpected_args"])
  ]

testsAtoms =
  [ testCase "int" (withRuntime ["10"] @?= ["10"]),
    testCase "bool" (withRuntime ["#t", "#f"] @?= ["#t", "#f"])
  ]

testsLookup =
  [ testCase "prim" (withRuntime ["+", "-"] @?= ["#<primitive>", "#<primitive>"]),
    testCase "undef" (withRuntime ["Mattox"] @?= ["ERROR: undef_symbol"])
  ]

testsDefine =
  [ testCase "define 1" (withRuntime ["(define x 10)", "x", "y"] @?= ["", "10", "ERROR: undef_symbol"]),
    testCase "define 2" (withRuntime ["(define x (+ 6 5))", "x"] @?= ["", "11"]),
    testCase "define 3" (withRuntime ["(define (id x) x)", "(id 10)"] @?= ["", "10"]),
    testCase "define 4" (withRuntime ["(define (inc y) (+ y 1))", "(inc 10)"] @?= ["", "11"]),
    testCase "define 5" (withRuntime ["(define (plus a b) (+ a b))", "(plus 10 20)", "(plus (plus 10 20) (plus 30 40))"] @?= ["", "30", "100"])
  ]

testsLambda =
  [ testCase "lambda 1" (withRuntime ["(lambda (x) (+ x 10))"] @?= ["#<function:(\955 (x) ...)>"]),
    testCase "lambda 2" (withRuntime ["((lambda (x) (+ (((lambda () (lambda () x)))) 10)) 20)"] @?= ["30"]),
    testCase "lambda 3" (withRuntime ["(lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg))))))"] @?= ["#<function:(\955 (X) ...)>"]),
    testCase "lambda 4" (withRuntime ["(lambda X (lambda (f) (f X)))"] @?= ["ERROR: invalid_special_form"]),
    testCase "lambda 5" (withRuntime ["(define foo (lambda (x) (+ 10 x)))", "(foo 20)"] @?= ["", "30"])
  ]

testsHofs =
  [ testCase "hof 1" (withRuntime ["(define (twice f x) (f (f x)))", "(define (inc x) (+ x 10))", "(twice inc 10)"] @?= ["", "", "30"]),
    testCase "hof 2" (withRuntime ["(define (twice f x) (f (f x)))", "(define g ((lambda () twice)))", "(define (inc x) (+ x 10))", "(g inc 10)"] @?= ["", "", "", "30"]),
    testCase "hof 3" (withRuntime ["(define Y (lambda (X) ((lambda (f) (X (lambda (arg) ((f f) arg)))) (lambda (f) (X (lambda (arg) ((f f) arg)))))))", "(define fact (Y (lambda (f) (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1)))))))))", "(fact 10)"] @?= ["", "", "3628800"])
  ]

testsCond =
  [ testCase "cond 1" (withRuntime ["(cond (#f 1) (#f 2))"] @?= [""]),
    testCase "cond 2" (withRuntime ["(cond (else 1) (#t 3))"] @?= ["ERROR: invalid_special_form"]),
    testCase "cond 3" (withRuntime ["(cond)"] @?= ["ERROR: invalid_special_form"]),
    testCase "cond 4" (withRuntime ["(cond ('a 1) ((+ 1 2) (+ 3 4)) (else 5))"] @?= ["1"]),
    testCase "cond 5" (withRuntime ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] @?= ["a"]),
    testCase "cond 6" (withRuntime ["(cond (#f 'a) ((- 4 2) 'b))"] @?= ["b"]),
    testCase "cond 7" (withRuntime ["(cond ((+ 4 3) 'a) ((- 4 2) 'b))"] @?= ["a"]),
    testCase "cond 8" (withRuntime ["(cond (False 'a) ((- 4 2) 'b))"] @?= ["ERROR: undef_symbol"]),
    testCase "cond 9" (withRuntime ["(cond (True 'a) ((- 4 2) 'b))"] @?= ["ERROR: undef_symbol"]),
    testCase "cond 10" (withRuntime ["(cond ((not 'a) 1) ((+ 1 2) (+ 3 4)) (else 5))"] @?= ["7"]),
    testCase "cond 11" (withRuntime ["(define (fact n) (cond ((< n 1) 1) (else (* n (fact (- n 1))))))", "(fact 5)"] @?= ["", "120"]),
    testCase "cond 12" (withRuntime ["(cond (#f 'a) (#f (cond (#f 1))) (((lambda () 'a)) (cond ((+ 1 2 3) 7))))"] @?= ["7"])
  ]

testsLet =
  [ testCase "let 1" (withRuntime ["(let ((x 5) (y 10)) (+ x y)) "] @?= ["15"]),
    testCase "let 2" (withRuntime ["(define x 20)", "(define y 30)", "(let ((x 11) (y 4)) (- (* x y) 2))"] @?= ["", "", "42"]),
    testCase "let 3" (withRuntime ["(define x 20)", "(define y 30)", "(let ((x 11) (y 4)) (- (* x y) 2))", "x", "y"] @?= ["", "", "42", "20", "30"]),
    testCase "let 4" (withRuntime ["(define x 20)", "(define y 30)", "(let ((x 11) (y x)) (- (* x y) 2))"] @?= ["", "", "218"]),
    testCase "let 5" (withRuntime ["(define x 20)", "(define y 30)", "(let ((x 11) (y x)) (- (* x y) 2))", "x", "y"] @?= ["", "", "218", "20", "30"])
  ]

testsQuoteEval =
  [ testCase "quote 1" (withRuntime ["'a"] @?= ["a"]),
    testCase "quote 2" (withRuntime ["'5"] @?= ["5"]),
    testCase "quote 3" (withRuntime ["(quote a)"] @?= ["a"]),
    testCase "quote 4" (withRuntime ["'*first-val*"] @?= ["*first-val*"]),
    testCase "quote 5" (withRuntime ["''a"] @?= ["(quote a)"]),
    testCase "quote 6" (withRuntime ["(car (quote (a b c)))"] @?= ["a"]),
    testCase "quote 7" (withRuntime ["(define (make-list x y z) `(,x ,y ,z))", "(car (make-list ((lambda (b) b) 'a) 'b 'c))"] @?= ["", "a"]),
    testCase "quote 8" (withRuntime ["(car ''(a b c))"] @?= ["quote"]),
    testCase "quote 9" (withRuntime ["'(2 3 4)"] @?= ["(2 3 4)"]),
    testCase "quote 11" (withRuntime ["(list (+ 2 3))"] @?= ["(5)"]),
    testCase "quote 12" (withRuntime ["'( (+ 2 3))"] @?= ["((+ 2 3))"]),
    testCase "quote 13" (withRuntime ["'(+ 2 3)"] @?= ["(+ 2 3)"]),
    testCase "quote 14" (withRuntime ["(eval '(+ 1 2))"] @?= ["3"]),
    testCase "quote 15" (withRuntime ["(eval ''(+ 1 2))"] @?= ["(+ 1 2)"]),
    testCase "quote 16" (withRuntime ["(eval (eval ''(+ 1 2)))"] @?= ["3"]),
    testCase "quote 17" (withRuntime ["(define a '(+ x 1))", "(define x 5)", "(eval a)", "(define a 5)", "``(+ ,,a 1)", "``(+ ,,a ,a)", "`(+ a ,,a)", "``(+ a ,,a)", "(eval ``(+ ,,a 1))", "(eval (eval ``(+ ,,a 1)))"] @?= ["", "", "6", "", "(quasiquote (+ (unquote 5) 1))", "(quasiquote (+ (unquote 5) (unquote a)))", "ERROR: unquote_notin_quasiquote"]),
    testCase "quote 18" (withRuntime ["(define a '(+ x 1))", "(define x 5)", "(eval a)", "(define a 5)", "``(+ ,,a 1)", "``(+ ,,a ,a)", "``(+ a ,,a)", "(eval ``(+ ,,a 1))", "(eval (eval ``(+ ,,a 1)))"] @?= ["", "", "6", "", "(quasiquote (+ (unquote 5) 1))", "(quasiquote (+ (unquote 5) (unquote a)))", "(quasiquote (+ a (unquote 5)))", "(+ 5 1)", "6"])
  ]

testsEval =
  [ testCase "eval 1" (withRuntime ["(eval 1)"] @?= ["1"]),
    testCase "eval 2" (withRuntime ["(eval 'a)"] @?= ["ERROR: undef_symbol"]),
    testCase "eval 3" (withRuntime ["(eval '(lambda (f x) (f x)))"] @?= ["#<function:(\955 (f x) ...)>"]),
    testCase "eval 4" (withRuntime ["(eval '(eval '(eval '(+ ((lambda (x y z) 1) '() '() '()) 2 3))))"] @?= ["6"])
  ]

testsExtraRuntime =
  [ testCase "extra 1" (withRuntime ["(pair? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 2)))))))"] @?= ["#t"]),
    testCase "extra 2" (withRuntime ["(pair? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] @?= ["ERROR: undef_symbol"]),
    testCase "extra 3" (withRuntime ["(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '(1 2)))))))"] @?= ["#t"]),
    testCase "extra 4" (withRuntime ["(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] @?= ["ERROR: undef_symbol"]),
    testCase "extra 5" (withRuntime ["(null? '(. (. (. (.  (.  () ))))))"] @?= ["#t"]),
    testCase "extra 6" (withRuntime ["(null? '(. (. (. (.  (.  1 ))))))"] @?= ["#f"]),
    testCase "extra 7" (withRuntime ["(null? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 zoinks))))))"] @?= ["ERROR: undef_symbol"])
  ]

testsProb =
  [ testCase
      "dice example"
      ( withRuntime
          [ "(define die (uniform '(1 2 3 4 5 6)))",
            "(define (dice n) (cond ((= n 0) (uniform '( '()))) (else (concatP die (dice (- n 1))))))",
            "(define (filter pred xs) (cond ((null? xs) xs) ((pred (car xs)) (cons (car xs) (filter pred (cdr xs)))) (else (filter pred (cdr xs)))))",
            "(define (length xs) (cond ((null? xs) 0) (else (+ 1 (length (cdr xs))))))",
            "(define (eq6? x) (= 6 x))",
            "(define (pred xs) (>= (length (filter eq6? xs)) 2))",
            "(?? pred (dice 4))"
          ]
          @?= ["", "", "", "", "", "", "0.13194445"]
      ),
    testCase
      "marbles example"
      ( withRuntime
          [ "(define (pred xs) (let ((i1 (car xs)) (i2 (car (cdr xs))) (i3 (car (cdr (cdr xs))))) (and (eq? i1 'r) and (eq? i2 'g) (eq? i3 'b)))))",
            "(?? pred (select 3 '('r 'r 'g 'g 'b)))"
          ]
          @?= ["", "6.666667e-2"]
      ),
    testCase
      "cards example"
      ( withRuntime
          [ "(define (length xs) (cond ((null? xs) 0) (else (+ 1 (length (cdr xs))))))",
            "(define (listN n) (cond ((= 0 n) '()) (else (cons n (listN (- n 1))))))",
            "(define (predH xs) (cond ((<= (length xs) 1) #f) ((= (car xs) (car (cdr xs))) #t) (else (predH (cdr xs))))))",
            "(define (pred xs) (predH (sort xs)))",
            "(?? pred (sample 2 (listN 52)))"
          ]
          @?= ["", "", "", "", "1.9230774e-2"]
      )
  ]

tests =
  [ testGroup
      "Runtime"
      [ testGroup "arith" testsArithRuntime,
        testGroup "bool" testsBoolRuntime,
        testGroup "comp" testsCompRuntime,
        testGroup "list" testsListOps,
        testGroup "unary" testsUnaryRuntime,
        testGroup "equality" testsEquality,
        testGroup "modulo" testsModulo,
        testGroup "typepred" testsTypePreds,
        testGroup "atoms" testsAtoms,
        testGroup "define" testsDefine,
        testGroup "lambda" testsLambda,
        testGroup "hofs" testsHofs,
        testGroup "cond" testsCond,
        testGroup "let" testsLet,
        testGroup "quote" testsQuoteEval,
        testGroup "eval" testsEval,
        testGroup "runtime" testsExtraRuntime,
        testGroup "prob" testsProb
      ],
    testGroup
      "Prob"
      [ testCase "compress 1" (compress (uniform ['A', 'B']) @?= "[('A',0.5),('B',0.5)]"),
        testCase "compress 2" (compress (uniform ['A', 'A', 'B', 'C']) @?= "[('A',0.5),('B',0.25),('C',0.25)]"),
        testCase "uniform 1" (uniform ['A', 'B'] @?= D [('A', 1 / 2), ('B', 1 / 2)]),
        testCase "uniform 2" (uniform [1, 2, 3] @?= D [(1, 1 / 3), (2, 1 / 3), (3, 1 / 3)]),
        testCase "enum 1" (enum [0.46, 0.54] [1, 2] @?= D [(1, 0.46), (2, 0.54)]),
        testCase "(??) 1" ((==) 'A' ?? uniform ['A', 'B'] @?= 1 / 2),
        testCase "(??) 2" ((==) 'A' ?? uniform ['A', 'A', 'B'] @?= 2 / 3),
        testCase
          "join 1"
          ( join (,) (uniform [1, 2]) (uniform [3, 4])
              @?= D [((1, 3), 0.25), ((1, 4), 0.25), ((2, 3), 0.25), ((2, 4), 0.25)]
          ),
        testCase
          "join 2"
          ( join (+) (uniform [1, 2]) (uniform [3, 4])
              @?= D [(4, 0.25), (5, 0.25), (5, 0.25), (6, 0.25)]
          ),
        testCase
          "prod"
          ( prod (uniform [1, 2]) (uniform [3, 4])
              @?= D [((1, 3), 0.25), ((1, 4), 0.25), ((2, 3), 0.25), ((2, 4), 0.25)]
          ),
        testGroup
          "Functor"
          [ testCase "fmap" (fmap (+ 1) (uniform [1 .. 3]) @?= D [(2, 1 / 3), (3, 1 / 3), (4, 1 / 3)])
          ],
        testGroup
          "Applicative"
          [ testCase "pure" ((pure 'A' :: Dist Char) @?= D [('A', 1)]),
            testCase
              "<*>"
              ( uniform [(+ 1), (+ 2)] <*> uniform [1, 2]
                  @?= D [(2, 0.25), (3, 0.25), (3, 0.25), (4, 0.25)]
              )
          ],
        testGroup
          "Monad"
          [ testCase
              "left identity"
              ( let a = 1
                    m = return a :: Dist Int
                    f = \x -> D [(x + 1, 1)]
                 in (m >>= f) @?= f a
              ),
            testCase
              "right identity"
              ( let m = uniform [1, 2]
                 in (m >>= return) @?= m
              ),
            testCase
              "associativity"
              ( let m = uniform [1, 2]
                    f = \x -> D [(x + 1, 1)]
                    g = \x -> D [(x - 1, 1)]
                 in ((m >>= f) >>= g) @?= (m >>= (\x -> f x >>= g))
              )
          ],
        testCase
          "(>@>)"
          ( let m = uniform [1, 2]
                f = \x -> D [(x + 1, 1)]
                g = \x -> D [(x - 1, 1)]
             in (m >>= (f >@> g)) @?= (m >>= f >>= g)
          ),
        testCase
          "sequ"
          ( let m = uniform [1, 2]
                f = \x -> D [(x + 1, 1)]
                g = \x -> D [(x - 1, 1)]
                h = \x -> D [(x * 2, 1)]
             in (m >>= sequ [f, g, h]) @?= (m >>= f >>= g >>= h)
          ),
        testCase "selectOne" (selectOne [1, 2] @?= D [((1, [2]), 0.5), ((2, [1]), 0.5)]),
        testCase "selectMany 1" (selectMany 0 [1, 2] @?= D [(([], [1, 2]), 1.0)]),
        testCase "selectMany 2" (selectMany 2 [1, 2] @?= D [(([1, 2], []), 0.5), (([2, 1], []), 0.5)]),
        testCase "select" (select 1 [1, 2] @?= D [([1], 0.5), ([2], 0.5)]),
        testCase "sampleOne" (sampleOne [1, 2] @?= D [((1, [1, 2]), 0.5), ((2, [1, 2]), 0.5)]),
        testCase "sampleMany 1" (sampleMany 0 [1, 2] @?= D [(([], [1, 2]), 1.0)]),
        testCase "sampleMany 2" (sampleMany 2 [1, 2] @?= D [(([1, 1], [1, 2]), 0.25), (([1, 2], [1, 2]), 0.25), (([2, 1], [1, 2]), 0.25), (([2, 2], [1, 2]), 0.25)]),
        testCase "sample" (sample 1 [1, 2] @?= D [([1], 0.5), ([2], 0.5)])
      ]
  ]

main :: IO ()
main = defaultMain tests
