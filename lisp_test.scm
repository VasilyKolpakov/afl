(import "core.scm")

(enable-REPL-print)

"arithmetic"
(+ 1 2)
(* 2 2)
(- 3 1)
(+ (* 2 2) (* 1 1))
(/ 4 2)

"lambda"
(((lambda (x) (lambda (y) (+ x y))) 1) 2)
((lambda (f s) f) 1 2)

"if"
(if #t 1 2)
(if #f 1 2)

"factorial"
(define factorial (lambda (n)
    (if (> n 0)
        (* (factorial (- n 1)) n)
        1)))
(factorial 5)

"equal? numbers"
(equal? 1 1)
(equal? 1 0)

"equal? lists"
(equal? (list 1 2 3) (list 1 2 3))
(equal? (list 1 3) (list 1 2 3))

"quote"
(quote (lambda (x) (+ x x)))

"list operations"
(list? 1)
(list? (list 1 2 3))
(length '(1 2 3))
(length '())

"let expression"
(let ((x 1)
      (y 10))
  (+ x y))

"memory operations and syscalls"
(define buffer (syscall-mmap-anon 10))
(write-mem-i64 buffer 42)
(read-mem-i64 buffer)
(write-mem-byte buffer 100)
(read-mem-byte buffer)

"string operations"
(string-length "123456789")
(string-get-byte "a" 0)

"reverse"
(reverse (list 1 2 3 4))

"single quote"
'(+ 1 2)
'symbol

"if and let"
(if #t
    (let ((x "true")) x)
    (+ 2 2))
(if (equal? 1 2)
    (+ 2 2)
    (let ((x "false")) x))
(if (equal? 1 2)
    (+ 2 2)
    (let ((x "false")) x))

"no capture"
(let ((x 0))
  ((lambda () 0)))

"capture x"
(let ((x 0))
  ((lambda () x)))

"not capture x"
(let ((x 0))
  ((lambda (x) x) 1))

"capture y"
(let ((x 0) (y 'y))
  ((lambda (x) (cons x y)) 1))

"capture x y"
(let ((x 'x) (y 'y))
  ((lambda () (cons x y))))

"capture x y again"
(let ((x 2))
  (let ((y 13))
    ((lambda () (+ x y)))))

"lambda call"
(((lambda () (lambda () 42))))

"capture x in inner lambda"
(let ((x 'x))
  (((lambda () (lambda () x)))))

"another lambda call"
((lambda (x) (+ x 1)) 41)

"bound function"
(let ((add +))
  (add 1 2))

"multiple copies"
(let ((x 1))
  (((lambda () (lambda () (+ x (+ x x)))))))

"begin 3"
(begin 1 2 3)

"define a 0"
(define a 0)
a

"print"
(print 42)

"lambda 2 args"
((lambda (x y) (+ x y)) 1 2)

(read-syntax)
"read-syntax"
"1 < 2"
(< 1 2)

"cons (1)"
(cons 1 '())

"cell"
(define test-cell (cell '()))
(cell-get test-cell)
(cell-set test-cell 42)
(cell-get test-cell)

"complex lambda"
(((lambda (x) (lambda (y) (+ x y))) 1) 2)
(print-string "\nend\n")

"tco test"
(define tco-test
  (lambda (n)
    (if (= n 0)
        (print-stack-trace)
        (tco-test (- n 1)))))
(tco-test 20)

"begin"
(begin
  1 2 3 4 42)

"number?"
(number? 1)
(number? '())
(number? "1")

(exit 0)
