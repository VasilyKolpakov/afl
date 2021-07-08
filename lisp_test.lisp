; import lisp_interpreter.2s

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
(if true 1 2)
(if false 1 2)

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

"list"
(list? 1)
(list? (list 1 2 3))

(exit 0)
