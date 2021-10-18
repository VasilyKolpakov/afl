(define i32-max-value (- (bitwise-lshift 1 31) 1))
(define i32-min-value (- 0 (bitwise-lshift 1 31)))

(define println (lambda (obj)
    (begin
        (print obj)
        (print-string "\n"))))

(define map (lambda (f l)
    (if (empty? l)
      '()
      (cons (f (car l)) (map f (cdr l))))))

(define non-empty-list?
  (lambda (l)
    (if (list? l)
      (not (empty? l))
      #f)))

(define assert
  (lambda (v p)
    (if (p v)
      v
      (begin
        (print-string "assertion failed: ")
        (print p)
        (print-string " on ")
        (println v)
        (print-stack-trace)
        (exit 1)))))

(define alist-lookup-rec
  (lambda (l k)
    (if (empty? l)
      '()
      (if (equal? (car (car l)) k)
        (cdr (car l))
        (alist-lookup-rec (cdr l) k)))))

(define alist-lookup
  (lambda (l k) (alist-lookup-rec (assert l list?) k)))

(define fixed-point-with-limit
  (lambda (f l i)
    (if (< l 1)
        (begin
          (print-string "error: fix point reached max iterations\n")
          (print-stack-trace)
          (print-string "stacktrace end\n")
          (exit 1))
        (let ((fi (f i)))
          (if (equal? i fi)
              i
              (fixed-point-with-limit f (- l 1) fi))))))

(define macro-list (cell '()))

(define add-macro
  (lambda (sym macro-func)
    (cell-set macro-list (cons (cons sym macro-func) (cell-get macro-list)))))

(define apply-single-macro
  (lambda (expr)
    (let ((macro-func (alist-lookup (cell-get macro-list) (car expr))))
      (if (equal? macro-func '())
        expr
        (macro-func (cdr expr))))))

(define apply-all-macro
  (lambda (expr)
    (if (non-empty-list? expr)
      (let ((newexpr (map apply-all-macro expr)))
        (apply-single-macro newexpr))
      expr)))

(define macro-expand
  (lambda (expr)
    (fixed-point-with-limit apply-all-macro 10 expr)))

(define REPL-print-enabled (cell #f))

(define REPL-print-value
  (lambda (value)
    (if (cell-get REPL-print-enabled)
      (println value)
      '())))

(define enable-REPL-print
  (lambda () (cell-set REPL-print-enabled #t)))

(define REPL (lambda ()
    (begin
      (REPL-print-value (eval (macro-expand (read-syntax))))
      (REPL))))

(REPL)

(define make-list-maker
  (lambda (expr-args)
    (if (empty? expr-args)
        ''()
        (cons 'cons
              (cons (car expr-args)
                    (cons (make-list-maker (cdr expr-args))
                          '()))))))

(add-macro 'list make-list-maker)

(define extract-defun-lambda
  (lambda (expr-args)
    (let ((args (cdr (car expr-args)))
          (body (if (> (length expr-args) 2)
                    (cons 'begin (cdr expr-args))
                    (car (cdr expr-args)))))
      (list 'lambda args body))))

(add-macro 'define
           (lambda (expr-args)
             (if (list? (car expr-args))
               (let ((lam (extract-defun-lambda expr-args)))
                 (list 'define (car (car expr-args)) lam))
               (cons 'define expr-args))))


(define (assert-stmt message b)
  (if b
      '()
      (begin
        (print-string "assertion failed: ")
        (print-string message)
        (print-string "\n")
        (print-stack-trace)
        (exit 1))))

(define (cond-to-if clauses)
  (assert-stmt "else clause is present" (not (empty? clauses)))
  (let ((clause (car clauses)))
    (if (equal? 'else (car clause))
        (car (cdr clause))
        (let ((condd (car clause))
              (then-body (car (cdr clause))))
          (list 'if condd then-body (cond-to-if (cdr clauses)))))))

(add-macro 'cond cond-to-if)

(define (and-to-if exprs)
  (cond
    ((empty? exprs) #t)
    ((empty? (cdr exprs)) (car exprs))
    (else (list 'if (car exprs) (and-to-if (cdr exprs)) #f))))

(add-macro 'and and-to-if)

(define (add-begin-in-let exprs)
  (if (= (length exprs) 2)
      (cons 'let exprs)
      (list 'let (car exprs) (cons 'begin (cdr exprs)))))

(add-macro 'let add-begin-in-let)


(define (panic msg obj)
  (print-string "error: ")
  (print-string msg)
  (print-string " ")
  (print obj)
  (print-string "\n")
  (exit 1))

(define syscall-mmap 9)
(define syscall-mmap-PROT-READ 1)
(define syscall-mmap-PROT-WRITE 2)
(define syscall-mmap-PROT-EXEC 4)
(define syscall-mmap-PROT-NONE 0)

(define syscall-mmap-MAP-ANONYMOUS 32)
(define syscall-mmap-MAP-PRIVATE 2)

(define syscall-mmap-ABSENT-FD -1)

(define syscall-mmap-anon
  (lambda (size)
    (syscall syscall-mmap 
             0 ; address hint
             size
             (bitwise-ior syscall-mmap-PROT-READ syscall-mmap-PROT-WRITE)
             (bitwise-ior syscall-mmap-MAP-ANONYMOUS syscall-mmap-MAP-PRIVATE)
             syscall-mmap-ABSENT-FD
             0 ; offset
             )))

(define foldl
  (lambda (f z l)
    (if (empty? l)
      z
      (foldl f (f (car l) z) (cdr l)))))

(define reverse
  (lambda (l)
    (foldl cons '() l)))

(define (foldr f z l)
  (foldl f z (reverse l)))

(define (append l1 l2)
  (assert-stmt "l1 is list" (list? l1))
  (assert-stmt "l2 is list" (list? l2))
  (foldl cons l2 (reverse l1)))

(define (vararg-append-to-append exprs)
  (if (> (length exprs) 2)
      (list 'append (car exprs) (vararg-append-to-append (cdr exprs)))
      (cons 'append exprs)))

(add-macro 'append vararg-append-to-append)

(define (drop l n)
  (if (> n 0)
      (drop (cdr l) (- n 1))
      l))

(define (index-of-rec lst v acc)
  (if (equal? lst '())
    '()
    (if (equal? v (car lst))
      acc
      (index-of-rec (cdr lst) v (+ acc 1)))))

(define (index-of lst v)
  (index-of-rec lst v 0))

(define range-tailrec (lambda (n l)
    (if (> n 0)
      (range-tailrec (- n 1) (cons (- n 1) l))
      l)))

(define range (lambda (n) (range-tailrec n '())))

(define filter
  (lambda (f l)
    (if (equal? l '())
      '()
      (if (f (car l))
        (cons (car l) (filter f (cdr l)))
        (filter f (cdr l))))))

(define (foreach-recursive f l)
  (if (empty? l)
      '()
      (begin
        (f (car l))
        (foreach-recursive f (cdr l)))))

(define (foreach f l)
  (assert-stmt "l is list" (list? l))
  (foreach-recursive f l))

(define (zip-recursive l1 l2)
  (if (empty? l1)
      '()
      (cons 
        (cons (car l1) (car l2))
        (zip-recursive (cdr l1) (cdr l2)))))

(define (zip l1 l2)
  (assert-stmt "l1 is list" (list? l1))
  (assert-stmt "l2 is list" (list? l2))
  (assert-stmt "equal length" (equal? (length l1) (length l2)))
  (zip-recursive l1 l2))

(define (expr-has-unquote expr)
  (if (list? expr)
      (cond
        ((empty? expr) #f)
        ((equal? (car expr) 'quote) #f)
        ((equal? (car expr) 'unquote) #t)
        (else (not (empty? (filter id (map expr-has-unquote expr))))))
      #f))

(define (transform-quasiquote expr)
  (cond
    ((symbol? expr) (list 'quote expr))
    ((not (list? expr)) expr)
    ((empty? expr) '(quote ()))
    ((not (expr-has-unquote expr)) (if (equal? (car expr) 'quote)
                                       expr
                                       (list 'quote expr)))
    ((equal? (car expr) 'unquote) (car (cdr expr)))
    (else (cons 'list (map transform-quasiquote expr)))))

(define (transform-quasiquote-macro exprs)
  (assert-stmt "quote has single arg" (empty? (cdr exprs)))
  (transform-quasiquote (car exprs)))

(add-macro 'quote transform-quasiquote-macro)

(define (not-empty? v) (not (empty? v)))

(define (flatten xs) (foldr append '() xs))
(define (flatmap f xs) (flatten (map f xs)))
