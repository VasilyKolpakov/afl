; import lisp_interpreter.2s

(define println (lambda (obj)
    (begin
        (print obj)
        (print-string "\n"))))

(define map (lambda (f l)
    (if (equal? l nil)
      nil
      (cons (f (car l)) (map f (cdr l))))))

(define non-empty-list?
  (lambda (l)
    (if (list? l)
      (not (equal? l nil))
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
    (if (equal? l nil)
      nil
      (if (equal? (car (car l)) k)
        (cdr (car l))
        (alist-lookup-rec (cdr l) k)))))

(define alist-lookup
  (lambda (l k) (alist-lookup-rec (assert l list?) k)))

(define transform-let-expr (lambda (let-expr-args)
    (cons
        (cons (quote lambda)
              (list
                (map (lambda (binding) (car binding)) (car let-expr-args))
                (car (cdr let-expr-args))))
        (map (lambda (binding) (car (cdr binding))) (car let-expr-args)))))


(define transform-all-let-exprs (lambda (expr)
    (if (list? expr)
      (map transform-all-let-exprs
           (if (equal? (car expr) 'let)
             (transform-let-expr (cdr expr))
             expr))
      expr)))

(eval (transform-all-let-exprs '(define fixed-point-with-limit
  (lambda (f l i)
    (if (< l 1)
      nil
      (let ((fi (f i)))
        (if (equal? i fi)
          i
          (fixed-point-with-limit f (- l 1) fi))))))))

(define macro-list (cell nil))

(define add-macro
  (lambda (sym macro-func)
    (cell-set macro-list (cons (cons sym macro-func) (cell-get macro-list)))))

(eval (transform-all-let-exprs '(define apply-single-macro
  (lambda (expr)
    (let ((macro-func (alist-lookup (cell-get macro-list) (car expr))))
      (if (equal? macro-func nil)
        expr
        (macro-func (cdr expr))))))))

(eval (transform-all-let-exprs '(define apply-all-macro
  (lambda (expr)
    (if (non-empty-list? expr)
      (let ((newexpr (map apply-all-macro expr)))
        (apply-single-macro newexpr))
      expr)))))

(define macro-expand
  (lambda (expr)
    (fixed-point-with-limit apply-all-macro 10 expr)))

(add-macro 'let transform-let-expr)

(define REPL-print-enabled (cell #f))

(define REPL-print-value
  (lambda (value)
    (if (cell-get REPL-print-enabled)
      (println value)
      nil)))

(define enable-REPL-print
  (lambda () (cell-set REPL-print-enabled #t)))

(define REPL (lambda ()
    (begin
      (REPL-print-value (eval (macro-expand (read-syntax))))
      (REPL))))

(REPL)

(define extract-defun-lambda
  (lambda (expr-args)
    (let ((args (cdr (car expr-args)))
          (body (car (cdr expr-args))))
      (list 'lambda args body))))

(add-macro 'defun
           (lambda (expr-args)
             (let ((lam (extract-defun-lambda expr-args)))
               (list 'define (car (car expr-args)) lam))))

(defun (transform-let-star bindings body)
  (if (empty? bindings)
    body
    (list
      'let
      (list (car bindings))
      (transform-let-star (cdr bindings) body))))

(defun (transform-let-star-expr let-args)
  (transform-let-star (car let-args) (car (cdr let-args))))

(add-macro 'let* transform-let-star-expr)


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
  (lambda (l f z)
    (if (equal? l nil)
      z
      (foldl (cdr l) f (f (car l) z)))))

(define reverse
  (lambda (l)
    (foldl l cons nil)))
