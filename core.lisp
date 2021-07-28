; import lisp_interpreter.2s

(define println (lambda (obj)
    (begin
        (print obj)
        (print-string "\n"))))

(define map (lambda (f l)
    (if (equal? l nil)
      nil
      (cons (f (car l)) (map f (cdr l))))))

(define transform-let-expr (lambda (let-expr)
    (cons
        (cons (quote lambda)
              (list
                (map (lambda (binding) (car binding)) (car (cdr let-expr)))
                (car (cdr (cdr let-expr)))))
        (map (lambda (binding) (car (cdr binding))) (car (cdr let-expr))))))


(define transform-all-let-exprs (lambda (expr)
    (if (list? expr)
      (map transform-all-let-exprs
           (if (equal? (car expr) (quote let))
             (transform-let-expr expr)
             expr))
      expr)))

(define REPL-print-enabled (cell false))

(define REPL-print-value
  (lambda (value)
    (if (cell-get REPL-print-enabled)
      (println value)
      nil)))

(define enable-REPL-print
  (lambda () (cell-set REPL-print-enabled true)))

(define REPL (lambda ()
    (begin
      (REPL-print-value (eval (transform-all-let-exprs (read-syntax))))
      (REPL))))

(REPL)


(define syscall-mmap 9)
(define syscall-mmap-PROT-READ 1)
(define syscall-mmap-PROT-WRITE 2)
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

