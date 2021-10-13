(import "core.scm")

(define (syscall-mmap-anon-exec size)
  (syscall syscall-mmap 
           0 ; address hint
           size
           (bitwise-ior syscall-mmap-PROT-EXEC syscall-mmap-PROT-WRITE)
           (bitwise-ior syscall-mmap-MAP-ANONYMOUS syscall-mmap-MAP-PRIVATE)
           syscall-mmap-ABSENT-FD
           0 ; offset
           ))

(define (generate-push-imm ptr imm-value)
  (write-mem-byte ptr #x48) ; mov rax, imm-value
  (write-mem-byte (+ 1 ptr) #xb8)
  (write-mem-i64  (+ 2 ptr) imm-value)
  (write-mem-byte (+ 10 ptr) #x50) ; push rax
  )

(define (push-imm-instruction imm-value)
  (list 11 (lambda (ptr) (generate-push-imm ptr imm-value)) "push-imm"))

(define (generate-drop ptr)
  (write-mem-byte ptr #x59)) ; pop rcx

(define drop-instruction (list 1 generate-drop))

(define (generate-pop-rax ptr)
  (write-mem-byte ptr #x58)) ; pop rax

(define pop-rax-instruction (list 1 generate-pop-rax))

(define (generate-return ptr)
  (write-mem-byte ptr #xc3)) ; ret

(define return-instruction (list 1 generate-return))

(define (generate-add ptr)
  (write-mem-byte ptr #x58) ; pop rax
  (write-mem-byte (+ 1 ptr) #x59) ; pop rcx
  (write-mem-byte (+ 2 ptr) #x48) ; add rax, rcx
  (write-mem-byte (+ 3 ptr) #x01)
  (write-mem-byte (+ 4 ptr) #xc8)
  (write-mem-byte (+ 5 ptr) #x50) ; push rax
  )

(define add-instruction (list 6 generate-add "add"))

(define (generate-set-64 ptr)
  (write-mem-byte ptr #x5f) ; pop rdi
  (write-mem-byte (+ 1 ptr) #x58) ; pop rax
  (write-mem-byte (+ 2 ptr) #x48) ; mov [rax], rdi
  (write-mem-byte (+ 3 ptr) #x89)
  (write-mem-byte (+ 4 ptr) #x38)
  )

(define set-64-instruction (list 5 generate-set-64 "set-64"))


(define (prefix-sum l)
  (reverse (cdr (foldl
                  (lambda (i acc) (cons (+ i (car acc)) acc))
                  (list 0)
                  l))))

(define symbol-to-instruction
  (list
    (list '+ 2 add-instruction)))

(define (compile-list expr rest)
  (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty?)))
    (assert-stmt "arg count matches" (= (length (cdr expr)) (car arg-count-and-inst)))
    (foldr
      compile-expr-rec
      (cons (car (cdr arg-count-and-inst)) rest)
      (cdr expr))))

(define (compile-expr-rec expr rest)
  (cond
    ((number? expr) (cons (push-imm-instruction expr) rest))
    ((list? expr) (compile-list expr rest))
    (else (panic "bad expr: " expr))))

(define (compile-expr expr)
  (compile-expr-rec expr '()))

(define (compile-statement stmt)
  (let ((stmt-type (car stmt)))
    (cond ((equal? stmt-type 'set-64)
           (append
             (compile-expr (car (cdr stmt)))
             (compile-expr (car (cdr (cdr stmt))))
             (list set-64-instruction)))
          (else (panic "bad statement" stmt)))))

(enable-REPL-print)

(define fpointer (syscall-mmap-anon-exec 1000))
(define buffer (syscall-mmap-anon 1000))

(define instructions
  (append
    (compile-statement (list 'set-64 buffer '(+ 40 2)))
    (compile-expr '(+ (+ 1 2) 3))
    (list
      pop-rax-instruction
      return-instruction)))

(foreach println instructions)

(let ((inst-locations (prefix-sum (map (lambda (x) (car x)) instructions)))
      (locations-and-insts (zip inst-locations instructions)))
  (foreach (lambda (loc-and-i) 
             (let ((loc (car loc-and-i))
                   (inst (cdr loc-and-i))
                   (generator (car (cdr inst))))
               (generator (+ fpointer loc)))) locations-and-insts))

(native-call fpointer)
(read-mem-i64 buffer)
