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
  (list 11 (lambda (ptr) (generate-push-imm ptr imm-value)) (list "push-imm" imm-value)))

(define (generate-return ptr)
  (write-mem-byte ptr #x5d) ; pop rbp
  (write-mem-byte (+ 1 ptr) #xc3) ; ret
  )

(define return-instruction (list 2 generate-return))

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

(define (generate-add-rsp ptr value)
  (write-mem-byte ptr       #x48) ; add rsp, value (signed byte)
  (write-mem-byte (+ 1 ptr) #x83)
  (write-mem-byte (+ 2 ptr) #xc4)
  (write-mem-byte (+ 3 ptr) value)
  )

(define (add-rsp-instruction value)
  (assert-stmt "value <= 127" (<= value 127))
  (assert-stmt "value >= 128" (>= value -128))
  (list 4 (lambda (ptr) (generate-add-rsp ptr value)) "add-rsp"))
; 48 8b 45 ff

(define (generate-push-var ptr index)
  (write-mem-byte ptr       #x48) ; rax,QWORD PTR [rbp-index]
  (write-mem-byte (+ 1 ptr) #x8b)
  (write-mem-byte (+ 2 ptr) #x45)
  (write-mem-byte (+ 3 ptr) (- 0 (* (+ 1 index) 8)))
  (write-mem-byte (+ 4 ptr) #x50) ; push rax
  )

(define (push-var-instruction index)
  (list 5 (lambda (ptr) (generate-push-var ptr index)) (list "push-var" index)))

;  63:	55                   	push   rbp
;  64:	48 89 e5             	mov    rbp,rsp

(define (generate-set-frame-pointer ptr)
  (write-mem-byte ptr       #x55) ; push rbp
  (write-mem-byte (+ 1 ptr) #x48) ; mov rbp,rsp
  (write-mem-byte (+ 2 ptr) #x89)
  (write-mem-byte (+ 3 ptr) #xe5)
  )

(define set-frame-pointer-instruction
  (list 4 generate-set-frame-pointer "set-frame-pointer"))


(define (prefix-sum l)
  (reverse (cdr (foldl
                  (lambda (i acc) (cons (+ i (car acc)) acc))
                  (list 0)
                  l))))

(define symbol-to-instruction
  (list
    (list '+ 2 add-instruction)))

(define (compile-list expr rest local-vars)
  (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty?)))
    (assert-stmt "arg count matches" (= (length (cdr expr)) (car arg-count-and-inst)))
    (foldr
      (lambda (expr rest) (compile-expr-rec expr rest local-vars))
      (cons (car (cdr arg-count-and-inst)) rest)
      (cdr expr))))

(define (compile-expr-rec expr rest local-vars)
  (let ((var-index (index-of local-vars expr)))
    (cond
      ((not-empty? var-index) (cons (push-var-instruction var-index) rest))
      ((number? expr) (cons (push-imm-instruction expr) rest))
      ((list? expr) (compile-list expr rest local-vars))
      (else (panic "bad expr: " expr)))))

(define (compile-expr expr local-vars)
  (compile-expr-rec expr '() local-vars))

(define (compile-statement stmt local-vars)
  (let ((stmt-type (car stmt)))
    (cond ((equal? stmt-type 'set-64)
           (append
             (compile-expr (car (cdr stmt)) local-vars)
             (compile-expr (car (cdr (cdr stmt))) local-vars)
             (list set-64-instruction)))
          (else (panic "bad statement" stmt)))))


(define fpointer (syscall-mmap-anon-exec 1000))
(define buffer (syscall-mmap-anon 1000))

(define (compile-procedure local-vars-with-inits statements)
  (let ((local-var-inits (map (lambda (v) (car (cdr v))) local-vars-with-inits))
        (local-vars (map car local-vars-with-inits)))
    (append
      (list set-frame-pointer-instruction)
      (flatmap (lambda (var-init) (compile-expr var-init '()))
               local-var-inits)
      (flatmap (lambda (stmt) (compile-statement stmt local-vars))
               statements)
      (list
        (add-rsp-instruction (* 8 (length local-vars)))
        return-instruction))))

(define instructions
  (compile-procedure
    '((a 1)
      (b 2)
      (c 3))
    '((set-64 ,buffer (+ a (+ b c)))
      (set-64 ,(+ 8 buffer) (+ 20 (+ a (+ b c)))))))

(foreach println instructions)

(let ((inst-locations (prefix-sum (map (lambda (x) (car x)) instructions)))
      (locations-and-insts (zip inst-locations instructions)))
  (foreach (lambda (loc-and-i) 
             (let ((loc (car loc-and-i))
                   (inst (cdr loc-and-i))
                   (generator (car (cdr inst))))
               (generator (+ fpointer loc)))) locations-and-insts))

(enable-REPL-print)
(native-call fpointer)
(read-mem-i64 buffer)
(read-mem-i64 (+ 8 buffer))
