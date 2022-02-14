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

(define return-instruction (list 2 generate-return "ret"))

(define (generate-add ptr)
  (write-mem-byte ptr #x58) ; pop rax
  (write-mem-byte (+ 1 ptr) #x59) ; pop rcx
  (write-mem-byte (+ 2 ptr) #x48) ; add rax, rcx
  (write-mem-byte (+ 3 ptr) #x01)
  (write-mem-byte (+ 4 ptr) #xc8)
  (write-mem-byte (+ 5 ptr) #x50) ; push rax
  )

(define add-instruction (list 6 generate-add "add"))

(define sub-instruction
  (list 6
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; sub rax,rdi
            (write-mem-byte (+ 3 ptr) #x29)
            (write-mem-byte (+ 4 ptr) #xf8)
            (write-mem-byte (+ 5 ptr) #x50) ; push rax
            ))
        "sub"))

(define mul-instruction
  (list 7
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; imul rax,rdi
            (write-mem-byte (+ 3 ptr) #x0f)
            (write-mem-byte (+ 4 ptr) #xaf)
            (write-mem-byte (+ 5 ptr) #xc7)
            (write-mem-byte (+ 6 ptr) #x50) ; push rax
            ))
        "mul"))

(define div-instruction
  (list 8
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; cqo ; mov sign-extend of rax to rdx
            (write-mem-byte (+ 3 ptr) #x99)
            (write-mem-byte (+ 4 ptr) #x48) ; idiv rdi ; divide rdx:rax by rdi
            (write-mem-byte (+ 5 ptr) #xf7)
            (write-mem-byte (+ 6 ptr) #xff)
            (write-mem-byte (+ 7 ptr) #x50) ; push rax
            ))
        "div"))

(define mod-instruction
  (list 8
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; cqo ; mov sign-extend of rax to rdx
            (write-mem-byte (+ 3 ptr) #x99)
            (write-mem-byte (+ 4 ptr) #x48) ; idiv rdi ; divide rdx:rax by rdi
            (write-mem-byte (+ 5 ptr) #xf7)
            (write-mem-byte (+ 6 ptr) #xff)
            (write-mem-byte (+ 7 ptr) #x52) ; push rdx
            ))
        "mod"))

(define bit-rshift-instruction
  (list 6
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x59) ; pop rcx
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; shr rax, cl
            (write-mem-byte (+ 3 ptr) #xd3)
            (write-mem-byte (+ 4 ptr) #xe8)
            (write-mem-byte (+ 5 ptr) #x50) ; push rax
            ))
        "bit-rshift"))

(define bit-lshift-instruction
  (list 6
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x59) ; pop rcx
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; shl rax, cl
            (write-mem-byte (+ 3 ptr) #xd3)
            (write-mem-byte (+ 4 ptr) #xe0)
            (write-mem-byte (+ 5 ptr) #x50) ; push rax
            ))
        "bit-lshift"))

(define bit-and-instruction
  (list 6
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x59) ; pop rcx
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; and rax, rcx
            (write-mem-byte (+ 3 ptr) #x21)
            (write-mem-byte (+ 4 ptr) #xc8)
            (write-mem-byte (+ 5 ptr) #x50) ; push rax
            ))
        "bit-and"))

(define bit-or-instruction
  (list 6
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x59) ; pop rcx
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; or rax, rcx
            (write-mem-byte (+ 3 ptr) #x09)
            (write-mem-byte (+ 4 ptr) #xc8)
            (write-mem-byte (+ 5 ptr) #x50) ; push rax
            ))
        "bit-or"))

(define set-i64-instruction
  (list 5 
        (lambda (ptr)
          (begin
            (write-mem-byte ptr #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x48) ; mov [rax], rdi
            (write-mem-byte (+ 3 ptr) #x89)
            (write-mem-byte (+ 4 ptr) #x38)
            ))
        "set-i64"))

(define get-i64-instruction
  (list 5
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x48) ; mov rax,QWORD PTR [rdi]
            (write-mem-byte (+ 2 ptr) #x8b)
            (write-mem-byte (+ 3 ptr) #x07)
            (write-mem-byte (+ 4 ptr) #x50) ; push rax
            ))
        "get-i64"))

(define set-u8-instruction
  (list 5
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x58) ; pop rax
            (write-mem-byte (+ 2 ptr) #x40) ; mov [rax], dil
            (write-mem-byte (+ 3 ptr) #x88)
            (write-mem-byte (+ 4 ptr) #x38)
            ))
        "set-u8"))

(define get-u8-instruction
  (list 8
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x58) ; pop rax
            (write-mem-byte (+ 1 ptr) #x48) ; xor rdi,rdi
            (write-mem-byte (+ 2 ptr) #x31)
            (write-mem-byte (+ 3 ptr) #xff)
            (write-mem-byte (+ 4 ptr) #x40) ; mov dil, [rax]
            (write-mem-byte (+ 5 ptr) #x8a)
            (write-mem-byte (+ 6 ptr) #x38)
            (write-mem-byte (+ 7 ptr) #x57) ; push rdi
            ))
        "get-u8"))

(define (generate-add-rsp ptr value)
  (write-mem-byte ptr       #x48) ; add rsp, value (signed byte)
  (write-mem-byte (+ 1 ptr) #x83)
  (write-mem-byte (+ 2 ptr) #xc4)
  (write-mem-byte (+ 3 ptr) value)
  )

(define (add-rsp-instruction value)
  (assert-stmt "value <= 127" (<= value 127))
  (assert-stmt "value >= 128" (>= value -128))
  (list 4 (lambda (ptr) (generate-add-rsp ptr value)) (list "add-rsp" value)))

(define (generate-push-var-addr ptr index)
  (assert-stmt "index >= 0" (>= index 0))
  (assert-stmt "index < 15" (< index 15))
  (write-mem-byte ptr       #x48) ; lea rax,[rbp-index*8]
  (write-mem-byte (+ 1 ptr) #x8d)
  (write-mem-byte (+ 2 ptr) #x45)
  (write-mem-byte (+ 3 ptr) (- 0 (* (+ 1 index) 8)))
  (write-mem-byte (+ 4 ptr) #x50) ; push rax
  )

(define (push-var-addr-instruction index)
  (list 5 (lambda (ptr) (generate-push-var-addr ptr index)) (list "push-var-addr" index)))

(define (generate-push-var ptr index)
  (assert-stmt "index >= 0" (>= index 0))
  (assert-stmt "index < 15" (< index 15))
  (write-mem-byte ptr       #x48) ; rax,QWORD PTR [rbp-index*8]
  (write-mem-byte (+ 1 ptr) #x8b)
  (write-mem-byte (+ 2 ptr) #x45)
  (write-mem-byte (+ 3 ptr) (- 0 (* (+ 1 index) 8)))
  (write-mem-byte (+ 4 ptr) #x50) ; push rax
  )

(define (push-var-instruction index)
  (list 5 (lambda (ptr) (generate-push-var ptr index)) (list "push-var" index)))

(define (generate-set-var ptr index)
  (write-mem-byte ptr       #x58) ; pop rax
  (write-mem-byte (+ 1 ptr) #x48) ; QWORD PTR [rbp+___],rax
  (write-mem-byte (+ 2 ptr) #x89)
  (write-mem-byte (+ 3 ptr) #x45)
  (write-mem-byte (+ 4 ptr) (- 0 (* (+ 1 index) 8)))
  )

(define (set-var-instruction index)
  (list 5 (lambda (ptr) (generate-set-var ptr index)) (list "set-var" index)))

(define set-frame-pointer-instruction
  (list 4 (lambda (ptr)
            (begin
              (write-mem-byte ptr       #x55) ; push rbp
              (write-mem-byte (+ 1 ptr) #x48) ; mov rbp,rsp
              (write-mem-byte (+ 2 ptr) #x89)
              (write-mem-byte (+ 3 ptr) #xe5)))
        "set-frame-pointer"))

(define zero-frame-pointer-instruction
  (list 3 (lambda (ptr)
            (begin
              (write-mem-byte ptr       #x48) ; xor rbp,rbp
              (write-mem-byte (+ 1 ptr) #x31)
              (write-mem-byte (+ 2 ptr) #xed)))
        "zero-frame-pointer"))

(define push-frame-pointer-instruction
  (list 1 (lambda (ptr)
            (begin
              (write-mem-byte ptr       #x55) ; push rbp
              ))
        "push-frame-pointer"))

(define (generate-jmp ptr target)
  (let ((rel-target (- target (+ ptr 5))))
    (assert-stmt "rel-target <= i32-max-value" (<= rel-target i32-max-value))
    (assert-stmt "rel-target >= i32-min-value" (>= rel-target i32-min-value))
  (write-mem-byte ptr #xe9) ; jmp rel-target
  (write-mem-i32 (+ ptr 1) rel-target)
  ))

(define (jmp-instruction target-label)
  (list 5
        (lambda (ptr label-locs)
          (generate-jmp ptr (assert (alist-lookup label-locs target-label) not-empty? "jmp target")))
        (list "jmp" target-label)))

(define (generate-call ptr target)
  (let ((rel-target (- target (+ ptr 5))))
    (assert-stmt "rel-target <= i32-max-value" (<= rel-target i32-max-value))
    (assert-stmt "rel-target >= i32-min-value" (>= rel-target i32-min-value))
  (write-mem-byte ptr #xe8) ; call rel-target
  (write-mem-i32 (+ ptr 1) rel-target)
  ))

(define (generate-syscall ptr)
  (write-mem-byte ptr        #x41) ; pop r9
  (write-mem-byte (+ 1 ptr)  #x59)
  (write-mem-byte (+ 2 ptr)  #x41) ; pop r8
  (write-mem-byte (+ 3 ptr)  #x58)
  (write-mem-byte (+ 4 ptr)  #x41) ; pop r10
  (write-mem-byte (+ 5 ptr)  #x5a)
  (write-mem-byte (+ 6 ptr)  #x5a) ; pop rdx
  (write-mem-byte (+ 7 ptr)  #x5e) ; pop rsi
  (write-mem-byte (+ 8 ptr)  #x5f) ; pop rdi
  (write-mem-byte (+ 9 ptr)  #x58) ; pop rax
  (write-mem-byte (+ 10 ptr) #x41) ; pop r15
  (write-mem-byte (+ 11 ptr) #x5f)
  (write-mem-byte (+ 12 ptr) #x0f) ; syscall
  (write-mem-byte (+ 13 ptr) #x05)
  (write-mem-byte (+ 14 ptr) #x49) ; mov [r15], rax
  (write-mem-byte (+ 15 ptr) #x89)
  (write-mem-byte (+ 16 ptr) #x07)
  )

(define syscall-instruction (list 17 generate-syscall "syscall"))

(define (call-instruction target-label)
  (list 5
        (lambda (ptr label-locs)
          (generate-call ptr (assert (alist-lookup label-locs target-label) not-empty? "call target")))
        (list "call" target-label)))

(define (generate-cond-jmp code ptr target)
  (let ((rel-target (- target (+ ptr 11))))
    (assert-stmt "rel-target <= i32-max-value" (<= rel-target i32-max-value))
    (assert-stmt "rel-target >= i32-min-value" (>= rel-target i32-min-value))
  (write-mem-byte ptr       #x5f) ; pop rdi
  (write-mem-byte (+ 1 ptr) #x58) ; pop rax
  (write-mem-byte (+ 2 ptr) #x48) ; cmp rax,rdi
  (write-mem-byte (+ 3 ptr) #x39)
  (write-mem-byte (+ 4 ptr) #xf8)
  (write-mem-byte (+ 5 ptr) #x0f) ; je rel-target
  (write-mem-byte (+ 6 ptr) code)
  (write-mem-i32  (+ 7 ptr) rel-target)
  ))

(define (cond-jmp-instruction cond-code target-label)
  (list 11
        (lambda (ptr label-locs)
          (generate-cond-jmp cond-code ptr (assert (alist-lookup label-locs target-label) not-empty? "cond-jmp target")))
        (list "cond-jmp" cond-code target-label)))

(define (cond-jmp-instruction-gen cond-code)
  (lambda (label)
    (cond-jmp-instruction cond-code label)))

; '(1 2 3) -> '(0 1 3)
(define (prefix-sum z l)
  (reverse (cdr (foldl
                  (lambda (i acc) (cons (+ i (car acc)) acc))
                  (list z)
                  l))))

(define symbol-to-instruction
  (list
    (list '+          2 add-instruction)
    (list '-          2 sub-instruction)
    (list '*          2 mul-instruction)
    (list '/          2 div-instruction)
    (list '%          2 mod-instruction)
    (list 'bit-rshift 2 bit-rshift-instruction)
    (list 'bit-lshift 2 bit-lshift-instruction)
    (list 'bit-and    2 bit-and-instruction)
    (list 'bit-or     2 bit-or-instruction)
    (list 'i64@       1 get-i64-instruction)
    (list 'u8@        1 get-u8-instruction)
    (list 'get-fp     0 push-frame-pointer-instruction)
    ))

(define symbol-to-cmp-instructions
  (list
    (list '=
          (cond-jmp-instruction-gen #x84)
          (cond-jmp-instruction-gen #x85))
    (list '!=
          (cond-jmp-instruction-gen #x85)
          (cond-jmp-instruction-gen #x84))

    (list '<= 
          (cond-jmp-instruction-gen #x8e)
          (cond-jmp-instruction-gen #x8f))
    (list '>
          (cond-jmp-instruction-gen #x8f)
          (cond-jmp-instruction-gen #x8e))

    (list '>= 
          (cond-jmp-instruction-gen #x8d)
          (cond-jmp-instruction-gen #x8c))
    (list '<
          (cond-jmp-instruction-gen #x8c)
          (cond-jmp-instruction-gen #x8d))
    ))

(define (compile-list expr rest local-vars)
  (cond
    ((or (equal? '-> (car expr)) (equal? 'var-addr (car expr)))
     (let ((var (car (cdr expr)))
           (var-index (index-of local-vars var)))
       (if (empty? var-index) (panic "ref: bad variable:" var) '())
       (list (push-var-addr-instruction var-index))))
    (else
      (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty? (list "undefined operator:" (car expr)))))
        (assert-stmt "arg count matches" (= (length (cdr expr)) (car arg-count-and-inst)))
        (foldr
          (lambda (expr rest) (compile-expr-rec expr rest local-vars))
          (cons (car (cdr arg-count-and-inst)) rest)
          (cdr expr))))))

(define (compile-expr-rec expr rest local-vars)
  (let ((var-index (index-of local-vars expr)))
    (cond
      ((not-empty? var-index) (cons (push-var-instruction var-index) rest))
      ((number? expr) (cons (push-imm-instruction expr) rest))
      ((list? expr) (compile-list expr rest local-vars))
      (else (panic "bad expression: " (list "expression:" expr "local-vars:" local-vars))))))

(define (compile-expr expr local-vars)
  (compile-expr-rec expr '() local-vars))

(define (compile-boolean-expression cond-expr local-vars label true-label?)
  (let ((cond-type (car cond-expr))
        (cond-args (cdr cond-expr)))
    (cond ((equal? cond-type 'not)
           (compile-boolean-expression (second cond-expr) local-vars label (not true-label?)))
          ((equal? cond-type 'and)
           (let ((first-child (first cond-args))
                 (second-child (second cond-args)))
             (assert-stmt "and expression has 2 children" (= (length cond-args) 2))
             (if true-label?
               (let ((false-label (new-label)))
                 (append
                   (compile-boolean-expression first-child local-vars false-label #f)
                   (compile-boolean-expression second-child local-vars label #t)
                   (list false-label)))
               (append
                   (compile-boolean-expression first-child local-vars label #f)
                   (compile-boolean-expression second-child local-vars label #f)))))

          (else 
            (let ((true-and-false-instrs
                    (assert
                      (alist-lookup symbol-to-cmp-instructions (car cond-expr))
                      not-empty?
                      (list "bool expr" cond-expr)))
                  (cond-inst-gen
                    (if true-label?
                      (first true-and-false-instrs)
                      (second true-and-false-instrs))))
              (assert-stmt "cond expr: cmp functions should have 2 args" (= (length cond-args) 2))
              (foldr
                (lambda (expr rest) (compile-expr-rec expr rest local-vars))
                (list (cond-inst-gen label))
                cond-args))))))

(define (compile-statement stmt local-vars procedure-list)
  (let ((stmt-type (car stmt))
        (stmt-args (cdr stmt)))
    (cond ((equal? stmt-type 'i64:=)
           (append
             (compile-expr (first stmt-args) local-vars)
             (compile-expr (second stmt-args) local-vars)
             (list set-i64-instruction)))
          ((equal? stmt-type 'u8:=)
           (append
             (compile-expr (first stmt-args) local-vars)
             (compile-expr (second stmt-args) local-vars)
             (list set-u8-instruction)))
          ((equal? stmt-type ':+=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (compile-statement '(:= ,var (+ ,var ,val-expr)) local-vars procedure-list)))
          ((equal? stmt-type ':=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (if (empty? var-index) (panic "set-var: bad variable:" var) '())
             (compile-expr-rec
               val-expr
               (list (set-var-instruction var-index))
               local-vars)))
          ((equal? stmt-type 'call)
           (let ((proc (assert (alist-lookup procedure-list (car (cdr stmt))) not-empty? (list "undefined function:" (first stmt-args))))
                 (proc-args (cdr (cdr stmt)))
                 (proc-label (car proc))
                 (proc-arg-count (car (cdr proc))))
             (assert-stmt (list (first stmt-args) "number of args") (= proc-arg-count (length proc-args)))
             (append
               (list (add-rsp-instruction -16))
               (flatmap (lambda (expr) (compile-expr expr local-vars)) proc-args)
               (list
                 (add-rsp-instruction (* 8 (+ 2 proc-arg-count)))
                 (call-instruction proc-label)))))
          ((equal? stmt-type 'syscall)
           (begin
             (assert-stmt "syscall has 8 args" (= 8 (length stmt-args)))
             (append
               (flatmap (lambda (expr) (compile-expr expr local-vars)) stmt-args)
               (list syscall-instruction))))
          ((equal? stmt-type 'zero-fp)
           (begin
             (assert-stmt "zero-fp has 0 args" (= 0 (length stmt-args)))
             (list zero-frame-pointer-instruction)))
          ((equal? stmt-type 'return)
           (begin
             (assert-stmt "return has 0 args" (= 0 (length stmt-args)))
             (list
               (add-rsp-instruction (* 8 (length local-vars)))
               return-instruction)))
          ((equal? stmt-type 'while)
           (begin
             (assert-stmt "while has 2 args" (= 2 (length stmt-args)))
             (let ((cond-expr (car stmt-args))
                   (body-statements (second stmt-args))
                   (compile-substatement (lambda (s) (compile-statement s local-vars procedure-list)))
                   (loop-label (new-label))
                   (end-label (new-label))
                   (cond-instructions (compile-boolean-expression cond-expr local-vars end-label #f)))
               (append
                 (list loop-label)
                 cond-instructions
                 (flatmap compile-substatement body-statements)
                 (list (jmp-instruction loop-label)
                       end-label)
                 ))))
          ((and (equal? stmt-type 'if) (equal? 2 (length stmt-args)))
           (let ((cond-expr (car stmt-args))
                 (then-branch (second stmt-args))
                 (compile-substatement (lambda (s) (compile-statement s local-vars procedure-list)))
                 (else-label (new-label))
                 (cond-instructions (compile-boolean-expression cond-expr local-vars else-label #f)))
             (append
               cond-instructions
               (flatmap compile-substatement then-branch)
               (list else-label)
               )))
          ((equal? stmt-type 'if)
           (let ((cond-expr (car stmt-args))
                 (then-branch (second stmt-args))
                 (else-branch (nth 2 stmt-args))
                 (compile-substatement (lambda (s) (compile-statement s local-vars procedure-list)))
                 (else-label (new-label))
                 (end-label (new-label))
                 (cond-instructions (compile-boolean-expression cond-expr local-vars else-label #f)))
             (append
               cond-instructions
               (flatmap compile-substatement then-branch)
               (list (jmp-instruction end-label)
                     else-label)
               (flatmap compile-substatement else-branch)
               (list end-label)
               )))
          (else (panic "bad statement" stmt)))))


(define (compile-procedure args
                           local-vars-with-inits
                           statements
                           procedure-list)
  (begin
    (assert-stmt (list "local vars must have init values, for example: (proc foo (a b) ((i 42)) ..., but was " local-vars-with-inits)
                 (andmap pair? local-vars-with-inits))
    (let ((local-var-inits (map (lambda (v) (car (cdr v))) local-vars-with-inits))
          (local-vars (append args (map car local-vars-with-inits))))
      (append
        (list
          set-frame-pointer-instruction
          (add-rsp-instruction (- 0 (* 8 (length args)))))
        (flatmap (lambda (var-init) (compile-expr var-init '()))
                 local-var-inits)
        (flatmap (lambda (stmt) (compile-statement stmt local-vars procedure-list))
                 statements)
        (list (add-rsp-instruction (* 8 (length local-vars)))
          return-instruction)))))

(define next-label-id (cell 0))

(define (new-label)
  (let ((id (cell-get next-label-id)))
    (cell-set next-label-id (+ id 1))
    id))

(define (label? v) (number? v))

(define (compile-upl procedures)
  (let ((procs-with-labels
          (map (lambda (p) (list p (new-label))) procedures))
        (procedure-list
          (map (lambda (p-with-l)
                 (let ((name (second (first p-with-l)))
                       (label (second p-with-l))
                       (arg-count (length (nth 2 (first p-with-l)))))
                   (list name label arg-count)))
               procs-with-labels)))
    (list
      procedure-list
      (flatmap
        (lambda (p-with-l)
          (let ((proc (first p-with-l))
                (args (nth 2 proc))
                (local-vars-with-inits (nth 3 proc))
                (statements (nth 4 proc))
                (label (second p-with-l)))
            (cons label (compile-procedure args local-vars-with-inits statements procedure-list))))
        procs-with-labels))))


(define (instruction? i)
  (and
    (list? i)
    (>= (length i) 2)
    (number? (car i))
    (callable? (car (cdr i)))))

(define (validate-stack-machine-code code)
  (foreach (lambda (i)
             (if (or (label? i) (instruction? i))
               '()
               (panic "bad instruction:" i)))
           code))

(define (instruction-size i)
  (cond
    ((label? i) 0)
    ((instruction? i) (car i))
    (else (panic "bad instruction:" i))))

(define (print-instructions instructions)
  (foreach
    println
    (map
      (lambda (i)
        (if (label? i)
          (list 'label i)
          (drop i 2)))
      instructions)))


(define (compile-upl-to-native upl-code)
  (let ((proc-list-and-insts (compile-upl upl-code))
        (proc-list (first proc-list-and-insts))
        (instructions (second proc-list-and-insts))
        (_ (validate-stack-machine-code instructions))
        (inst-sizes (map instruction-size instructions))
        (total-code-size (foldl + 0 inst-sizes))
        (exec-buffer (syscall-mmap-anon-exec total-code-size))
        (inst-locations (prefix-sum exec-buffer inst-sizes))
        (with-locations (zip instructions inst-locations))
        (insts-and-locations (filter (lambda (x) (not (label? (car x)))) with-locations))
        (labels-and-locations (filter (lambda (x) (label? (car x))) with-locations)))
    (foreach (lambda (i-and-loc)
               (let ((inst (car i-and-loc))
                     (loc (cdr i-and-loc))
                     (generator (car (cdr inst))))
                 (if (= (arity generator) 2)
                   (generator loc labels-and-locations)
                   (generator loc))))
             insts-and-locations)
    (append
      (map (lambda (proc) (cons
                            (first proc)
                            (alist-lookup labels-and-locations (second proc))))
           proc-list)
      (list (cons '__end_of_range__not_a_function (+ exec-buffer total-code-size))))))

(define upl-print-static-string
  (let ((ret-val-buffer (syscall-mmap-anon 100))
        (str-buffer-size 10000)
        (str-buffer (syscall-mmap-anon str-buffer-size))
        (bump-index (cell 0)))
    (lambda (str)
      (let ((str-length (string-length str))
            (original-bump-index (cell-get bump-index)))
        (begin
          (assert-stmt "bump-index + string length <= str-buffer-size"
                       (<= (+ (cell-get bump-index) str-length) str-buffer-size))
          (string-to-native-buffer str (+ str-buffer (cell-get bump-index)))
          (cell-set bump-index (+ str-length (cell-get bump-index)))
          '(syscall ,ret-val-buffer 1 1 ,(+ str-buffer original-bump-index) ,str-length 1 2 3))))))

(define dummy-buf (syscall-mmap-anon 1000))
(define (upl-exit code)
  '(syscall ,dummy-buf 60 ,code 1 2 3 4 5))

(define alloc-arena-size 409600)
(define alloc-arena (syscall-mmap-anon alloc-arena-size))
(define alloc-bump-ptr (syscall-mmap-anon 1000))
(write-mem-i64 alloc-bump-ptr alloc-arena)

(define tmp-4k-buffer (syscall-mmap-anon 4096))
(define proc-dict-ptr (syscall-mmap-anon 4096))

(define upl-code 
  '(
    (proc chk-syscall (retcode-out call-code arg1 arg2 arg3 arg4 arg5 arg6) ((ret 0) (hack-8-byte-buf 0) (num-length 0))
          ((syscall (var-addr ret) call-code arg1 arg2 arg3 arg4 arg5 arg6)
           (if (and (< ret 0) (> ret -4096))
               (,(upl-print-static-string "syscall error: ")
                 (call number-to-string (var-addr hack-8-byte-buf) (- 0 ret))
                 (call number-string-length (var-addr num-length) (- 0 ret))
                 (syscall (var-addr ret) 1 1 (var-addr hack-8-byte-buf) num-length 1 2 3)
                ,(upl-exit 1)))
           (i64:= retcode-out ret)))
    (proc write-to-stdout (buf length) ((sys-ret 0) (written 0))
          ((while (> length written)
                  ((call chk-syscall (-> sys-ret) 1 1 (+ buf written) (- length written) 1 2 3)
                   (:= written (+ written sys-ret))))))
    (proc gc-malloc (addr-out size) ((bump-ptr 0))
          (
           (:= bump-ptr (i64@ ,alloc-bump-ptr))
           (if (> (+ bump-ptr size) ,(+ alloc-arena alloc-arena-size))
               (
                ,(upl-print-static-string "malloc arena overflow\n")
                ,(upl-exit 1)
                ) ())
           (i64:= ,alloc-bump-ptr (+ bump-ptr size))
           (i64:= addr-out bump-ptr)
           ))
;   object header layout [gc flags (1 byte)][type id (1 byte)]
;   i64 obj type id = 0

    (proc allocate-i64 (addr-out num) ((addr 0))
          ((call gc-malloc addr 10)
           (u8:=  addr 0)
           (u8:=  (+ addr 1) 0) ; type id
           (i64:= (+ addr 2) num)
           (i64:= addr-out addr)))
;   pair obj type id = 1

    (proc allocate-pair (addr-out first second) ((addr 0))
          ((call gc-malloc addr 18)
          (u8:=  addr 0)
          (u8:=  (+ addr 1) 1) ; type id
          (i64:= (+ addr 2) first)
          (i64:= (+ addr 10) second)
          (i64:= addr-out addr)))


    (proc number-string-length (length-ptr num) ((l 0))
          (
           (if (= num 0)
             ((i64:= length-ptr 1)
              (return)))
           (if (< num 0)
             ((:+= l 1)) ())
           (while (!= num 0)
                  ((:+= l 1)
                   (:= num (/ num 10))))
           (i64:= length-ptr l)
           ))
    (proc number-to-string (buf num) ((char-ptr 0))
          (
           (call number-string-length (var-addr char-ptr) num)
           (:= char-ptr (+ (- char-ptr 1) buf)) ; start printing the number from the end
           (if (= num 0)
             ((u8:= char-ptr 48)
              (return)) ())
           (if (> num 0)
             ((while (not (= num 0))
                    ((u8:= char-ptr (+ 48 (% num 10)))
                     (:+= char-ptr -1)
                     (:= num (/ num 10)))))
             ((while (not (= num 0))
                    ((u8:= char-ptr (+ 48 (* (% num 10) -1)))
                     (:+= char-ptr -1)
                     (:= num (/ num 10))))
              (u8:= char-ptr 45))) ; minus sign
           ))
    (proc number-to-hex-string (buf num) ((i 0)
                                          (octet 0))
          (
           (u8:= (+ buf 0) 48)  ; 0
           (u8:= (+ buf 1) 120) ; x
           (:+= buf 2)
           (while (< i 16)
                  ((:= octet (bit-and 15 (bit-rshift num (* 4 (- 15 i)))))
                   (if (> octet 9)
                       ((u8:= (+ buf i) (+ octet 87)))
                       ((u8:= (+ buf i) (+ octet 48))))
                   (:+= i 1)))
           ))
    (proc print-number (num) ((str-length 0))
          (
           (call number-to-string ,tmp-4k-buffer num)
           (call number-string-length (-> str-length) num)
           (call write-to-stdout ,tmp-4k-buffer str-length)
           ))
    (proc print-hex-number (num) ()
          (
           (call number-to-hex-string ,tmp-4k-buffer num)
           (call write-to-stdout ,tmp-4k-buffer 18)
           ))
    (proc print-newline () ()
          (
           (u8:= ,tmp-4k-buffer 10) ; newline
           (call write-to-stdout ,tmp-4k-buffer 1)
           ))
    (proc print-stack-trace () ((test 128)
                                (current-fp (get-fp))
                                (str-buf 0)
                                (str-length 0))
          (
           (call print-newline)
           ,(upl-print-static-string "ptr?:\n")
           (call print-hex-number (i64@ (+ current-fp 8)))
           (call print-newline)
           (call print-hex-number (i64@ (+ current-fp 0)))
           (call print-newline)
           (call print-hex-number (i64@ (+ current-fp -8)))
           (call print-newline)
           (call print-newline)

           ,(upl-print-static-string "Stacktrace:\n")
           (while (!= current-fp 0)
                  ((call print-hex-number (i64@ (+ current-fp 8)))
                   (u8:= ,tmp-4k-buffer 10) ; newline
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (call function-name-by-iptr (-> str-buf) (-> str-length) (i64@ (+ current-fp 8)))
                   (call write-to-stdout str-buf str-length)
                   (u8:= ,tmp-4k-buffer 10) ; newline
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (:= current-fp (i64@ current-fp))))
           ))
    (proc function-name-by-iptr (buf-ptr size-ptr iptr) ((index 0)
                                                         (proc-dict (i64@ ,proc-dict-ptr))
                                                         (dict-array 0)
                                                         (string 0))
          (
           (:= dict-array (+ proc-dict 8))
           ; skip records
           (while (and (< index (i64@ proc-dict)) (> iptr (i64@ (+ (* index 16) dict-array))))
                  ((:+= index 1)))
           (if (and (> index 0) (< index (i64@ proc-dict))) ; last function in dict is end-of-range pointer
               ((:+= index -1)
                (:= string (i64@ (+ (+ (* index 16) dict-array) 8)))
                (i64:= buf-ptr (+ string 8))
                (i64:= size-ptr (i64@ string)))
               ((call print-hex-number iptr)
                ,(upl-print-static-string "\nfunction-name-by-iptr: error, no such function")
                ,(upl-exit 1)))
           ))
    (proc print-proc-dict () ((index 0)
                              (proc-dict (i64@ ,proc-dict-ptr))
                              (dict-array 0)
                              (string 0))
          (
           (:= dict-array (+ proc-dict 8))
           ; skip records
           (while (< index (i64@ proc-dict))
                  (
                   (:= string (i64@ (+ (+ (* index 16) dict-array) 8)))
                   ;(call print-hex-number (+ (* index 16) dict-array))
                   (call print-hex-number (i64@ (+ (* index 16) dict-array)))
                   (u8:= ,tmp-4k-buffer 32) ; whitespace
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (call print-number (i64@ (+ (* index 16) dict-array)))
                   (u8:= ,tmp-4k-buffer 32) ; whitespace
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (call write-to-stdout (+ string 8) (i64@ string))
                   (u8:= ,tmp-4k-buffer 10) ; newline
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (:+= index 1)
                   ))
           ))
    (proc main-proc () ((string 0) (index 0))
          (
           (while (< index (i64@ (i64@ ,proc-dict-ptr)))
                  ((:= string (i64@ (+ (* index 16)
                                       (+ 16 (i64@ ,proc-dict-ptr)))))
                   (call write-to-stdout (+ string 8) (i64@ string))
                   (u8:= ,tmp-4k-buffer 10) ; newline
                   (call write-to-stdout ,tmp-4k-buffer 1)
                   (:+= index 1)))
          ))
    (proc c () () ((call print-stack-trace)))
    (proc b () ((d 0)) ((call c) (:= d 0)))
    (proc a () ((d 0)) ((call b) (:= d 0)))
    (proc start () ()
          (
           (zero-fp)
           (call print-proc-dict)
           (call a)
          ))
    ))

(define proc-list (compile-upl-to-native upl-code))
(foldl (lambda (next prev)
         (begin
           (assert-stmt "pointers are not sorted in proc-list" (< prev (cdr next)))
           (cdr next)))
       0
       proc-list)

(let ((proc-count (length proc-list))
      (names (map (lambda (n-and-p) (symbol-to-string (car n-and-p))) proc-list))
      (name-lengths (map string-length names))
      (name-length-sum (foldl + 0 name-lengths))
      (dict-ptr (syscall-mmap-anon (+ (+ 8 (* 16 proc-count)) (+ (* 8 proc-count) name-length-sum))))
      (dict-array-ptr (+ dict-ptr 8))
      (names-array-ptr (+ dict-array-ptr (* 16 proc-count)))
      (name-ptrs (prefix-sum names-array-ptr (map (lambda (l) (+ l 8)) name-lengths))))
  (write-mem-i64 dict-ptr proc-count)
  (foreach-with-index (lambda (proc-ptr-and-name-ptr index)
                        (begin
                          (write-mem-i64 (+ dict-array-ptr (* index 16)) (car proc-ptr-and-name-ptr))
                          (write-mem-i64 (+ dict-array-ptr (+ (* index 16) 8)) (cdr proc-ptr-and-name-ptr))))
                      (zip (map cdr proc-list) name-ptrs))
  (foreach (lambda (string-and-ptr)
             (begin
               (write-mem-i64 (cdr string-and-ptr) (string-length (car string-and-ptr)))
               (string-to-native-buffer (car string-and-ptr) (+ (cdr string-and-ptr) 8))))
           (zip names name-ptrs))
  (println (list "dict-ptr" dict-ptr))
  (write-mem-i64 proc-dict-ptr dict-ptr))

(foreach println proc-list)
(println "native call:")
(native-call (alist-lookup proc-list 'start))
(println "native call end")
(enable-REPL-print)
