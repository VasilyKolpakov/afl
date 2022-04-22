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

(define push-rax-instruction
  (list 1 (lambda (ptr)
            (write-mem-byte ptr #x50) ; push rax
            )
        (list "push rax")))

(define set-rax-instruction
  (list 1 (lambda (ptr)
            (write-mem-byte ptr #x58) ; pop rax
            )
        (list "pop rax")))

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

(define syscall-instruction 
  (list 13 (lambda (ptr)
             (begin
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
               (write-mem-byte (+ 10 ptr) #x0f) ; syscall
               (write-mem-byte (+ 11 ptr) #x05)
               (write-mem-byte (+ 12 ptr) #x50) ; mov [r15], rax
               ))
        "syscall"))

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

(define (push-label-pointer-instruction label)
  (list 11
        (lambda (ptr label-locs)
             (let ((label-ptr
                     (assert (alist-lookup label-locs label) not-empty? "label pointer")))
               (generate-push-imm ptr label-ptr)))
        (list "push-label-pointer" label)))

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
    (list 'syscall    7 syscall-instruction)
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

(define (context-local-vars context) (first context))
(define (context-label-list context) (second context))

(define (compile-list expr rest context)
  (let ((local-vars (context-local-vars context))
        (label-list (context-label-list context)))
    (cond
      ((equal? 'function-pointer (car expr))
       (let ((label (second expr))
             (proc (assert (alist-lookup label-list label) not-empty? (list "undefined function:" expr)))
             (proc-label (second proc)))
         (append
           (list (push-label-pointer-instruction proc-label))
           rest)))
      ((equal? 'fcall (car expr))
       (let ((proc (assert (alist-lookup label-list (car (cdr expr))) not-empty? (list "undefined function:" (second expr))))
             (proc-args (cdr (cdr expr)))
             (proc-label (second proc))
             (proc-arg-count (nth 2 proc)))
         (assert-stmt (list proc "is a func") (equal? 'func (first proc)))
         (assert-stmt (list (second expr) "number of args") (= proc-arg-count (length proc-args)))
         (append
           (list (add-rsp-instruction -16))
           (flatmap (lambda (expr) (compile-expression expr context)) proc-args)
           (list
             (add-rsp-instruction (* 8 (+ 2 proc-arg-count)))
             (call-instruction proc-label)
             push-rax-instruction)
           rest)))
      ((equal? '-> (car expr))
       (let ((var (car (cdr expr)))
             (var-index (index-of local-vars var)))
         (if (empty? var-index) (panic "ref: bad variable:" var) '())
         (append (list (push-var-addr-instruction var-index))
                 rest)))
      (else
        (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty? (list "undefined operator:" (car expr)))))
          (assert-stmt "arg count matches" (= (length (cdr expr)) (car arg-count-and-inst)))
          (foldr
            (lambda (expr rest) (compile-expression-rec expr rest context))
            (cons (car (cdr arg-count-and-inst)) rest)
            (cdr expr)))))))

(define (compile-expression-rec expr rest context)
  (let ((local-vars (context-local-vars context))
        (var-index (index-of local-vars expr)))
    (cond
      ((not-empty? var-index) (cons (push-var-instruction var-index) rest))
      ((number? expr) (cons (push-imm-instruction expr) rest))
      ((list? expr) (compile-list expr rest context))
      (else (panic "bad expression: " (list "expression:" expr "context:" context))))))

(define (compile-expression expr context)
  (compile-expression-rec expr '() context))

(define (compile-boolean-expression cond-expr context label true-label?)
  (let ((cond-type (car cond-expr))
        (cond-args (cdr cond-expr))
        (local-vars (context-local-vars context))
        (label-list (context-label-list context)))
    (cond ((equal? cond-type 'not)
           (compile-boolean-expression (second cond-expr) context label (not true-label?)))
          ((equal? cond-type 'and)
           (let ((first-child (first cond-args))
                 (second-child (second cond-args)))
             (assert-stmt "and expression has 2 children" (= (length cond-args) 2))
             (if true-label?
               (let ((false-label (new-label)))
                 (append
                   (compile-boolean-expression first-child context false-label #f)
                   (compile-boolean-expression second-child context label #t)
                   (list false-label)))
               (append
                   (compile-boolean-expression first-child context label #f)
                   (compile-boolean-expression second-child context label #f)))))

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
                (lambda (expr rest) (compile-expression-rec expr rest context))
                (list (cond-inst-gen label))
                cond-args))))))

(define (compile-statement stmt context)
  (assert-stmt (list "statement must be a list, not:" stmt) (list? stmt))
  (let ((stmt-type (car stmt))
        (stmt-args (cdr stmt))
        (local-vars (context-local-vars context))
        (label-list (context-label-list context))
        (compile-expr (lambda (expr local-vars) (compile-expression expr context))))
    (cond ((equal? stmt-type 'i64:=)
           (append
             (compile-expr (first stmt-args) local-vars)
             (compile-expr (second stmt-args) local-vars)
             (list set-i64-instruction)))
          ((equal? stmt-type 'u8:=)
           (begin
             (assert-stmt (list "u8:= statement has 2 parts" stmt) (equal? 2 (length stmt-args)))
             (append
               (compile-expr (first stmt-args) local-vars)
               (compile-expr (second stmt-args) local-vars)
               (list set-u8-instruction))))
          ((equal? stmt-type ':+=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (compile-statement '(:= ,var (+ ,var ,val-expr)) context)))
          ((equal? stmt-type ':=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (if (empty? var-index) (panic "set-var: bad variable:" var) '())
             (compile-expression-rec
               val-expr
               (list (set-var-instruction var-index))
               context)))
          ((equal? stmt-type 'call)
           (let ((proc (assert (alist-lookup label-list (car (cdr stmt))) not-empty? (list "undefined function:" (first stmt-args))))
                 (proc-args (cdr (cdr stmt)))
                 (proc-label (second proc))
                 (proc-arg-count (nth 2 proc)))
             (assert-stmt (list proc "is a proc") (equal? 'proc (first proc)))
             (assert-stmt (list (first stmt-args) "number of args") (= proc-arg-count (length proc-args)))
             (append
               (list (add-rsp-instruction -16))
               (flatmap (lambda (expr) (compile-expr expr local-vars)) proc-args)
               (list
                 (add-rsp-instruction (* 8 (+ 2 proc-arg-count)))
                 (call-instruction proc-label)))))
          ((equal? stmt-type 'return)
           (begin
             (assert-stmt "return has 0 args" (= 0 (length stmt-args)))
             (list
               (add-rsp-instruction (* 8 (length local-vars)))
               return-instruction)))
          ((equal? stmt-type 'return-val)
           (begin
             (assert-stmt "return-val has 1 args" (= 1 (length stmt-args)))
             (append
               (compile-expr (first stmt-args) local-vars)
               (list
                 set-rax-instruction
                 (add-rsp-instruction (* 8 (length local-vars)))
                 return-instruction))))
          ((equal? stmt-type 'while)
           (begin
             (assert-stmt "while has 2 args" (= 2 (length stmt-args)))
             (let ((cond-expr (car stmt-args))
                   (body-statements (second stmt-args))
                   (compile-substatement (lambda (s) (compile-statement s context)))
                   (loop-label (new-label))
                   (end-label (new-label))
                   (cond-instructions (compile-boolean-expression cond-expr context end-label #f)))
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
                 (compile-substatement (lambda (s) (compile-statement s context)))
                 (else-label (new-label))
                 (cond-instructions (compile-boolean-expression cond-expr context else-label #f)))
             (append
               cond-instructions
               (flatmap compile-substatement then-branch)
               (list else-label)
               )))
          ((equal? stmt-type 'if)
           (let ((cond-expr (car stmt-args))
                 (then-branch (second stmt-args))
                 (else-branch (nth 2 stmt-args))
                 (compile-substatement (lambda (s) (compile-statement s context)))
                 (else-label (new-label))
                 (end-label (new-label))
                 (cond-instructions (compile-boolean-expression cond-expr context else-label #f)))
             (append
               cond-instructions
               (flatmap compile-substatement then-branch)
               (list (jmp-instruction end-label)
                     else-label)
               (flatmap compile-substatement else-branch)
               (list end-label)
               )))
          ((equal? stmt-type 'cond)
           (begin
             (assert-stmt "non-empty cond statement" (not (empty? stmt-args)))
             (assert-stmt "last cond statement is else" (equal? (car (last stmt-args)) 'else))
             (let ((c-branches (reverse (cdr (reverse stmt-args))))
                   (else-branch (last stmt-args))
                   (end-label (new-label))
                   (compile-substatement (lambda (s) (compile-statement s context))))
               (append
                 (flatmap (lambda (branch)
                            (let ((condition (first branch))
                                  (statements (second branch))
                                  (false-label (new-label)))
                              (append
                                (compile-boolean-expression condition context false-label #f)
                                (flatmap compile-substatement statements)
                                (list (jmp-instruction end-label)
                                      false-label))))
                          c-branches)
                 (flatmap compile-substatement (second else-branch))
                 (list end-label)))))
          (else (panic "bad statement" stmt)))))


(define (compile-procedure args
                           local-vars-with-inits
                           statements
                           label-list)
  (begin
    (assert-stmt (list "local vars must have init values, for example: (proc foo (a b) ((i 42)) ..., but was " local-vars-with-inits)
                 (andmap pair? local-vars-with-inits))
    (let ((local-var-inits (map (lambda (v) (car (cdr v))) local-vars-with-inits))
          (local-vars (append args (map car local-vars-with-inits))))
      (append
        (list
          set-frame-pointer-instruction
          (add-rsp-instruction (- 0 (* 8 (length args)))))
        (flatmap (lambda (var-init) (compile-expression var-init (list args label-list)))
                 local-var-inits)
        (flatmap (lambda (stmt) (compile-statement stmt (list local-vars label-list)))
                 statements)
        (list (add-rsp-instruction (* 8 (length local-vars)))
          return-instruction)))))

(define next-label-id (cell 0))

(define (new-label)
  (let ((id (cell-get next-label-id)))
    (cell-set next-label-id (+ id 1))
    id))

(define (label? v) (number? v))

(define (compile-upl code-chunks)
  (let ((chunks-with-labels
          (map (lambda (p) (cons p (new-label))) code-chunks))
        (procs-with-labels (filter (lambda (c-with-l) (equal? 'proc (first (car c-with-l)))) chunks-with-labels))
        (label-list
          (map (lambda (c-with-l)
                 (let ((chunk (car c-with-l))
                       (label (cdr c-with-l))
                       (chunk-type (first chunk))
                       (name (second chunk)))
                   (cond
                     ((equal? 'proc chunk-type)
                      (let ((arg-count (length (nth 2 chunk))))
                        (list name 'proc label arg-count)))
                     ((equal? 'func chunk-type)
                      (let ((arg-count (length (nth 2 chunk))))
                        (list name 'func label arg-count)))
                     ((equal? 'bytes chunk-type)
                      (list name 'bytes label))
                     (else (assert-stmt (list "chunk type" chunk-type) #f)))))
               chunks-with-labels))
        (procedure-list
          (map (lambda (p-with-l)
                 (let ((chunk (car p-with-l))
                       (label (cdr p-with-l))
                       (name (second chunk))
                       (arg-count (length (nth 2 chunk))))
                   (list name label arg-count)))
               procs-with-labels))
        (chunk-label-list
          (map (lambda (c-with-l)
                 (let ((chunk (car c-with-l))
                       (label (cdr c-with-l))
                       (name (second chunk)))
                   (cons name label)))
               chunks-with-labels)))
    (list
      label-list
      (flatmap
        (lambda (c-with-l)
          (let ((chunk (first c-with-l))
                (chunk-type (first chunk))
                (label (cdr c-with-l)))
            (cond
              ((or (equal? 'proc chunk-type) (equal? 'func chunk-type))
               (let ((args (nth 2 chunk))
                     (local-vars-with-inits (nth 3 chunk))
                     (statements (nth 4 chunk)))
                 (cons label (compile-procedure args local-vars-with-inits statements label-list))))
              ((equal? 'bytes chunk-type)
               (let ((bytes-list (nth 2 chunk)))
                 (cons label (list
                               (list
                                 (length bytes-list)
                                 (lambda (ptr)
                                   (foreach-with-index
                                     (lambda (b index) (write-mem-byte (+ ptr index) b))
                                     bytes-list))
                                 (second chunk))))))
              (else (begin
                      (println (list "bad code chunk" chunk))
                      (exit 1))))))
            chunks-with-labels))))


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
  (let ((chunk-label-list-and-insts (compile-upl upl-code))
        (chunk-label-list (first chunk-label-list-and-insts))
        (instructions (second chunk-label-list-and-insts))
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
                     (generator (second inst)))
                 (if (= (arity generator) 2)
                   (generator loc labels-and-locations)
                   (generator loc))))
             insts-and-locations)
    (append
      (map (lambda (chunk-name-and-label)
             (cons
               (car chunk-name-and-label)
               (assert (alist-lookup labels-and-locations (nth 2 chunk-name-and-label)) not-empty? (list "label location lookup" chunk-name-and-label))))
           chunk-label-list)
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
          '(i64:= ,ret-val-buffer (syscall 1 1 ,(+ str-buffer original-bump-index) ,str-length 1 2 3)))))))

(define dummy-buf (syscall-mmap-anon 1000))
(define (upl-exit code)
  '(i64:= ,dummy-buf (syscall 60 ,code 1 2 3 4 5)))

(define alloc-arena-size 409600)
(define alloc-arena (syscall-mmap-anon alloc-arena-size))
(define alloc-bump-ptr (syscall-mmap-anon 1000))
(write-mem-i64 alloc-bump-ptr alloc-arena)

(define tmp-4k-buffer (syscall-mmap-anon 4096))
(define proc-dict-ptr (syscall-mmap-anon 4096))

;   object header layout [gc flags (1 byte)][type id (1 byte)]
(define (upl-obj-type-id expr)
  '(u8@ (+ 1 ,expr)))
(define obj-header-size 2)
(define i64-obj-type-id 0)
;   [value: i64]
(define (upl-obj-i64-value expr)
  '(i64@ (+ ,obj-header-size ,expr)))

(define pair-obj-type-id 1)
;   [car: ref][cdr: ref]
(define (upl-obj-pair-car expr)
  '(i64@ (+ ,obj-header-size ,expr)))

(define (upl-obj-pair-cdr expr)
  '(i64@ (+ ,(+ 8 obj-header-size) ,expr)))

(define upl-code 
  '(
    (func chk-syscall (call-code arg1 arg2 arg3 arg4 arg5 arg6) ((ret 0) (hack-8-byte-buf 0) (num-length 0))
          ((:= ret (syscall call-code arg1 arg2 arg3 arg4 arg5 arg6))
           (if (and (< ret 0) (> ret -4096))
             (,(upl-print-static-string "syscall error: ")
               (call number-to-string (-> hack-8-byte-buf) (- 0 ret))
               (call number-string-length (-> num-length) (- 0 ret))
               (:= ret (syscall 1 1 (-> hack-8-byte-buf) num-length 1 2 3))
               ,(upl-exit 1)))
           (return-val ret)
           ))
    (proc write-to-stdout (buf length) ((sys-ret 0) (written 0))
          ((while (> length written)
                  (
                   (:= sys-ret (fcall chk-syscall 1 1 (+ buf written) (- length written) 1 2 3))
                   (:+= written sys-ret)))))
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
    (func syscall-mmap-anon (size) ()
          (
           (return-val (fcall chk-syscall ,syscall-mmap 
                    0 ; address hint
                    size
                    ,(bitwise-ior syscall-mmap-PROT-READ syscall-mmap-PROT-WRITE)
                    ,(bitwise-ior syscall-mmap-MAP-ANONYMOUS syscall-mmap-MAP-PRIVATE)
                    ,syscall-mmap-ABSENT-FD
                    0 ; offset
                    ))
           ))
    (func syscall-mremap-maymove (addr oldsize newsize) ()
          (
           (return-val (fcall chk-syscall ,syscall-mremap 
                    addr
                    oldsize
                    newsize
                    ,syscall-mremap-MREMAP-MAYMOVE
                    0 0 ; dummy args
                    ))
           ))
    (proc allocate-i64 (addr-out num) ((addr 0))
          ((call gc-malloc (-> addr) 10)
           (u8:=  addr 0)
           (u8:=  (+ addr 1) 0) ; type id
           (i64:= (+ addr 2) num)
           (i64:= addr-out addr)))
;   pair obj type id = 1

    (proc allocate-pair (addr-out first second) ((addr 0))
          ((call gc-malloc (-> addr) 18)
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
           (call number-string-length (-> char-ptr) num)
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
    (proc print-stack-trace () ()
          (
           (call print-stack-trace-from-fp-and-ip (get-fp) 0)
           ))
    (proc print-stack-trace-from-fp-and-ip (fp ip) ((current-fp fp)
                                                    (str-buf 0)
                                                    (str-length 0))
          (
           ,(upl-print-static-string "Stacktrace:\n")
           (if (!= ip 0)
               (
                (call function-name-by-iptr (-> str-buf) (-> str-length) ip)
                (call write-to-stdout str-buf str-length)
                (call print-newline)))

           (call function-name-by-iptr (-> str-buf) (-> str-length) (i64@ (+ current-fp 8)))
           (while (!= str-buf 0)
                  (
                   (call write-to-stdout str-buf str-length)
                   (call print-newline)
                   (:= current-fp (i64@ current-fp))
                   (call function-name-by-iptr (-> str-buf) (-> str-length) (i64@ (+ current-fp 8)))
                   ))
           ))
    ; sets buf-ptr to 0 if fucntion is not found
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
               (
                (:+= index -1)
                (:= string (i64@ (+ (+ (* index 16) dict-array) 8)))
                (i64:= buf-ptr (+ string 8))
                (i64:= size-ptr (i64@ string)))
               (
                (i64:= buf-ptr 0)))
           ))
    (func zero-if-list (list) ((pair-ptr list))
          (
           (while (and (!= pair-ptr 0) (= ,pair-obj-type-id ,(upl-obj-type-id 'pair-ptr)))
                  (
                   (:= pair-ptr ,(upl-obj-pair-cdr 'pair-ptr))))
           (return-val pair-ptr)
           ))
    (proc print-object (obj) ((obj-type-id 0) (tmp 0))
          (
           (:= obj-type-id ,(upl-obj-type-id 'obj))
           (cond
             ((= obj-type-id ,i64-obj-type-id)
              (
               (call print-number ,(upl-obj-i64-value 'obj))
               ))
             ((= obj-type-id ,pair-obj-type-id)
              (
               (if (= (fcall zero-if-list obj) 0)
                 (
                  ,(upl-print-static-string "zero-if-list: ")
                  (call print-number (fcall zero-if-list obj))
                  ,(upl-print-static-string "\n")
                  (:= tmp obj)
                  ,(upl-print-static-string "(")
                  (while (!= tmp 0)
                         (
                          (call print-object ,(upl-obj-pair-car 'tmp))
                          (:= tmp ,(upl-obj-pair-cdr 'tmp))
                          (if (!= tmp 0)
                            (,(upl-print-static-string " "))
                            ())
                          ))
                  ,(upl-print-static-string ")")
                  )
                 (
                  ,(upl-print-static-string "(")
                  (call print-object ,(upl-obj-pair-car 'obj))
                  ,(upl-print-static-string " . ")
                  (call print-object ,(upl-obj-pair-cdr 'obj))
                  ,(upl-print-static-string ")")
                  ))
               ))
             (else (
                    ,(upl-print-static-string "bad obj-type\n")
                    )))
           ))
    (proc c () () ((i64:= 0 0)))
    (proc b () ((d 0)) ((call c) (:= d 0)))
    (proc a () ((d 0)) ((call b) (:= d 0)))

    (proc cond-test () ((i 0))
          (
           (while (< i 4)
                  (
                   (cond
                     ((= i 0) (,(upl-print-static-string "zero")))
                     ((= i 1) (,(upl-print-static-string "one")))
                     ((= i 2) (,(upl-print-static-string "two")))
                     (else (,(upl-print-static-string "else"))))
                   ,(upl-print-static-string "\n")
                   (:+= i 1)
                   ))
          ))

    (bytes sigaction-restorer
           (
            #xb8 #x0f #x00 #x00 #x00 ; mov    eax,0xf ; sigreturn
            #x0f #x05                ; syscall
            ))
    
    (bytes sigsegv-handler
           (
            ; return address is on top of the stack
            ; need to push a dummy value (saved fp placeholder)
            ; then push the argument and then restore stack pointer
            ; to match upl calling convention
            #x50 #x52 ; push rax ; push rdx
            #x48 #x83 #xc4 #x10 ; add rsp, 16 
            ))
    (proc __sigsegv-handler (ucontext) ((fp-reg 0) (ip-reg 0))
          (
           ,(upl-print-static-string "=========== Segmentation fault\n")
           ; ucontext_t.uc_mcontext.__ctx = 40 bytes offset
           (:= fp-reg (i64@ (+ 40 (+ (* 10 8) ucontext))))
           (:= ip-reg (i64@ (+ 40 (+ (* 16 8) ucontext))))
           (call print-stack-trace-from-fp-and-ip fp-reg ip-reg)
           ,(upl-exit 1)
           ))
    (proc test-print-object () ((obj 0) (obj2 0) (obj3 0))
          (
           (call allocate-i64 (-> obj) 42)
           (call allocate-pair (-> obj2) obj 0)
           (call allocate-i64 (-> obj) 420)
           (call allocate-pair (-> obj3) obj obj2)
           (call print-object obj3)
           ,(upl-print-static-string "\n")
           ))
    (func test-func (num) ()
          (
           (return-val num)
           ))
    (proc tests () ((tmp 0))
          (
           ,(upl-print-static-string "test mmap\n")
           (:= tmp (fcall syscall-mmap-anon 4096))
           (u8:= (+ 4095 tmp) 100)
           ,(upl-print-static-string "test mremap\n")
           (:= tmp (fcall syscall-mremap-maymove tmp 4096 (* 2 4096)))
           (call print-number (u8@ (+ 4095 tmp)))
           (call print-newline)
           ))
    (proc main () ((syscall-ret 0) (sigaction-struct ,tmp-4k-buffer))
          (
           (i64:= (+ sigaction-struct 0 ) (function-pointer sigsegv-handler))
           (i64:= (+ sigaction-struct 8 ) #x4000004) ; flags SA_SIGINFO | SA_RESTORER
           (i64:= (+ sigaction-struct 16) (function-pointer sigaction-restorer))
           (i64:= (+ sigaction-struct 24) 0) ; sig mask
           (:= syscall-ret (syscall
                    13 ; sigaction
                    11 ; sigsegv signal
                    sigaction-struct
                    0 ; old sigaction struct
                    8 ; sig mask size
                    1 2))
           (call print-number (fcall test-func 1111))
           (call print-newline)
           (call test-print-object)

           ,(upl-print-static-string "end\n")
           (call tests)

           (call a)
          ))
    ))

(define name-to-iptr-list (compile-upl-to-native upl-code))
(foldl (lambda (next prev)
         (begin
           (assert-stmt "pointers are not sorted in name-to-iptr-list" (< prev (cdr next)))
           (cdr next)))
       0
       name-to-iptr-list)

(let ((proc-count (length name-to-iptr-list))
      (names (map (lambda (n-and-p) (symbol-to-string (car n-and-p))) name-to-iptr-list))
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
                      (zip (map cdr name-to-iptr-list) name-ptrs))
  (foreach (lambda (string-and-ptr)
             (begin
               (write-mem-i64 (cdr string-and-ptr) (string-length (car string-and-ptr)))
               (string-to-native-buffer (car string-and-ptr) (+ (cdr string-and-ptr) 8))))
           (zip names name-ptrs))
  (write-mem-i64 proc-dict-ptr dict-ptr))

(println "native call:")
(native-call (alist-lookup name-to-iptr-list 'main))
(println "native call end")
(enable-REPL-print)
