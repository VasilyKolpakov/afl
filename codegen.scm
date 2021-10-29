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

(define (generate-set-64 ptr)
  (write-mem-byte ptr #x5f) ; pop rdi
  (write-mem-byte (+ 1 ptr) #x58) ; pop rax
  (write-mem-byte (+ 2 ptr) #x48) ; mov [rax], rdi
  (write-mem-byte (+ 3 ptr) #x89)
  (write-mem-byte (+ 4 ptr) #x38)
  )

(define set-64-instruction (list 5 generate-set-64 "set-64"))

(define deref-instruction
  (list 5
        (lambda (ptr)
          (begin
            (write-mem-byte ptr       #x5f) ; pop rdi
            (write-mem-byte (+ 1 ptr) #x48) ; mov rax,QWORD PTR [rdi]
            (write-mem-byte (+ 2 ptr) #x8b)
            (write-mem-byte (+ 3 ptr) #x07)
            (write-mem-byte (+ 4 ptr) #x50) ; push rax 
            ))
        "deref"))

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

(define (generate-push-var-ref ptr index)
  (assert-stmt "index >= 0" (>= index 0))
  (assert-stmt "index < 15" (< index 15))
  (write-mem-byte ptr       #x48) ; lea rax,[rbp-index*8] 
  (write-mem-byte (+ 1 ptr) #x8d)
  (write-mem-byte (+ 2 ptr) #x45)
  (write-mem-byte (+ 3 ptr) (- 0 (* (+ 1 index) 8)))
  (write-mem-byte (+ 4 ptr) #x50) ; push rax
  )

(define (push-var-ref-instruction index)
  (list 5 (lambda (ptr) (generate-push-var-ref ptr index)) (list "push-var-ref" index)))

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

(define (generate-set-frame-pointer ptr)
  (write-mem-byte ptr       #x55) ; push rbp
  (write-mem-byte (+ 1 ptr) #x48) ; mov rbp,rsp
  (write-mem-byte (+ 2 ptr) #x89)
  (write-mem-byte (+ 3 ptr) #xe5)
  )

(define set-frame-pointer-instruction
  (list 4 generate-set-frame-pointer "set-frame-pointer"))

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
          (generate-jmp ptr (assert (alist-lookup label-locs target-label) not-empty?)))
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
          (generate-call ptr (assert (alist-lookup label-locs target-label) not-empty?)))
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
          (generate-cond-jmp cond-code ptr (assert (alist-lookup label-locs target-label) not-empty?)))
        (list "cond-jmp" cond-code target-label)))

(define (cond-jmp-instruction-gen cond-code)
  (lambda (label)
    (cond-jmp-instruction cond-code label)))

(define (prefix-sum l)
  (reverse (cdr (foldl
                  (lambda (i acc) (cons (+ i (car acc)) acc))
                  (list 0)
                  l))))

(define symbol-to-instruction
  (list
    (list '+ 2 add-instruction)
    (list 'deref 1 deref-instruction)
    ))

(define symbol-to-cond-goto-instruction
  (list
    (list '= 2 (cond-jmp-instruction-gen #x84))
    (list '> 2 (cond-jmp-instruction-gen #x87))
    ))

(define (compile-list expr rest local-vars)
  (cond
    ((equal? 'ref (car expr))
     (let ((var (car (cdr expr))) 
           (var-index (index-of local-vars var)))
       (if (empty? var-index) (panic "ref: bad variable:" var) '())
       (list (push-var-ref-instruction var-index))))
    (else
      (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty?)))
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
      (else (panic "bad expr: " (list expr local-vars))))))

(define (compile-expr expr local-vars)
  (compile-expr-rec expr '() local-vars))

(define (compile-statement stmt local-vars)
  (let ((stmt-type (car stmt))
        (stmt-args (cdr stmt)))
    (cond ((equal? stmt-type 'set-64)
           (append
             (compile-expr (car (cdr stmt)) local-vars)
             (compile-expr (car (cdr (cdr stmt))) local-vars)
             (list set-64-instruction)))
          ((equal? stmt-type 'set-var)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (if (empty? var-index) (panic "set-var: bad variable:" var) '())
             (compile-expr-rec
               val-expr
               (list (set-var-instruction var-index))
               local-vars)))
          ((equal? stmt-type 'call)
           (let ((proc (car (cdr stmt)))
                 (proc-args (cdr (cdr stmt)))
                 (proc-label (car proc))
                 (proc-arg-count (car (cdr proc))))
             (assert-stmt "number of args" (= proc-arg-count (length proc-args)))
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
          ((equal? stmt-type 'label)
           (list (car (cdr stmt))))
          ((equal? stmt-type 'goto)
           (let ((label (car (cdr stmt))))
             (list (jmp-instruction label))))
          ((equal? stmt-type 'goto-if)
           (let ((cond-expr (car (cdr stmt)))
                 (label (car (cdr (cdr stmt)))))
             (let ((arg-count-and-cond-inst
                     (assert
                       (alist-lookup symbol-to-cond-goto-instruction (car cond-expr))
                       not-empty?))
                   (arg-count (car arg-count-and-cond-inst))
                   (cond-inst-gen (car (cdr arg-count-and-cond-inst))))
               (assert-stmt "cond expr: arg count matches" (= (length (cdr cond-expr)) arg-count))
               (foldr 
                 (lambda (expr rest) (compile-expr-rec expr rest local-vars))
                 (list (cond-inst-gen label))
                 (cdr cond-expr))))) 
          (else (panic "bad statement" stmt)))))



(define (compile-procedure args local-vars-with-inits statements)
  (let ((local-var-inits (map (lambda (v) (car (cdr v))) local-vars-with-inits))
        (local-vars (append args (map car local-vars-with-inits))))
    (append
      (list
        set-frame-pointer-instruction
        (add-rsp-instruction (- 0 (* 8 (length args)))))
      (flatmap (lambda (var-init) (compile-expr var-init '()))
               local-var-inits)
      (flatmap (lambda (stmt) (compile-statement stmt local-vars))
               statements)
      (list
        (add-rsp-instruction (* 8 (length local-vars)))
        return-instruction))))

(define next-label-id (cell 0))

(define (generate-label)
  (let ((id (cell-get next-label-id)))
    (cell-set next-label-id (+ id 1))
    id))

(define (label? v) (number? v))

(define (make-procedure args locals stmts)
  (let ((new-label (generate-label))
        (instructions (compile-procedure args locals stmts)))
    (list
      (list new-label (length args))
      (cons new-label instructions))))

(define buffer (syscall-mmap-anon 1000))
(write-mem-i64 buffer 41)
(define the-label (generate-label))
(define label-if-equal (generate-label))
(define label-end-if (generate-label))
(define instructions_
  (list
    set-frame-pointer-instruction
    (push-imm-instruction 42)
    (push-var-ref-instruction 0)
    (push-imm-instruction 4422)
    set-64-instruction
    (push-imm-instruction buffer)
    (push-var-instruction 0)
    set-64-instruction
    (add-rsp-instruction (* 1 8))
    ;(call-instruction the-label)

    return-instruction
    ))

(define set-64-proc (make-procedure
    '(ptr val)
    '()
    '(
      (goto-if (> 9 9) ,the-label)
      (set-64 (ref val) (+ val 100))
      (set-64 ptr val)
      (label ,the-label)
      )))

(define main-proc (make-procedure
    '()
    '()
    '(
      (set-64 ,buffer (+ 1 (deref ,buffer)))
      ;(set-64 ,buffer 1 )
      ;(syscall ,buffer 39 1 2 3 4 5 6)
      ;(call ,(car set-64-proc) ,(+ 8 buffer) 44)
      ;(call ,(car set-64-proc) ,buffer 42)
      )))

(define instructions
  (append
    (car (cdr main-proc))
    (car (cdr set-64-proc))))

(define (instruction? i)
  (and
    (list? i)
    (>= (length i) 2)
    (number? (car i))
    (callable? (car (cdr i)))))

(define (instruction-size i)
  (cond
    ((label? i) 0)
    ((instruction? i) (car i))
    (else (panic "bad instruction:" i))))

(define fpointer (syscall-mmap-anon-exec 1000))

(println fpointer)

(let ((inst-locations (map
                        (lambda (l) (+ l fpointer))
                        (prefix-sum (map instruction-size instructions))))
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
           insts-and-locations))

(println "========== instructions ============")
(foreach
  println
  (map
    (lambda (i)
      (if (label? i)
          (list 'label i)
          (drop i 2)))
    instructions))

(enable-REPL-print)
(native-call fpointer)
"getpid"
(syscall 39 1 2 3 4 5 6)
(read-mem-i64 buffer)
(read-mem-i64 (+ 8 buffer))
