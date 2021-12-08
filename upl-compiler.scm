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
            (write-mem-byte (+ 7 ptr) #x52) ; push rax
            ))
        "mod"))

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

(define (prefix-sum l)
  (reverse (cdr (foldl
                  (lambda (i acc) (cons (+ i (car acc)) acc))
                  (list 0)
                  l))))

(define symbol-to-instruction
  (list
    (list '+       2 add-instruction)
    (list '-       2 sub-instruction)
    (list '*       2 mul-instruction)
    (list '/       2 div-instruction)
    (list '%       2 mod-instruction)
    (list 'get-i64 1 get-i64-instruction)
    (list 'get-u8  1 get-u8-instruction)
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
    ((equal? 'var-addr (car expr))
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
    (cond ((equal? stmt-type 'set-i64)
           (append
             (compile-expr (first stmt-args) local-vars)
             (compile-expr (second stmt-args) local-vars)
             (list set-i64-instruction)))
          ((equal? stmt-type 'set-u8)
           (append
             (compile-expr (first stmt-args) local-vars)
             (compile-expr (second stmt-args) local-vars)
             (list set-u8-instruction)))
          ((equal? stmt-type 'inc-var)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (compile-statement '(set-var ,var (+ ,var ,val-expr)) local-vars procedure-list)))
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
           (let ((proc (assert (alist-lookup procedure-list (car (cdr stmt))) not-empty? (list "undefined function:" (first stmt-args))))
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
        (list
          (add-rsp-instruction (* 8 (length local-vars)))
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

(define buffer (syscall-mmap-anon 1000))

(define test-string-buffer (syscall-mmap-anon 1000))
(define test-string "test\n")
(define test-string-length (string-length test-string))
(string-to-native-buffer test-string test-string-buffer)

(define (compile-upl-to-native fpointer upl-code)
  (let ((proc-list-and-insts (compile-upl upl-code))
        (proc-list (first proc-list-and-insts))
        (instructions (second proc-list-and-insts))
        (_ (validate-stack-machine-code instructions))
        (inst-locations (map
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
             insts-and-locations)
    (map (lambda (proc) (cons
                          (first proc)
                          (alist-lookup labels-and-locations (second proc))))
         proc-list)))

(define upl-code 
  '((proc write-hello (ptr) ()
          (
           (set-u8 ,buffer 104)
           (set-u8 ,(+ 1 buffer) 101)
           (set-u8 ,(+ 2 buffer) 108)
           (set-u8 ,(+ 3 buffer) 108)
           (set-u8 ,(+ 4 buffer) 111)
           ))
    (proc number-string-length (length-ptr num) ((l 0))
          (
           (if (= num 0)
             ((set-i64 length-ptr 1)
              (return)) ())
           (if (< num 0)
             ((inc-var l 1)) ())
           (while (!= num 0)
                  ((inc-var l 1)
                   (set-var num (/ num 10))))
           (set-i64 length-ptr l)
           ))
    (proc number-to-string (buf num) ((char-ptr 0))
          (
           (call number-string-length (var-addr char-ptr) num)
           (set-var char-ptr (+ (- char-ptr 1) buf)) ; start printing the number from the end
           (if (= num 0)
             ((set-u8 char-ptr 48)
              (return)) ())
           (if (> num 0)
             ((while (not (= num 0))
                    ((set-u8 char-ptr (+ 48 (% num 10)))
                     (inc-var char-ptr -1)
                     (set-var num (/ num 10)))))
             ((while (not (= num 0))
                    ((set-u8 char-ptr (+ 48 (* (% num 10) -1)))
                     (inc-var char-ptr -1)
                     (set-var num (/ num 10))))
              (set-u8 char-ptr 45))) ; minus sign
           ))
    (proc main-proc () ((num -9223372036854775808) (syscall-ret-val 0))
          (
           (call number-string-length ,buffer num)
           (call number-to-string ,(+ buffer 8) num)
           (set-u8 (+ (+ ,buffer 8) (get-i64 ,buffer)) 10)
           (syscall (var-addr syscall-ret-val) 1 1 ,(+ buffer 8) (+ 1 (get-i64 ,buffer)) 1 2 3)
           ;(syscall ,buffer 39 1 2 3 4 5 6)
           ;(call write-hello ,buffer)
           ;(while (< i 5)
           ;       ((set-u8 (+ i ,buffer) (+ 0 (get-u8 (+ i ,buffer))))
           ;        (set-var i (+ i 1))))
           ;)
          ))
    ))

(define fpointer (syscall-mmap-anon-exec 1000))

(define proc-list (compile-upl-to-native fpointer upl-code))

(enable-REPL-print)
(native-call (alist-lookup proc-list 'main-proc))
"number:"
(string-from-native-buffer (+ buffer 8) (read-mem-i64 buffer))
;"getpid"
;(syscall 39 1 2 3 4 5 6)
"==========================="
"string length:"
(read-mem-i64 buffer)
(read-mem-i64 (+ 8 buffer))
