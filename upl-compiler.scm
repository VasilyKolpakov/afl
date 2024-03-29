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

(define (set-i64-instruction-imm-ptr dest-ptr)
  (list 14 
        (lambda (ptr)
          (begin
            (write-mem-byte       ptr #x48) ; mov rax, dest-ptr
            (write-mem-byte (+ 1  ptr) #xb8)
            (write-mem-i64  (+ 2  ptr) dest-ptr)
            (write-mem-byte (+ 10 ptr) #x5f) ; pop rdi
            (write-mem-byte (+ 11 ptr) #x48) ; mov [rax], rdi
            (write-mem-byte (+ 12 ptr) #x89)
            (write-mem-byte (+ 13 ptr) #x38)
            ))
        "set-i64-imm-ptr"))

(define (set-i64-instruction-imm-label-ptr label)
    (list 14 
          (lambda (ptr label-locator)
            (let ([label-ptr (label-locator label (list 'absolute-i64 label (+ ptr 2)))])
              [begin
                (write-mem-byte       ptr  #x48) ; mov rax, label-ptr
                (write-mem-byte (+ 1  ptr) #xb8)
                (write-mem-i64  (+ 2  ptr) label-ptr)
                (write-mem-byte (+ 10 ptr) #x5f) ; pop rdi
                (write-mem-byte (+ 11 ptr) #x48) ; mov [rax], rdi
                (write-mem-byte (+ 12 ptr) #x89)
                (write-mem-byte (+ 13 ptr) #x38)
                ]))
          (list "set-i64-imm-label-ptr" label)))

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

(define (set-tail-call-arg-instruction arg-count index)
  (assert-stmt "index >= 0" (>= index 0))
  (assert-stmt "index < 15" (< index 15))
  (list 9
        (lambda (ptr)
          [begin
            (write-mem-byte ptr       #x48) ; mov    rax,QWORD PTR [rsp+(arg-count - 1 - index)]
            (write-mem-byte (+ 1 ptr) #x8b)
            (write-mem-byte (+ 2 ptr) #x44)
            (write-mem-byte (+ 3 ptr) #x24)
            (write-mem-byte (+ 4 ptr) (* (- (- arg-count 1) index) 8))

            (write-mem-byte (+ 5 ptr) #x48) ; QWORD PTR [rbp+___],rax
            (write-mem-byte (+ 6 ptr) #x89)
            (write-mem-byte (+ 7 ptr) #x45)
            (write-mem-byte (+ 8 ptr) (- 0 (* (+ 1 index) 8)))
            ])))

(define (single-byte-instruction byte instruciton-name)
    (list 1 
          (lambda (ptr) (write-mem-byte ptr byte))
          instruciton-name))

(define (two-byte-instruction first-byte second-byte instruciton-name)
    (list 2 
          (lambda (ptr) 
            (begin
              (write-mem-byte ptr first-byte)
              (write-mem-byte (+ 1 ptr) second-byte)
            ))
          instruciton-name))

(define push-rax-instruction (single-byte-instruction #x50 "push rax"))
(define push-rdi-instruction (single-byte-instruction #x57 "push rdi"))
(define push-rsi-instruction (single-byte-instruction #x56 "push rsi"))
(define push-rdx-instruction (single-byte-instruction #x52 "push rdx"))
(define push-rcx-instruction (single-byte-instruction #x51 "push rcx"))
(define push-r8-instruction  (two-byte-instruction    #x41 #x50 "push r8"))
(define push-r9-instruction  (two-byte-instruction    #x41 #x51 "push r9"))

(define pop-rax-instruction (single-byte-instruction #x58 "pop rax"))
(define pop-rdi-instruction (single-byte-instruction #x5f "pop rdi"))
(define pop-rsi-instruction (single-byte-instruction #x5e "pop rsi"))
(define pop-rdx-instruction (single-byte-instruction #x5a "pop rdx"))
(define pop-rcx-instruction (single-byte-instruction #x59 "pop rcx"))
(define pop-r8-instruction  (two-byte-instruction    #x41 #x58 "pop r8"))
(define pop-r9-instruction  (two-byte-instruction    #x41 #x59 "pop r9"))

(define (generate-set-var ptr index)
  (write-mem-byte ptr       #x58) ; pop rax
  (write-mem-byte (+ 1 ptr) #x48) ; QWORD PTR [rbp+___],rax
  (write-mem-byte (+ 2 ptr) #x89)
  (write-mem-byte (+ 3 ptr) #x45)
  (write-mem-byte (+ 4 ptr) (- 0 (* (+ 1 index) 8)))
  )

(define (set-var-instruction index)
  (list 5 (lambda (ptr) (generate-set-var ptr index)) (list "set-var" index)))

(define save-frame-pointer-instruction
  (list 1 (lambda (ptr)
            (begin
              (write-mem-byte ptr       #x55) ; push rbp
              ))
        "save-frame-pointer"))

(define set-frame-pointer-instruction
  (list 3 (lambda (ptr)
            (begin
              (write-mem-byte (+ 0 ptr) #x48) ; mov rbp,rsp
              (write-mem-byte (+ 1 ptr) #x89)
              (write-mem-byte (+ 2 ptr) #xe5)))
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
        (lambda (ptr label-locator)
          (generate-jmp ptr (assert (label-locator target-label (list 'relative-i32 target-label (+ ptr 1) 4)) not-empty? "jmp target")))
        (list "jmp" target-label)))

; HACK
(define (tail-call-instruction target-label)
  (list 5
        (lambda (ptr label-locator)
          (generate-jmp ptr (+ 
                              ; assume that the first instruction of the function is save-frame-pointer-instruction and skip it
                              (first save-frame-pointer-instruction)
                              (assert (label-locator target-label (list 'relative-i32 target-label (+ ptr 1) 4)) not-empty? "jmp target"))))
        (list "tail-call" target-label)))

(define (generate-call ptr target)
  (let ((rel-target (- target (+ ptr 5))))
    (assert-stmt "rel-target <= i32-max-value" (<= rel-target i32-max-value))
    (assert-stmt "rel-target >= i32-min-value" (>= rel-target i32-min-value))
  (write-mem-byte ptr #xe8) ; call rel-target
  (write-mem-i32 (+ ptr 1) rel-target)
  ))

(define (call-instruction target-label)
  (list 5
        (lambda (ptr label-locator)
          (generate-call ptr (assert (label-locator target-label (list 'relative-i32 target-label (+ ptr 1) 4)) not-empty? "call target")))
        (list "call" target-label)))

(define indirect-call-instruction
  (list 3
        (lambda (ptr)
          (begin
            (write-mem-byte ptr        #x58) ; pop rax
            (write-mem-byte (+ 1 ptr)  #xff)
            (write-mem-byte (+ 2 ptr)  #xd0) ; call rax
            ))
        "indirect call"))

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
        (lambda (ptr label-locator)
          (generate-cond-jmp cond-code ptr (assert (label-locator target-label (list 'relative-i32 target-label (+ ptr 7) 4)) not-empty? "cond-jmp target")))
        (list "cond-jmp" cond-code target-label)))

(define (cond-jmp-instruction-gen cond-code)
  (lambda (label)
    (cond-jmp-instruction cond-code label)))

(define (push-label-pointer-instruction label)
  (list 11
        (lambda (ptr label-locator)
             (let ((label-ptr
                     (assert (label-locator label (list 'absolute-i64 label (+ ptr 2))) not-empty? "label pointer")))
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

(define-struct context ([proc-name symbol?]
                        [local-vars list?]
                        [label-list list?]
                        [globals-mapping list?]))

;(define (create-context local-vars label-list globals-mapping) (list local-vars label-list globals-mapping))
;(define (context-local-vars context) (first context))
;(define (context-label-list context) (second context))
;(define (context-globals-mapping context) (nth 2 context))

(define (find-upl-func func-list name)
  (assert
    (findf (lambda (l) (equal? (upl-func-name l) name)) func-list)
    (compose not false?)
    (list "no such label:" name)))

(define (compile-list expr rest context)
  (let ((local-vars (context-local-vars context))
        (label-list (context-label-list context)))
    (cond
      ((equal? 'function-pointer (car expr))
       (let ((name (second expr))
             (proc-label (upl-func-label (find-upl-func label-list name))))
         (append
           (list (push-label-pointer-instruction proc-label))
           rest)))
      ((equal? 'fcall (car expr))
       (let ((name (second expr))
             (proc (find-upl-func label-list name))
             (proc-args (cdr (cdr expr)))
             (proc-label (upl-func-label proc))
             (proc-arg-count (upl-func-arity proc)))
         (assert-stmt (list proc "is a func") (equal? 'func (upl-func-type proc)))
         (assert-stmt (list (second expr) "number of args") (= proc-arg-count (length proc-args)))
         (--- append
           (list (add-rsp-instruction -16))
           (flatmap (lambda (expr) (compile-expression expr context)) proc-args)
           (list
             (add-rsp-instruction (* 8 (+ 2 proc-arg-count)))
             (call-instruction proc-label)
             push-rax-instruction)
           rest)))
      ((equal? '-> (car expr))
       (let ([var (car (cdr expr))]
             [var-index (index-of local-vars var)]
             [global-var-label (alist-lookup (context-globals-mapping context) var)])
         (cond [(not-empty? var-index)
                (append (list (push-var-addr-instruction var-index))
                        rest)]
               [(not-empty? global-var-label)
                (append (list (push-label-pointer-instruction global-var-label))
                        rest)]
               [else (panic "ref: bad variable:" var)])))
      (else
        (let ((arg-count-and-inst (assert (alist-lookup symbol-to-instruction (car expr)) not-empty? (list "undefined operator:" expr))))
          (assert-stmt (list "arg count matches" expr) (= (length (cdr expr)) (car arg-count-and-inst)))
          (foldr
            (lambda (expr rest) (compile-expression-rec expr rest context))
            (cons (car (cdr arg-count-and-inst)) rest)
            (cdr expr)))))))

(define (compile-expression-rec expr rest context)
  (let ((local-vars (context-local-vars context))
        (var-index (index-of local-vars expr))
        (globals-mapping (context-globals-mapping context))
        (global-var-label (alist-lookup globals-mapping expr)))
    (cond
      ((not-empty? var-index) (cons (push-var-instruction var-index) rest))
      ((not-empty? global-var-label) (append
                                     (list
                                       (push-label-pointer-instruction global-var-label)
                                       get-i64-instruction)
                                     rest))
      ((number? expr) (cons (push-imm-instruction expr) rest))
      ((list? expr) (compile-list expr rest context))
      (else (panic "bad expression: " (list ">" expr "<" "context:" context))))))

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
                 (--- append
                   (compile-boolean-expression first-child context false-label #f)
                   (compile-boolean-expression second-child context label #t)
                   (list false-label)))
               (append
                 (compile-boolean-expression first-child context label #f)
                 (compile-boolean-expression second-child context label #f)))))
          ((equal? cond-type 'or)
           (let ((first-child (first cond-args))
                 (second-child (second cond-args)))
             (assert-stmt "or expression has 2 children" (= (length cond-args) 2))
             (if true-label?
                 (append
                   (compile-boolean-expression first-child context label #t)
                   (compile-boolean-expression second-child context label #t)
                   )
                 (let ((true-label (new-label)))
                   (--- append
                     (compile-boolean-expression first-child context true-label #t)
                     (compile-boolean-expression second-child context label #f)
                     (list true-label)
                     )))))

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
        (compile-expr (lambda (expr) (compile-expression expr context))))
    (cond ((equal? stmt-type 'i64:=)
           (begin
             (assert-stmt (list "u64:= statement has 2 parts" stmt) (equal? 2 (length stmt-args)))
             (--- append
                  (compile-expr (first stmt-args))
                  (compile-expr (second stmt-args))
                  (list set-i64-instruction))))
          ((equal? stmt-type 'u8:=)
           (begin
             (assert-stmt (list "u8:= statement has 2 parts" stmt) (equal? 2 (length stmt-args)))
             (--- append
               (compile-expr (first stmt-args))
               (compile-expr (second stmt-args))
               (list set-u8-instruction))))
          ((equal? stmt-type ':+=)
           (begin
             (assert-stmt (list ":+= statement has 2 parts" stmt) (equal? 2 (length stmt-args)))
             (let ((var (car stmt-args))
                   (var-index (index-of local-vars var))
                   (val-expr (car (cdr stmt-args))))
               (compile-statement '(:= ,var (+ ,var ,val-expr)) context))))
          ((equal? stmt-type ':-=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (val-expr (car (cdr stmt-args))))
             (compile-statement '(:= ,var (- ,var ,val-expr)) context)))
          ((equal? stmt-type ':=)
           (let ((var (car stmt-args))
                 (var-index (index-of local-vars var))
                 (globals-mapping (context-globals-mapping context))
                 (global-var-label (alist-lookup globals-mapping var))
                 (val-expr (car (cdr stmt-args))))
             (assert-stmt (list ":= has 2 args, not:" stmt-args) (= (length stmt-args) 2))
             (cond
               ((not-empty? var-index) (compile-expression-rec
                                         val-expr
                                         (list (set-var-instruction var-index))
                                         context))
               ((not-empty? global-var-label) (compile-expression-rec
                                              val-expr
                                              (list (set-i64-instruction-imm-label-ptr global-var-label))
                                              context))
               (else (panic "set-var: bad variable:" var)))
             ))
          ((equal? stmt-type 'call)
           (let ((name (second stmt))
                 (proc (find-upl-func label-list name))
                 (proc-args (drop stmt 2))
                 (proc-label (upl-func-label proc))
                 (proc-arg-count (upl-func-arity proc)))
             (assert-stmt (list proc "is a proc") (equal? 'proc (upl-func-type proc)))
             (assert-stmt (list (first stmt-args) "number of args") (= proc-arg-count (length proc-args)))
             (--- append
               (list (add-rsp-instruction -16))
               (flatmap compile-expr proc-args)
               (list
                 (add-rsp-instruction (* 8 (+ 2 proc-arg-count)))
                 (call-instruction proc-label)))))
          ((equal? stmt-type 'indirect-call)
           (let ((proc-ptr-expression (first stmt-args)))
             (append 
               (compile-expr proc-ptr-expression)
               (list indirect-call-instruction))))
          ((equal? stmt-type 'tail-call)
           (let ((name (second stmt))
                 (proc (find-upl-func label-list name))
                 (proc-args (drop stmt 2))
                 (proc-label (upl-func-label proc))
                 (proc-arg-count (upl-func-arity proc)))
             (assert-stmt (list proc "is a proc") (equal? 'proc (upl-func-type proc)))
             (assert-stmt (list (first stmt-args) "number of args") (= proc-arg-count (length proc-args)))
             (--- append
               (flatmap compile-expr proc-args)
               (map (lambda (index) (set-tail-call-arg-instruction proc-arg-count index)) (range proc-arg-count))
               (list
                 ; drop proc-arg-count + local var count 
                 (add-rsp-instruction (* 8 (--- + proc-arg-count (length local-vars))))
                 (tail-call-instruction proc-label)))))
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
               (compile-expr (first stmt-args))
               (list
                 pop-rax-instruction
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
               (--- append
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
             (--- append
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
             (--- append
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
               (--- append
                 (flatmap (lambda (branch)
                            (let ((condition (first branch))
                                  (statements (second branch))
                                  (false-label (new-label)))
                              (--- append
                                (compile-boolean-expression condition context false-label #f)
                                (flatmap compile-substatement statements)
                                (list (jmp-instruction end-label)
                                      false-label))))
                          c-branches)
                 (flatmap compile-substatement (second else-branch))
                 (list end-label)))))
          ((equal? stmt-type 'block)
           (let ((statements (first stmt-args)))
               (flatmap (lambda (s) (compile-statement s context)) statements)))
          ((equal? stmt-type 'drop)
           (append
             (compile-expr (first stmt-args))
             (list pop-rax-instruction)))
          (else (panic "bad statement" stmt)))))


(define (compile-procedure name
                           args
                           local-vars-with-inits
                           statements
                           upl-func-list
                           globals-mapping)
  (begin
    (assert-stmt (list "local vars must have init values, for example: (proc foo (a b) ((i 42)) ..., but was " local-vars-with-inits)
                 (andmap pair? local-vars-with-inits))
    (let ((local-var-inits (map (lambda (v) (car (cdr v))) local-vars-with-inits))
          (local-vars (append args (map car local-vars-with-inits))))
      (--- append
        (list
          save-frame-pointer-instruction
          set-frame-pointer-instruction
          (add-rsp-instruction (- 0 (* 8 (length args)))))
        (flatmap (lambda (var-init) (compile-expression var-init (create-context name args upl-func-list globals-mapping)))
                 local-var-inits)
        (flatmap (lambda (stmt) (compile-statement stmt (create-context name local-vars upl-func-list globals-mapping)))
                 statements)
        (list (add-rsp-instruction (* 8 (length local-vars)))
          return-instruction)))))

(define next-label-id (cell 0))

(define (new-label)
  (let ((id (cell-get next-label-id)))
    (cell-set next-label-id (+ id 1))
    id))

(define (label? v) (number? v))

(define-struct upl-func ((name symbol?)
                         (type symbol?)
                         (label label?)
                         (arity number?)))

(define (compile-upl code-chunks globals-mapping existing-upl-funcs)
  (let ((chunks-with-labels
          (map (lambda (p) (cons p (new-label))) code-chunks))
        (upl-func-list
          (map (lambda (c-with-l)
                 (let ((chunk (car c-with-l))
                       (label (cdr c-with-l))
                       (chunk-type (first chunk))
                       (name (second chunk)))
                   (cond
                     ((equal? 'proc chunk-type)
                      (let ((arg-count (length (nth 2 chunk))))
                        (create-upl-func name 'proc label arg-count)))
                     ((equal? 'func chunk-type)
                      (let ((arg-count (length (nth 2 chunk))))
                        (create-upl-func name 'func label arg-count)))
                     ((equal? 'bytes chunk-type)
                      (create-upl-func name 'bytes label -1))
                     (else (assert-stmt (list "chunk type" chunk-type) #f)))))
               chunks-with-labels))
        (all-upl-funcs (append upl-func-list existing-upl-funcs)))
    (list
      upl-func-list
      (flatmap
        (lambda (c-with-l)
          (let ((chunk (first c-with-l))
                (chunk-type (first chunk))
                (label (cdr c-with-l)))
            (cond
              ((or (equal? 'proc chunk-type) (equal? 'func chunk-type))
               (let ([name (nth 1 chunk)]
                     [args (nth 2 chunk)]
                     [local-vars-with-inits (nth 3 chunk)]
                     [statements (nth 4 chunk)])
                 (cons label (compile-procedure name args local-vars-with-inits statements all-upl-funcs globals-mapping))))
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
    (procedure? (car (cdr i)))))

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


; object code file
; external functions references list - to link call sites\function-pointers\tail call sites to the actual functions
; globals list - mapping from global variable name (symbol) to pointer offset
; internal constants list - for function-pointer handling
; list of defined functions
(define-struct upl-object-code
               (
                [external-globals list?] ; (global name (symbol) -> pointer offset) list
                [internal-constants list?] ; (pointer offset, value, value offset) list
                [external-func-references list?] ; (function name (symbol),  rel-i32 offset and stuff) list
                [func-list list?] ; functions list
                [code number?] ; pointer to code
                [code-size number?] ; code size
                ))

(define-struct compiled-upl-func ((name symbol?)
                                  (type symbol?)
                                  (ptr number?)
                                  (size (lambda (size) (and (number? size) (>= size 0))))
                                  (arity number?)))


(define (collect-strings x)
  (cond [(pair? x) (append (collect-strings (car x)) (collect-strings (cdr x)))]
        [(string? x) (list x)]
        [else '()]))

(define (compile-upl-to-object-code upl-code global-vars existing-upl-funcs)
  (let ([globals-labels (map (lambda (_) (new-label)) global-vars)]
        [globals-mapping (zip global-vars globals-labels)]
        [code-strings (collect-strings upl-code)]
        [_ (println (list "code strings" code-strings))]
        [upl-func-list-and-insts (compile-upl upl-code globals-mapping existing-upl-funcs)]
        [upl-func-list (first upl-func-list-and-insts)]
        [instructions (second upl-func-list-and-insts)]
        [_ [begin
             (print-instructions instructions)
             ]]
        [_ (validate-stack-machine-code instructions)]
        [inst-sizes (map instruction-size instructions)]
        [total-code-size (foldl + 0 inst-sizes)]
        ;[code-buffer (syscall-mmap-anon total-code-size)]
        [code-buffer (syscall-mmap-anon-exec total-code-size)]
        [inst-locations (prefix-sum code-buffer inst-sizes)]
        [with-locations (zip instructions inst-locations)]
        [insts-and-locations (filter (lambda (x) (not (label? (car x)))) with-locations)]
        [labels-and-locations 
                              ;(--- append
                                  ; globals-labels-locations
                                  ; existing-labels
                                   (filter (lambda (x) (label? (car x))) with-locations)
                                  ; )
        ]
        [_ [begin
             (println "code-buffer:")
             (println code-buffer)
             (println "labels:")
             (foreach println labels-and-locations)
             ]]
        [internal-labels-and-locations 
          (filter (lambda (x) (label? (car x))) with-locations)]
        [internal-labels (filter label? instructions)]
        [label-locator (lambda (label label-def)
                         [begin
                           (println label-def)
                           (alist-lookup labels-and-locations label)
                           ])
                         ])
    (foreach (lambda (i-and-loc)
               (let ((inst (car i-and-loc))
                     (loc (cdr i-and-loc))
                     (generator (second inst)))
                 (if (= (arity generator) 2)
                   (generator loc label-locator)
                   (generator loc))))
             insts-and-locations)

    (let ([resolved-funcs
            (map (lambda (upl-func)
                   (let ((name (upl-func-name upl-func))
                         (ptr (assert (alist-lookup labels-and-locations (upl-func-label upl-func)) not-empty? (list "label location lookup" upl-func)))
                         (arity (upl-func-arity  upl-func))
                         (type (upl-func-type upl-func)))
                     (list name ptr arity type)))
                 upl-func-list)]
          [last-ptr (+ code-buffer total-code-size)]
          [compiled-funcs
            (map (lambda (prev-next-pair)
                   (let ((prev (car prev-next-pair))
                         (next (cdr prev-next-pair))
                         (name (first prev))
                         (ptr (second prev))
                         (arity (nth 2 prev))
                         (type (nth 3 prev))
                         (next-ptr (second next)))
                     (create-compiled-upl-func name type ptr (- next-ptr ptr) arity)))
                 (zip
                   resolved-funcs
                   (append (cdr resolved-funcs) (list (list '() last-ptr)))))])

      (create-upl-object-code '() ; globals
                              '() ; internal constants
                              '()
                              compiled-funcs
                              code-buffer
                              total-code-size)
      ))
    )


(define (compile-upl-to-native upl-code globals-ptr-mapping compiled-func-list)
  (let ((existing-upl-func-and-ptrs
          (map (lambda (compiled-func)
                 (list
                   (create-upl-func
                     (compiled-upl-func-name compiled-func)
                     (compiled-upl-func-type compiled-func)
                     (new-label)
                     (compiled-upl-func-arity compiled-func))
                   (compiled-upl-func-ptr compiled-func)))
               compiled-func-list))
        (existing-upl-funcs (map first existing-upl-func-and-ptrs))
        (existing-labels
          (map
            (lambda (f-and-p)
              (let ((upl-func (first f-and-p))
                    (ptr (second f-and-p)))
                (cons (upl-func-label upl-func) ptr)))
            existing-upl-func-and-ptrs))
        [globals-labels (map (lambda (_) (new-label)) globals-ptr-mapping)]
        [globals-mapping (zip (map car globals-ptr-mapping) globals-labels)]
        [globals-labels-locations (zip globals-labels (map cdr globals-ptr-mapping))]
        (upl-func-list-and-insts (compile-upl upl-code globals-mapping existing-upl-funcs))
        (upl-func-list (first upl-func-list-and-insts))
        (instructions (second upl-func-list-and-insts))
        (_ (validate-stack-machine-code instructions))
        (inst-sizes (map instruction-size instructions))
        (total-code-size (foldl + 0 inst-sizes))
        (exec-buffer (syscall-mmap-anon-exec total-code-size))
        (inst-locations (prefix-sum exec-buffer inst-sizes))
        (with-locations (zip instructions inst-locations))
        (insts-and-locations (filter (lambda (x) (not (label? (car x)))) with-locations))
        (labels-and-locations (--- append
                                   globals-labels-locations
                                   existing-labels
                                   (filter (lambda (x) (label? (car x))) with-locations)
                                   ))
        [internal-labels (filter label? instructions)]
        [label-locator (lambda (label label-def) (alist-lookup labels-and-locations label)) ])
    (foreach (lambda (i-and-loc)
               (let ((inst (car i-and-loc))
                     (loc (cdr i-and-loc))
                     (generator (second inst)))
                 (if (= (arity generator) 2)
                   (generator loc label-locator)
                   (generator loc))))
             insts-and-locations)

    (let ((resolved-funcs
            (map (lambda (upl-func)
                   (let ((name (upl-func-name upl-func))
                         (ptr (assert (alist-lookup labels-and-locations (upl-func-label upl-func)) not-empty? (list "label location lookup" upl-func)))
                         (arity (upl-func-arity  upl-func))
                         (type (upl-func-type upl-func)))
                     (list name ptr arity type)))
                 upl-func-list))
          (last-ptr (+ exec-buffer total-code-size)))
      (map (lambda (prev-next-pair)
             (let ((prev (car prev-next-pair))
                   (next (cdr prev-next-pair))
                   (name (first prev))
                   (ptr (second prev))
                   (arity (nth 2 prev))
                   (type (nth 3 prev))
                   (next-ptr (second next)))
               (create-compiled-upl-func name type ptr (- next-ptr ptr) arity)))
           (zip
             resolved-funcs
             (append (cdr resolved-funcs) (list (list '() last-ptr))))))))


; returns (list string-ptr string-length)
(define upl-static-string
  (let ((ret-val-buffer (syscall-mmap-anon 100))
        (str-buffer-size 10000)
        (str-buffer (syscall-mmap-anon str-buffer-size))
        (bump-index (cell 0)))
    (lambda (str)
      (let ((str-length (string-length str))
            (original-bump-index (cell-get bump-index))
            (string-ptr (+ str-buffer (cell-get bump-index)))
            (_  (assert-stmt "static string buffer overflow"
                             (<= (+ string-ptr str-length) (+ str-buffer str-buffer-size)))))
        (string-to-native-buffer str string-ptr)
        (cell-set bump-index (+ str-length (cell-get bump-index)))
        (list string-ptr str-length)))))


(define (upl-print-static-string str)
  (let ((ss (upl-static-string str))
        (str-ptr (first ss))
        (str-len (second ss)))
    '(drop (syscall 1 1 ,str-ptr ,str-len 1 2 3))))

(define (upl-exit code)
  '(drop (syscall 60 ,code 1 2 3 4 5)))

(define (core-upl-code proc-dict-ptr tmp-4k-buffer)
  '(
    (proc swap-i64 (a-ptr b-ptr) ([tmp 0])
          [
           (:= tmp (i64@ a-ptr))
           (i64:= a-ptr (i64@ b-ptr))
           (i64:= b-ptr tmp)
           ])
    (func chk-syscall (call-code arg1 arg2 arg3 arg4 arg5 arg6) ((ret 0) (hack-8-byte-buf 0) (num-length 0))
          ((:= ret (syscall call-code arg1 arg2 arg3 arg4 arg5 arg6))
           (if (and (< ret 0) (> ret -4096))
             (,(upl-print-static-string "syscall error: ")
               (call number-to-string (-> hack-8-byte-buf) (- 0 ret))
               (call number-string-length (-> num-length) (- 0 ret))
               (:= ret (syscall 1 1 (-> hack-8-byte-buf) num-length 1 2 3))
               (call print-newline)
               (call print-stack-trace)
               ,(upl-exit 1)))
           (return-val ret)
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
    (proc syscall-munmap (ptr size) ((syscall-ret 0))
          (
           (:= syscall-ret (fcall chk-syscall ,syscall-munmap 
                    ptr
                    size
                    0 0 0 0
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
    (proc write-to-stdout (buf length) ((sys-ret 0) (written 0))
          ((while (> length written)
                  (
                   (:= sys-ret (fcall chk-syscall 1 1 (+ buf written) (- length written) 1 2 3))
                   (:+= written sys-ret)))))
    (proc print-newline () ()
          (
           (u8:= ,tmp-4k-buffer 10) ; newline
           (call write-to-stdout ,tmp-4k-buffer 1)
           ))
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
                       ((u8:= (+ buf i) (+ octet 87))) ((u8:= (+ buf i) (+ octet 48)))) (:+= i 1)))))
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
                                                         (proc-dict-size 0)
                                                         (dict-array 0)
                                                         (string 0)
                                                         (offset 0))
          (
           (:= dict-array (+ proc-dict 8))
           (:= proc-dict-size (i64@ proc-dict))
           ; skip records
           (:= offset dict-array)
           (while (and 
                    (< index proc-dict-size)
                    (or
                      (< iptr (i64@ offset))
                      (> iptr (+ (i64@ offset) (i64@ (+ offset 8))))))
                  (
                   (:+= index 1)
                   (:= offset (+ (* index 24) dict-array))
                   ))
           (if (< index proc-dict-size)
               (
                (:= string (i64@ (+ offset 16)))
                (i64:= buf-ptr (+ string 8))
                (i64:= size-ptr (i64@ string)))
               (
                (i64:= buf-ptr 0)))
           ))
    (bytes sigaction-restorer
           (
            #xb8 #x0f #x00 #x00 #x00 ; mov    eax,0xf ; sigreturn
            #x0f #x05                ; syscall
            ))
    
    (bytes sigsegv-handler
           (
            ; return address is on the top of the stack
            ; need to push a dummy value (saved fp placeholder)
            ; then push the argument and then restore the stack pointer
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
    ))


; dictionary layout:
; [dictionary size][dictionary array]
; dictionary array item layout:
; [function pointer][function size][function name string ptr]
; string layout:
; [size in bytes][bytes ...]

(define (create-proc-dict proc-dict-ptr func-list)
  (let ((dict-size (length func-list))
        (dict-item-size 24)
        (names (map (lambda (f) (symbol-to-string (compiled-upl-func-name f))) func-list))
        (name-lengths (map string-length names))
        (name-length-sum (foldl + 0 name-lengths))
        (dict-ptr (syscall-mmap-anon (--- + 8
                                          (* dict-item-size dict-size)
                                          (* 8 dict-size)
                                          name-length-sum)))
        (dict-array-ptr (+ dict-ptr 8))
        (names-array-ptr (+ dict-array-ptr (* dict-item-size dict-size)))
        (name-ptrs (prefix-sum names-array-ptr (map (lambda (l) (+ l 8)) name-lengths))))
    (write-mem-i64 dict-ptr dict-size)
    (foreach-with-index (lambda (p index)
                          (let ((offset (+ dict-array-ptr (* index dict-item-size)))
                                (upl-func (car p))
                                (name-ptr (cdr p)))
                            (write-mem-i64 offset (compiled-upl-func-ptr upl-func))
                            (write-mem-i64 (+ offset 8) (compiled-upl-func-size upl-func))
                            (write-mem-i64 (+ offset 16) name-ptr)))
                        (zip func-list name-ptrs))
    (foreach (lambda (string-and-ptr)
               (begin
                 (write-mem-i64 (cdr string-and-ptr) (string-length (car string-and-ptr)))
                 (string-to-native-buffer (car string-and-ptr) (+ (cdr string-and-ptr) 8))))
             (zip names name-ptrs))
    (write-mem-i64 proc-dict-ptr dict-ptr)))


(define-struct upl-compiler ([compile-next procedure?]
                             [run procedure?]
                             [global-value procedure?]
                             [get-functions procedure?]))

(define (new-upl-compiler globals)
  (let ([proc-dict-ptr (syscall-mmap-anon 4096)]
        [tmp-4k-buffer (syscall-mmap-anon 4096)]
        [globals-buffer (syscall-mmap-anon (* 8 (length globals)))]
        [globals-ptrs (map (lambda (i) (+ globals-buffer (* 8 i))) (range (length globals)))]
        [globals-mapping (zip globals globals-ptrs)]
        [core-functions
          (compile-upl-to-native (core-upl-code proc-dict-ptr tmp-4k-buffer) globals-mapping '())]
        [func-list-cell (cell core-functions)]
        [get-functions (lambda () (cell-get func-list-cell))]
        [compile-next (lambda (code)
                        (let ((new-funcs (compile-upl-to-native code globals-mapping (cell-get func-list-cell)))
                              (all-funcs (append new-funcs (cell-get func-list-cell))))
                          (cell-set func-list-cell all-funcs)
                          (create-proc-dict proc-dict-ptr all-funcs)
                          new-funcs))]
        [run (lambda (locals code)
               (let ((main-func-code 
                       '(
                         (proc main () ,(append '((syscall-ret 0)
                                                  (sigaction-struct (fcall syscall-mmap-anon 4096)) ; assume that k_sigaction struct size is less than 4k
                                                  (old-sigaction-struct (fcall syscall-mmap-anon 4096)))
                                                locals)
                               (
                                (i64:= (+ sigaction-struct 0 ) (function-pointer sigsegv-handler))
                                (i64:= (+ sigaction-struct 8 ) #x4000004) ; flags SA_SIGINFO | SA_RESTORER
                                (i64:= (+ sigaction-struct 16) (function-pointer sigaction-restorer))
                                (i64:= (+ sigaction-struct 24) 0) ; sig mask
                                (:= syscall-ret (fcall chk-syscall
                                                       13 ; sigaction
                                                       11 ; sigsegv signal
                                                       sigaction-struct
                                                       old-sigaction-struct
                                                       8 ; sig mask size
                                                       1 2))

                                (block ,code)

                                (:= syscall-ret (fcall chk-syscall
                                                       13 ; sigaction
                                                       11 ; sigsegv signal
                                                       old-sigaction-struct
                                                       0 ; old sigaction struct
                                                       8 ; sig mask size
                                                       1 2))
                                (call syscall-munmap sigaction-struct 4096)
                                (call syscall-munmap old-sigaction-struct 4096)
                                ))
                         ))
                     (compiled-func-list (compile-upl-to-native main-func-code globals-mapping (cell-get func-list-cell))))
                 (native-call (compiled-upl-func-ptr (first compiled-func-list)))))]
        [global-value (lambda (var) 
                        (read-mem-i64 (alist-lookup globals-mapping var)))])
    ; TODO: unmap exec memory?
    (create-upl-compiler compile-next run global-value get-functions)))

(define (upl-debug-print-hex-value msg value)
  (list 'block
        (list
          (upl-print-static-string msg)
          '(call print-hex-number ,value)
          '(call print-newline))))

