; dev notes

;   - lisp code compilation
;       - generate literals and put them to literal block list
;       - symbols
;           - an interned string
;           - binding
;               - with 'value' field that holds an object (global binding)
;               - dictionary - slow lookup in runtime
;       - generate code
;   - TCO
;       - upl

(import "upl-compiler.scm")

(define alloc-arena-size 409600)
(define alloc-arena (syscall-mmap-anon alloc-arena-size))
(define alloc-bump-ptr (syscall-mmap-anon 1000))
(write-mem-i64 alloc-bump-ptr alloc-arena)

;   object header layout [gc flags (1 byte)][type id (1 byte)]
(define (upl-obj-type-id expr)
  '(u8@ (+ 1 ,expr)))
(define (upl-obj-set-type-id obj obj-type)
  '(u8:= (+ 1 ,obj) ,obj-type))

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

(define procedure-obj-type-id 2)
;   [fptr][nargs][nfields][fields ...]
(define (upl-obj-procedure-fptr expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-procedure-nargs expr)
  '(i64@ (+ ,(+ 8 obj-header-size) ,expr)))
(define (upl-obj-procedure-nfields expr)
  '(i64@ (+ ,(+ 16 obj-header-size) ,expr)))
(define (upl-obj-procedure-field n expr)
  '(i64@ (+ (* ,n 8) (+ ,(+ 24 obj-header-size) ,expr))))


(define symbol-obj-type-id 3)
;   [name length][u8 is bound][binding][name bytes ...]
(define (upl-obj-symbol-name-length expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-symbol-set-name-length expr length)
  '(i64:= (+ ,obj-header-size ,expr) ,length))

(define (upl-obj-symbol-is-bound expr)
  '(u8@ (+ ,(+ 8 obj-header-size) ,expr)))
(define (upl-obj-symbol-set-is-bound expr is-bound)
  '(u8:= (+ ,(+ 8 obj-header-size) ,expr) ,is-bound))

(define (upl-obj-symbol-binding expr)
  '(i64@ (+ ,(+ 9 obj-header-size) ,expr)))
(define (upl-obj-symbol-set-binding expr binding)
  '(i64:= (+ ,(+ 9 obj-header-size) ,expr) ,binding))

(define (upl-obj-symbol-name-buf expr)
  '(+ ,(+ 17 obj-header-size) ,expr))

(define (upl-obj-symbol-struct-size name-size)
  '(+ ,(+ 17 obj-header-size) ,name-size))

(define lisp-globals
  '(
    lisp-stack-bottom
    lisp-stack-ptr
    lisp-stack-size
    literals-block-list
    symbol-block-list
    symbol-alloc-4k-buffer-ptr
    test
    ))

; block-list block layout
; [size][next][values]{510}
(define block-list-max-block-size (- (/ 4096 8) 2))

(define (upl-block-list-block-set-next block next-block)
  '(i64:= (+ ,block 8) ,next-block)
  )
(define (upl-block-list-block-next block)
  '(i64@ (+ ,block 8))
  )
(define (upl-block-list-block-set-size block size)
  '(i64:= ,block ,size)
  )
(define (upl-block-list-block-size block)
  '(i64@ ,block)
  )
(define (upl-block-list-block-item-ptr block index)
  '(+ ,block (+ 16 (* ,index 8)))
  )
(define (upl-block-list-block-set-item block index value)
  '(i64:= ,(upl-block-list-block-item-ptr block index) ,value)
  )
(define (upl-block-list-block-item block index)
  '(i64@ (+ ,block (+ 16 (* ,index 8))))
  )


(define (upl-lisp-symbol name)
  (let ([ss (upl-static-string name)])
    '(fcall get-or-create-symbol ,(first ss) ,(second ss))))

(define upl-code 
  '(
    (proc memcopy (src dest length) ((index 0))
          (
            (while (< index length)
                   (
                    (u8:= (+ dest index) (u8@ (+ src index)))
                    (:+= index 1)
                    ))
           ))
    (func memcmp (left right length) ((index 0))
          (
            (while (< index length)
                   (
                    (cond
                      [(< (u8@ (+ left index)) (u8@ (+ right index)))
                       ((return-val -1))]
                      [(> (u8@ (+ left index)) (u8@ (+ right index)))
                       ((return-val 1))]
                      [else ()])
                    (:+= index 1)
                    ))
            (return-val 0)
           ))
    (func create-block-list () ((block-ptr (fcall syscall-mmap-anon 4096)))
          (
           ,(upl-block-list-block-set-size 'block-ptr 0)
           ,(upl-block-list-block-set-next 'block-ptr 0)
           (return-val block-ptr)
           ))
    (func next-value-slot-in-block-list (ptr) ((block-ptr (i64@ ptr))
                                               (block-size 0))
          (
           (:= block-size ,(upl-block-list-block-size 'block-ptr))
           (if (>= ,(upl-block-list-block-size 'block-ptr) ,block-list-max-block-size)
             (
              (i64:= ptr (fcall create-block-list))
              ,(upl-block-list-block-set-next '(i64@ ptr) 'block-ptr)
              (:= block-ptr (i64@ ptr))
              (:= block-size 0)
              ))
           (:+= block-size 1)
           ,(upl-block-list-block-set-size 'block-ptr 'block-size)
           (return-val ,(upl-block-list-block-item-ptr 'block-ptr '(- block-size 1)))
           ))
    (proc add-item-to-block-list (ptr value) ((block-ptr (i64@ ptr)) (block-size 0))
          (
           (i64:= (fcall next-value-slot-in-block-list ptr) value)
           ))
    (proc print-block-list-values (ptr-to-block-list) ((index 0)
                                                       (block-ptr 0)
                                                       (block-size 0))
          (
           (:= block-ptr (i64@ ptr-to-block-list))
           (while (!= block-ptr 0)
                  (
                   (:= block-size ,(upl-block-list-block-size 'block-ptr))
                   (:= index (- block-size 1))
                   (while (>= index 0)
                          (
                           (call print-number ,(upl-block-list-block-item 'block-ptr 'index))
                           (call print-newline)
                           (:-= index 1)
                           ))
                   (:= block-ptr ,(upl-block-list-block-next 'block-ptr))
                   (:= index 0)
                   ))
           ))

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

    (proc init-lisp-stack () ()
          (
           (:= lisp-stack-size (* 10 4096))
           (:= lisp-stack-bottom (fcall syscall-mmap-anon lisp-stack-size))
           (:= lisp-stack-ptr lisp-stack-bottom)
           ))
    (proc init-symbol-allocator () ()
          (
           (:= symbol-alloc-4k-buffer-ptr (fcall syscall-mmap-anon 4096))
           ))
    (proc init-symbol-block-list () ()
          (
           (:= symbol-block-list (fcall create-block-list))
           ))
    (proc init-literals-block-list () ()
          (
           (:= literals-block-list (fcall create-block-list))
           ))

    (proc push-to-lisp-stack (obj) ()
          (
           (if (>= (+ 8 lisp-stack-ptr) (+ lisp-stack-bottom lisp-stack-size))
             (
              ,(upl-print-static-string "==== lisp stack overflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:+= lisp-stack-ptr 8)
           (i64:= lisp-stack-ptr obj)
           ))
    (func peek-lisp-stack (index) ()
          (
           (if (< (- lisp-stack-ptr (* 8 index)) lisp-stack-bottom)
             (
              ,(upl-print-static-string "==== lisp stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (return-val (i64@ (- lisp-stack-ptr (* 8 index))))
           ))
    (func pop-lisp-stack () ([value (fcall peek-lisp-stack 0)])
          (
           (call drop-lisp-stack 1)
           (return-val value)
           ))
    (proc drop-lisp-stack (n) ()
          (
           (if (< n 0)
             (
              ,(upl-print-static-string "n should be >=0 \n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (< (- lisp-stack-ptr (* 8 n)) lisp-stack-bottom)
             (
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:-= lisp-stack-ptr (* 8 n))
           ))

    (func allocate-i64 (num) ((addr 0))
          ((call gc-malloc (-> addr) 10)
           (u8:=  addr 0)
           (u8:=  (+ addr 1) ,i64-obj-type-id)
           (i64:= (+ addr 2) num)
           (return-val addr)))
    (func allocate-pair (first second) ((addr 0))
          ((call gc-malloc (-> addr) 18)
          (u8:=  addr 0)
          (u8:=  (+ addr 1) ,pair-obj-type-id)
          (i64:= (+ addr 2) first)
          (i64:= (+ addr 10) second)
          (return-val addr)))
    (func allocate-procedure (fptr nargs nfields) ((addr 0))
          (
           (if (< nargs 0)
             (
              ,(upl-print-static-string "nargs should be >= 0\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (< nfields 0)
             (
              ,(upl-print-static-string "nfields should be >= 0\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (call gc-malloc (-> addr) (+ (* 8 nfields) ,(+ 24 2)))
           (u8:=  addr 0)
           (u8:=  (+ addr 1) ,procedure-obj-type-id)
           (i64:= (+ addr 2) fptr)
           (i64:= (+ addr 10) nargs)
           (i64:= (+ addr 18) nfields)
           (return-val addr)))
    (func allocate-symbol (name-buf name-length) ((alloc-buf-ptr (% symbol-alloc-4k-buffer-ptr 4096))
                                                  (symbol-struct-size ,(upl-obj-symbol-struct-size 'name-length))
                                                  (symbol-ptr 0))
          (
           (if (> symbol-struct-size 4096)
             (
              ,(upl-print-static-string "symbol struct size is too large\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (>= (+ alloc-buf-ptr symbol-struct-size) 4096)
             (
               (call init-symbol-allocator) 
               (:= alloc-buf-ptr (% symbol-alloc-4k-buffer-ptr 4096))
              ))
           (:= symbol-ptr symbol-alloc-4k-buffer-ptr)
           (:+= symbol-alloc-4k-buffer-ptr symbol-struct-size)
           ,(upl-obj-set-type-id 'symbol-ptr symbol-obj-type-id)
           ,(upl-obj-symbol-set-name-length 'symbol-ptr 'name-length)
           ,(upl-obj-symbol-set-is-bound 'symbol-ptr 0)
           (call memcopy name-buf ,(upl-obj-symbol-name-buf 'symbol-ptr) name-length)
           (return-val symbol-ptr)
           ))
    (func get-or-create-symbol (name-buf name-length) ((block-ptr 0)
                                                       (block-size 0)
                                                       (index 0)
                                                       (symbol 0))
          (
           (:= block-ptr symbol-block-list)
           (while (!= block-ptr 0)
                  (
                   (:= block-size ,(upl-block-list-block-size 'block-ptr))
                   (:= index (- block-size 1))
                   (while (>= index 0)
                          (
                           (:= symbol ,(upl-block-list-block-item 'block-ptr 'index))
                           (if (and
                                 (= ,(upl-obj-symbol-name-length 'symbol) name-length)
                                 (= 0 (fcall memcmp ,(upl-obj-symbol-name-buf 'symbol) name-buf name-length)))
                             (
                              (return-val symbol)
                              ))
                           (:-= index 1)
                           ))
                   (:= block-ptr ,(upl-block-list-block-next 'block-ptr))
                   (:= index 0)
                   ))
           (:= symbol (fcall allocate-symbol name-buf name-length))

           (call add-item-to-block-list (-> symbol-block-list) symbol)
           (return-val symbol)
           ))

    (proc call-lisp-procedure () ((lisp-proc 0))
          (
           (:= lisp-proc (fcall peek-lisp-stack 0)) 
           (indirect-call ,(upl-obj-procedure-fptr 'lisp-proc))
           ))
    (proc push-number-to-lisp-stack (num) ()
          (
           (call push-to-lisp-stack (fcall allocate-i64 num))
           ))
    (proc lisp-define (symbol) ([value 0])
          (
           (:= value (fcall pop-lisp-stack))
           (if (!= 0 ,(upl-obj-symbol-is-bound 'symbol))
             (
              ,(upl-print-static-string "symbol ")
              (call print-object symbol)
              ,(upl-print-static-string " is already bound\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           ,(upl-obj-symbol-set-binding 'symbol 'value)
           ,(upl-obj-symbol-set-is-bound 'symbol 1)
    ))
    (proc push-symbol-bound-value (symbol) ()
          (
           (if (= 0 ,(upl-obj-symbol-is-bound 'symbol))
             (
              ,(upl-print-static-string "symbol ")
              (call print-object symbol)
              ,(upl-print-static-string " is not bound\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (call push-to-lisp-stack ,(upl-obj-symbol-binding 'symbol))
           ))
    (func allocate-number-literal (num) ([handle 0])
          [
           (:= handle (fcall next-value-slot-in-block-list (-> literals-block-list)))
           (i64:= handle (fcall allocate-i64 num))
           (return-val handle)
           ])
    (proc lisp-procedure-cons () ((pair 0))
          (
           ; stack state: [proc] [cdr] [car]
           (:= pair (fcall allocate-pair
                           (fcall peek-lisp-stack 2)
                           (fcall peek-lisp-stack 1)))
           (call drop-lisp-stack 3)
           (call push-to-lisp-stack pair)
           ))
    (proc lisp-cons () ((pair 0))
          (
           (:= pair (fcall allocate-pair
                           (fcall peek-lisp-stack 1)
                           (fcall peek-lisp-stack 0)))
           (call drop-lisp-stack 2)
           (call push-to-lisp-stack pair)
           ))
    (func zero-if-list (list) ((pair-ptr list))
          (
           (while (and (!= pair-ptr 0) (= ,pair-obj-type-id ,(upl-obj-type-id 'pair-ptr)))
                  (
                   (:= pair-ptr ,(upl-obj-pair-cdr 'pair-ptr))))
           (return-val pair-ptr)
           ))
    (proc print-obj-type-id-name (type-id) ()
          [
           (cond [(= type-id ,i64-obj-type-id) [,(upl-print-static-string "i64")]]
                 [(= type-id ,procedure-obj-type-id) [,(upl-print-static-string "procedure")]]
                 [(= type-id ,pair-obj-type-id) [,(upl-print-static-string "pair")]]
                 [else
                   [,(upl-print-static-string "unknown obj type: ")
                     (call print-number type-id)]])
           ])
    (proc check-lisp-type (obj type-id msg msg-length) ()
          [
           (if (or (= obj 0)
                   (!= ,(upl-obj-type-id 'obj) type-id))
             [
                (call write-to-stdout msg msg-length)
                ,(upl-print-static-string ": expected ")
                (call print-obj-type-id-name type-id)
                ,(upl-print-static-string ", but was ")
                (call print-object obj)
                (call print-newline)
                ,(upl-exit 1)
              ])
           ])
    (proc print-object (obj) ()
          (
           (if (or (= 0 (fcall zero-if-list obj))
                   (= ,symbol-obj-type-id ,(upl-obj-type-id 'obj)))
             (
              ,(upl-print-static-string "'")
              ))
           (call print-quoted-object obj)
           ))
    (proc print-quoted-object (obj) ((obj-type-id 0) (tmp 0))
          (
           (if (= obj 0)
             (
              ,(upl-print-static-string "()")
              (return)
              ))
           (:= obj-type-id ,(upl-obj-type-id 'obj))
           (cond
             ((= (fcall zero-if-list obj) 0)
              (
               (:= tmp obj)
               ,(upl-print-static-string "(")
               (while (!= tmp 0)
                      (
                       (call print-quoted-object ,(upl-obj-pair-car 'tmp))
                       (:= tmp ,(upl-obj-pair-cdr 'tmp))
                       (if (!= tmp 0)
                         (,(upl-print-static-string " "))
                         ())
                       ))
               ,(upl-print-static-string ")")
               ))
             ((= obj-type-id ,i64-obj-type-id)
              (
               (call print-number ,(upl-obj-i64-value 'obj))
               ))
             ((= obj-type-id ,pair-obj-type-id)
              (
               ,(upl-print-static-string "(")
               (call print-object ,(upl-obj-pair-car 'obj))
               ,(upl-print-static-string " . ")
               (call print-object ,(upl-obj-pair-cdr 'obj))
               ,(upl-print-static-string ")")
               ))
             [(= obj-type-id ,symbol-obj-type-id)
              (
               (call write-to-stdout ,(upl-obj-symbol-name-buf 'obj) ,(upl-obj-symbol-name-length 'obj))
               )]
             (else (
                    ,(upl-print-static-string "bad obj-type\n")
                    )))
           ))
    (proc init-globals () ()
          (
           (call init-lisp-stack)
           (call init-symbol-allocator)
           (call init-symbol-block-list)
           (call init-literals-block-list)
           ))
    (proc c () () ((i64:= 0 0)))
    (proc b () ((d 0)) ((call c) (:= d 0)))
    (proc a () ((d 0)) ((call b) (:= d 0)))

    (proc test-lisp () ((proc 0))
          {
           ;(block ,(compile-lisp-literal '(42 420 () 69)))

           (call push-number-to-lisp-stack 1)
           (call lisp-define ,(upl-lisp-symbol "a"))
           
           ,(upl-print-static-string "a is bound to ")
           (call print-object ,(upl-obj-symbol-binding (upl-lisp-symbol "a")))
           (call print-newline)

           (call push-number-to-lisp-stack 42)
           (call lisp-define ,(upl-lisp-symbol "b"))
           ,(upl-print-static-string "after define\n")

           ,(upl-print-static-string "symbol a: ")
           (call print-object ,(upl-lisp-symbol "a"))
           (call print-newline)
           (call push-symbol-bound-value ,(upl-lisp-symbol "a"))
           (call push-symbol-bound-value ,(upl-lisp-symbol "b"))
           ,(upl-print-static-string "after push\n")
           (:= proc (fcall allocate-procedure (function-pointer lisp-procedure-cons) 2 0))
           (call push-to-lisp-stack proc)
           (call call-lisp-procedure)
           ,(upl-print-static-string "after call\n")
           (call print-object (fcall peek-lisp-stack 0))
           (call print-newline)
           })
    (proc test-block-list () ((block-list 0) (index 0))
          (
           (:= block-list (fcall create-block-list))
           (while (< index 1000)
                  (
                   (call add-item-to-block-list (-> block-list) index)
                   (:+= index 1)
                   ))
           (call print-block-list-values (-> block-list))
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
           (call push-to-lisp-stack 999)
           (call drop-lisp-stack 0)
           (call print-number (fcall peek-lisp-stack 0))
           (call print-newline)
           ))
    ))

(define upl-compiler (new-upl-compiler lisp-globals))

((upl-compiler-compile-next upl-compiler) upl-code)
((upl-compiler-run upl-compiler) '() '( 
                                       (call init-globals)
                                       ))

(define (compile-native-func name local-vars body)
  (let ([funcs ((upl-compiler-compile-next upl-compiler) 
                '(
                  (proc ,name () ,local-vars ,body)
                  ))])
    (compiled-upl-func-ptr (first funcs))))

(define (compile-native-one-arg-func arg-name local-vars body)
  (let ([funcs ((upl-compiler-compile-next upl-compiler) 
                '(
                  (bytes native-arg
                         (
                          ; return address is on the top of the stack
                          ; need to push a dummy value (saved fp placeholder)
                          ; then push the argument and then restore the stack pointer
                          ; to match upl calling convention
                          #x50 #x50 ; push rax ; push rax
                          #x48 #x83 #xc4 #x10 ; add rsp, 16
                          ))
                  (proc __native-arg (,arg-name) ,local-vars ,body)
                  ))])
    (compiled-upl-func-ptr (first funcs))))

(define (make-ffi-one-arg-proc func-name)
  (let ([fptr (compile-native-one-arg-func 
    'arg 
    '()
    '(
      (call ,func-name arg)
      ))])
    (lambda (arg) (native-call-one-arg fptr arg))))
(define (make-ffi-one-arg-func func-name)
  (let ([fptr (compile-native-one-arg-func 
    'arg 
    '()
    '(
      (return-val (fcall ,func-name arg))
      ))])
    (lambda (arg) (native-call-one-arg fptr arg))))

(define ffi-get-or-create-symbol
  (let ([tmp-buffer-size 4096]
        [tmp-buffer (syscall-mmap-anon tmp-buffer-size)]
        [native-get-or-create-symbol
          (compile-native-one-arg-func 
            'str 
            '()
            '(
              (return-val (fcall get-or-create-symbol (+ 8 str) (i64@ str)))
              ))])
    (lambda (name) [begin
                     (assert-stmt "symbol name fits to the buffer" (>= tmp-buffer-size (+ 8 (string-length name))))
                     (write-mem-i64 tmp-buffer (string-length name))
                     (string-to-native-buffer name (+ 8 tmp-buffer))
                     (native-call-one-arg native-get-or-create-symbol tmp-buffer)])))

(define ffi-lisp-define 
  (let ([tmp-buffer (syscall-mmap-anon 4096)]
        [native-define
          (compile-native-one-arg-func 
            'arg-buf 
            '()
            '(
              (call push-to-lisp-stack (i64@ arg-buf)) ;  obj
              (call lisp-define (i64@ (+ arg-buf 8)))  ; symbol
              ))])
    (lambda (symbol-name obj) 
      (let ([symbol-ptr (ffi-get-or-create-symbol symbol-name)])
        (write-mem-i64 tmp-buffer obj)
        (write-mem-i64 (+ tmp-buffer 8) symbol-ptr)
        (native-call-one-arg native-define tmp-buffer)))))

(define ffi-lisp-define-procedure
  (let ([tmp-buffer (syscall-mmap-anon 4096)]
        [native-proc-define
          (compile-native-func 
            'anon-func
            '()
            '(
              (call push-to-lisp-stack (fcall allocate-procedure
                                              (i64@ (+ ,tmp-buffer 8))  ; fptr
                                              (i64@ (+ ,tmp-buffer 16)) ; number of args
                                              0))
              (call lisp-define (i64@ ,tmp-buffer)))  ; symbol
              )])
    (lambda (name number-of-args local-vars code) 
      (let ([name-str (symbol-to-string name)]
            [symbol-ptr (ffi-get-or-create-symbol name-str)]
            [fptr (compile-native-func name local-vars code)])
        (write-mem-i64 tmp-buffer (ffi-get-or-create-symbol name-str))
        (write-mem-i64 (+ tmp-buffer 8) fptr)
        (write-mem-i64 (+ tmp-buffer 16) number-of-args)
        (native-call native-proc-define)))))

(define ffi-print-object (make-ffi-one-arg-proc 'print-object))

(define ffi-allocate-number-literal (make-ffi-one-arg-func 'allocate-number-literal))
(define ffi-allocate-i64 (make-ffi-one-arg-func 'allocate-i64))

(define (upl-check-lisp-type obj type-id msg)
  (let ([ss (upl-static-string msg)])
    '(call check-lisp-type ,obj ,type-id ,(first ss) ,(second ss))))

; define arithmetic operations
(foreach 
  (lambda (op)
    (ffi-lisp-define-procedure op 2
                               '((left 0) (right 0) (obj 0))
                               '(
                                 (:= left  (fcall peek-lisp-stack 1))
                                 (:= right (fcall peek-lisp-stack 2))
                                 ,(upl-check-lisp-type 'left  i64-obj-type-id (--- string-concat "first arg of '"  (symbol-to-string op) "'"))
                                 ,(upl-check-lisp-type 'right i64-obj-type-id (--- string-concat "second arg of '" (symbol-to-string op) "'"))
                                 (call drop-lisp-stack 2)
                                 (call push-number-to-lisp-stack (,op ,(upl-obj-i64-value 'left) ,(upl-obj-i64-value 'right)))
                                 )))
  '(+ - * / %))

; to compile lisp expression:
;   - symbol: emit push bound value
;   - literal: emit push literal
;   - list:
;       - define
;       - quote
;       - if
;       - cond?
;       - let
;       - begin
;       - lambda
;       - default: compile items in reverse order and emit function call code at the end

(define (compile-lisp-expression expr local-vals)
  (cond
    [(symbol? expr) 
     '(
       (call push-symbol-bound-value ,(upl-lisp-symbol (symbol-to-string expr)))
       )]
    [(number? expr)
     '(
       (call push-to-lisp-stack (i64@ ,(ffi-allocate-number-literal expr)))
       )]
    [(list? expr)
     '(
       (block ,(flatmap (lambda (e) (compile-lisp-expression e local-vals))
                        (reverse expr)))
       (call call-lisp-procedure) ; TODO: check arg count
       )]
    [else (panic (list "bad obj: " obj))]))


(ffi-lisp-define "a" (ffi-allocate-i64 42))
(ffi-lisp-define "b" (ffi-allocate-i64 4242))

((upl-compiler-run upl-compiler) '() 
                                 '(
                                   ;(call tests)
                                   (block ,(compile-lisp-expression '(- b a) '()))

                                   (call print-object (fcall peek-lisp-stack 0))
                                   (call print-newline)
                                   ;(call test-block-list)
                                   ;(call a)
                                   ))

