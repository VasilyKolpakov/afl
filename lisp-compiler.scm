(import "upl-compiler.scm")

(define alloc-arena-size 409600)
(define alloc-arena (syscall-mmap-anon alloc-arena-size))
(define alloc-bump-ptr (syscall-mmap-anon 1000))
(write-mem-i64 alloc-bump-ptr alloc-arena)

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

(define function-obj-type-id 2)
(define (upl-obj-function-fptr expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-function-nargs expr)
  '(i64@ (+ ,(+ 8 obj-header-size) ,expr)))
(define (upl-obj-function-nfields expr)
  '(i64@ (+ ,(+ 16 obj-header-size) ,expr)))
(define (upl-obj-function-field n expr)
  '(i64@ (+ (* ,n 8) (+ ,(+ 24 obj-header-size) ,expr))))

(define (compile-lisp-literal obj)
  (cond
    ((empty? obj) '((call push-to-lisp-stack 0)))
    ((number? obj) '((call push-number-to-lisp-stack ,obj)))
    ((pair? obj) (--- append
                   (compile-lisp-literal (car obj))
                   (compile-lisp-literal (cdr obj))
                   '((call lisp-cons))))
    (else (panic (list "bad obj: " obj)))))

(define upl-globals
  '(
    lisp-stack-bottom
    lisp-stack-ptr
    lisp-stack-size
     ))

(define upl-code 
  '(
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
           (:= lisp-stack-ptr (- lisp-stack-bottom 8))
           ))
    (proc push-to-lisp-stack (obj) ()
          (
           (if (>= (+ 8 lisp-stack-ptr) (+ lisp-stack-bottom lisp-stack-size))
             (
              ,(upl-print-static-string "lisp stack overflow\n")
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
              ,(upl-print-static-string "lisp stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (return-val (i64@ (- lisp-stack-ptr (* 8 index))))
           ))
    (proc drop-lisp-stack (n) ()
          (
           (if (< n 0)
             (
              ,(upl-print-static-string "n should be >=0 \n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (< (- lisp-stack-ptr (* 8 n)) (- lisp-stack-bottom 8))
             (
              ,(upl-print-static-string "lisp stack underflow\n")
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
    (func allocate-function (fptr nargs nfields) ((addr 0))
          (
           (if (< nargs 0)
             (
              ,(upl-print-static-string "nargs should be >= 0")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (< nfields 0)
             (
              ,(upl-print-static-string "nfields should be >= 0")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (call gc-malloc (-> addr) (+ (* 8 nfields) ,(+ 24 2)))
           (u8:=  addr 0)
           (u8:=  (+ addr 1) ,function-obj-type-id)
           (i64:= (+ addr 2) fptr)
           (i64:= (+ addr 10) nargs)
           (i64:= (+ addr 18) nfields)
           (return-val addr)))

    (proc push-number-to-lisp-stack (num) ()
          (
           (call push-to-lisp-stack (fcall allocate-i64 num))
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
    (proc print-object (obj) ()
          (
           (if (= 0 (fcall zero-if-list obj))
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
             (else (
                    ,(upl-print-static-string "bad obj-type\n")
                    )))
           ))
    (proc c () () ((i64:= 0 0)))
    (proc b () ((d 0)) ((call c) (:= d 0)))
    (proc a () ((d 0)) ((call b) (:= d 0)))

    (bytes sigaction-restorer
           (
            #xb8 #x0f #x00 #x00 #x00 ; mov    eax,0xf ; sigreturn
            #x0f #x05                ; syscall
            ))
    
    (bytes sigsegv-handler
           (
            ; return address is on the top of the stack
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
    (proc test-print-object () ()
          (
           (block ,(compile-lisp-literal '(42 420 () 69)))
           (call print-object (fcall peek-lisp-stack 0))
           (call print-newline)
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

(define upl-compiler (new-upl-compiler upl-globals))

; TODO: native proc-dict 
((upl-compiler-compile-next upl-compiler) upl-code)
((upl-compiler-run upl-compiler) '() '(
                                       ;,(upl-exit 42)
                                       (call init-lisp-stack)
                                       ;(call print-newline)
                                       (call test-print-object)

                                       ,(upl-print-static-string "end\n")
                                       (call tests)

                                        (call a)
                                       ))

