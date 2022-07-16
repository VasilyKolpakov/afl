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
(define obj-header-size 2)

(define (upl-obj-type-id expr)
  '(u8@ (+ 1 ,expr)))
(define (upl-obj-set-type-id obj obj-type)
  '(u8:= (+ 1 ,obj) ,obj-type))

(define (upl-obj-is-moved expr)
  '(u8@ ,expr))
(define (upl-obj-set-is-moved obj expr)
  '(u8:= ,obj ,expr))

(define (upl-obj-moved-ref expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-set-moved-ref obj expr)
  '(i64:= (+ ,obj-header-size ,obj) ,expr))

(define nil-obj-type-id -1)
(define i64-obj-type-id 0)
;   [value: i64]
(define (upl-obj-i64-value expr)
  '(i64@ (+ ,obj-header-size ,expr)))

(define pair-obj-type-id 1)
;   [car: ref][cdr: ref]
(define (upl-obj-pair-car expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-pair-car-ptr expr)
  '(+ ,obj-header-size ,expr))
(define (upl-obj-pair-cdr expr)
  '(i64@ (+ ,(+ 8 obj-header-size) ,expr)))
(define (upl-obj-pair-cdr-ptr expr)
  '(+ ,(+ 8 obj-header-size) ,expr))

(define procedure-obj-type-id 2)
;   [fptr][nargs][nfields][fields ...]
(define (upl-obj-procedure-fptr expr)
  '(i64@ (+ ,obj-header-size ,expr)))
(define (upl-obj-procedure-nargs expr)
  '(i64@ (+ ,(+ 8 obj-header-size) ,expr)))
(define (upl-obj-procedure-nfields expr)
  '(i64@ (+ ,(+ 16 obj-header-size) ,expr)))
(define (upl-obj-procedure-field expr n)
  '(i64@ (+ (* ,n 8) (+ ,(+ 24 obj-header-size) ,expr))))
(define (upl-obj-procedure-field-ptr expr n)
  '(+ (* ,n 8) (+ ,(+ 24 obj-header-size) ,expr)))
(define (upl-obj-procedure-set-field expr n val-expr)
  '(i64:= (+ (* ,n 8) (+ ,(+ 24 obj-header-size) ,expr)) ,val-expr))


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
(define (upl-obj-symbol-binding-ptr expr)
  '(+ ,(+ 9 obj-header-size) ,expr))
(define (upl-obj-symbol-set-binding expr binding)
  '(i64:= (+ ,(+ 9 obj-header-size) ,expr) ,binding))

(define (upl-obj-symbol-name-buf expr)
  '(+ ,(+ 17 obj-header-size) ,expr))

(define (upl-obj-symbol-struct-size name-size)
  '(+ ,(+ 17 obj-header-size) ,name-size))

(define boolean-obj-type-id 4)
(define boolean-obj-true-value -1)
(define boolean-obj-false-value -2)

(define lisp-globals
  '(
    lisp-stack-bottom
    lisp-stack-ptr
    lisp-stack-capacity

    lisp-local-val-stack-bottom
    lisp-local-val-stack-ptr
    lisp-local-val-stack-capacity

    literals-block-list
    symbol-block-list
    symbol-alloc-4k-buffer-ptr

    gc-command-stack-bottom
    gc-command-stack-ptr
    gc-command-stack-capacity

    gc-active-arena-ptr
    gc-active-arena-size
    gc-inactive-arena-ptr
    gc-inactive-arena-size

    gc-alloc-bump-ptr

    run-gc-recursive-guard
    ))

(define gc-command-stack-item-size (+ 1 8))
(define gc-command-move-op-code 1)
(define gc-command-update-ref-op-code 2)

(define (upl-gccsi-op-code item-ptr-expr)
  '(u8@ ,item-ptr-expr))

(define (upl-gccsi-op-arg item-ptr-expr)
  '(i64@ (+ 1 ,item-ptr-expr)))

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

(define (upl-check-lisp-type obj type-id msg)
  (let ([ss (upl-static-string msg)])
    '(call check-lisp-type ,obj ,type-id ,(first ss) ,(second ss))))

(define upl-code 
  '(
    (proc memzero (buf length) ([index 0]
                                [word-length (* (/ length 8) 8)])
          [
           (while (< index word-length)
                  (
                   (i64:= (+ buf index) 0)
                   (:+= index 8)
                   ))
           (while (< index length)
                  (
                   (u8:= (+ buf index) 0)
                   (:+= index 1)
                   ))
           ])
    (proc memcopy (src dest length) ([index 0]
                                     [word-length (* (/ length 8) 8)])
          [
           (while (< index word-length)
                  (
                   (i64:= (+ dest index) (i64@ (+ src index)))
                   (:+= index 8)
                   ))
           (while (< index length)
                  (
                   (u8:= (+ dest index) (u8@ (+ src index)))
                   (:+= index 1)
                   ))
           ])
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

    (func gc-malloc (size) ([obj 0])
          (
           (if (> (+ gc-alloc-bump-ptr size) (+ gc-active-arena-ptr gc-active-arena-size))
               (
                ,(upl-print-static-string "gc malloc arena overflow\n")
                ,(upl-exit 1)
                ) ())
           (:= obj gc-alloc-bump-ptr)
           ; TODO: move this check to gc-active-arena update code
           (if (or (= obj -1) (= obj -2))
             [
                ,(upl-print-static-string "gc-malloc returned -1 or -2 (those addressed are reseved for boolean values)\n")
                ,(upl-exit 1)
              ])
           (:+= gc-alloc-bump-ptr size)
           (return-val obj)
           ))

    (proc init-lisp-stack () ()
          (
           (:= lisp-stack-capacity (* 10 4096))
           (:= lisp-stack-bottom (fcall syscall-mmap-anon lisp-stack-capacity))
           (:= lisp-stack-ptr (- lisp-stack-bottom 8))
           ))
    (proc init-lisp-local-val-stack () ()
          (
           (:= lisp-local-val-stack-capacity (* 10 4096))
           (:= lisp-local-val-stack-bottom (fcall syscall-mmap-anon lisp-local-val-stack-capacity))
           (:= lisp-local-val-stack-ptr (- lisp-local-val-stack-bottom 8))
           ))
    (proc init-gc-command-stack () ()
          (
           (:= run-gc-recursive-guard 0)
           (:= gc-command-stack-capacity (* 1000 4096))
           (:= gc-command-stack-bottom (fcall syscall-mmap-anon gc-command-stack-capacity))
           (:= gc-command-stack-ptr (- gc-command-stack-bottom 8))
           ))
    (proc init-gc () ([arena-size 4096000])
          (
           (:= gc-active-arena-size arena-size)
           (:= gc-active-arena-ptr (fcall syscall-mmap-anon arena-size))
           (:= gc-alloc-bump-ptr gc-active-arena-ptr)

           (:= gc-inactive-arena-size arena-size)
           (:= gc-inactive-arena-ptr (fcall syscall-mmap-anon arena-size))
           (call init-gc-command-stack)
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
           (if (>= (+ 8 lisp-stack-ptr) (+ lisp-stack-bottom lisp-stack-capacity))
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
           (if (< (- lisp-stack-ptr (* 8 index)) (- lisp-stack-bottom 8))
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
           (if (< (- lisp-stack-ptr (* 8 n)) (- lisp-stack-bottom 8))
             (
              ,(upl-print-static-string "lisp stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:-= lisp-stack-ptr (* 8 n))
           ))

    (proc push-to-lisp-local-val-stack () ([obj (fcall pop-lisp-stack)])
          (
           (if (>= (+ 8 lisp-local-val-stack-ptr) (+ lisp-local-val-stack-bottom lisp-local-val-stack-capacity))
             (
              ,(upl-print-static-string "==== lisp val stack overflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:+= lisp-local-val-stack-ptr 8)
           (i64:= lisp-local-val-stack-ptr obj)
           ))
    (func peek-lisp-local-val-stack (index) ()
          (
           (if (< (- lisp-local-val-stack-ptr (* 8 index)) (- lisp-local-val-stack-bottom 8))
             (
              ,(upl-print-static-string "==== lisp local val stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (return-val (i64@ (- lisp-local-val-stack-ptr (* 8 index))))
           ))
    (proc drop-lisp-local-val-stack (n) ()
          (
           (if (< n 0)
             (
              ,(upl-print-static-string "n should be >=0 \n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (if (< (- lisp-local-val-stack-ptr (* 8 n)) (- lisp-local-val-stack-bottom 8))
             (
              ,(upl-print-static-string "lisp local-val stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:-= lisp-local-val-stack-ptr (* 8 n))
           ))

    (proc push-to-gc-command-stack (op-code arg) ()
          (
           (if (>= (+ ,gc-command-stack-item-size gc-command-stack-ptr)
                   (+ gc-command-stack-bottom gc-command-stack-capacity))
             (
              ,(upl-print-static-string "==== gc command stack overflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (:+= gc-command-stack-ptr 8)
           (i64:= gc-command-stack-ptr op-code)
           (:+= gc-command-stack-ptr 8)
           (i64:= gc-command-stack-ptr arg)
           ))
    (proc pop-from-gc-command-stack (op-code-ptr arg-ptr) ()
          (
           (if (< (- gc-command-stack-ptr ,gc-command-stack-item-size) (- 8 gc-command-stack-bottom))
             (
              ,(upl-print-static-string "==== gc command stack underflow\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ))
           (i64:= arg-ptr (i64@ gc-command-stack-ptr))
           (:-= gc-command-stack-ptr 8)
           (i64:= op-code-ptr (i64@ gc-command-stack-ptr))
           (:-= gc-command-stack-ptr 8)
           ))
    (func gc-command-stack-size () ()
          (
           (return-val (/ (- gc-command-stack-ptr gc-command-stack-bottom)
                          ,gc-command-stack-item-size))
           ))
    (proc push-ref-to-gc-stack (ptr-to-obj-ptr) ([obj (i64@ ptr-to-obj-ptr)])
          [
           ;,(upl-print-static-string "push-ref-to-gc-stack(") (call print-object obj) ,(upl-print-static-string ")\n")
           ; if the object is in the new space, then do nothing
           (if (and (>= obj gc-active-arena-ptr)
                    (< obj (+ gc-active-arena-ptr gc-active-arena-size)))
             [
              ,(upl-print-static-string "obj is in the new space\n")
              (return)
              ])
           (cond 
             [(and (= ,symbol-obj-type-id (fcall obj-type-id obj))
                   (= 1 ,(upl-obj-symbol-is-bound 'obj)))
              [
               (call push-ref-to-gc-stack ,(upl-obj-symbol-binding-ptr 'obj))
               ]]
             [(= 0 (fcall zero-if-heap-object obj))
              [
               (call push-to-gc-command-stack ,gc-command-update-ref-op-code ptr-to-obj-ptr)
               (call push-to-gc-command-stack ,gc-command-move-op-code obj)
               ]]
             [else []]
             )
           ])
    (proc gc-move-obj (obj) ([type-id 0]
                             [moved-obj 0]
                             [index 0])
          [
           (if (= 1 ,(upl-obj-is-moved 'obj))
             [
              (return)
              ])
           (:= type-id (fcall obj-type-id obj))
           (cond
             [(= ,i64-obj-type-id type-id)
              [
               (:= moved-obj (fcall allocate-i64 ,(upl-obj-i64-value 'obj)))
               ]]
             [(= ,pair-obj-type-id type-id)
              [
               (:= moved-obj (fcall allocate-pair
                              ,(upl-obj-pair-car 'obj)
                              ,(upl-obj-pair-cdr 'obj)))
               (call push-ref-to-gc-stack ,(upl-obj-pair-car-ptr 'moved-obj))
               (call push-ref-to-gc-stack ,(upl-obj-pair-cdr-ptr 'moved-obj))
               ]]
             [(= ,procedure-obj-type-id type-id)
              [
               (:= moved-obj (fcall allocate-procedure
                                    ,(upl-obj-procedure-fptr 'obj)
                                    ,(upl-obj-procedure-nargs 'obj)
                                    ,(upl-obj-procedure-nfields 'obj)))
               
               (:= index 0)
               (while (< index ,(upl-obj-procedure-nfields 'moved-obj))
                      [
                       (call push-ref-to-gc-stack ,(upl-obj-procedure-field-ptr 'moved-obj 'index))
                       ])
               ]]
             [else [
                    ,(upl-print-static-string "bad obj type: ")
                    (call print-number type-id)
                    (call print-newline)
                    (call print-stack-trace)
                    ,(upl-exit 1)
                    ]]
             )
           ,(upl-obj-set-is-moved 'obj 1)
           ,(upl-obj-set-moved-ref 'obj 'moved-obj)
           ])

    (proc run-gc () (
                     [op-code 0]
                     [op-arg 0]
                     [ptr-to-obj-ptr 0]
                     [block-ptr 0]
                     [block-size 0]
                     [index 0]
                     [type-id 0]
                     [obj 0]
                     [sym 0]
                     )
          [
           (if (= run-gc-recursive-guard 1)
             [
              ,(upl-print-static-string "error: recursive run-gc call\n")
              (call print-stack-trace)
              ,(upl-exit 1)
              ])
           (:= run-gc-recursive-guard 1)
           ; swap allocation arenas
           (call swap-i64 (-> gc-active-arena-ptr) (-> gc-inactive-arena-ptr))
           (call swap-i64 (-> gc-active-arena-size) (-> gc-inactive-arena-size))
           ; push gc roots to the stack
           ; push temp values
           (:= ptr-to-obj-ptr lisp-stack-bottom)
           (while (<= ptr-to-obj-ptr lisp-stack-ptr)
                  [
                   (call push-ref-to-gc-stack ptr-to-obj-ptr)
                   (:+= ptr-to-obj-ptr 8)
                   ])
           ; push local values
           (:= ptr-to-obj-ptr lisp-local-val-stack-bottom)
           (while (<= ptr-to-obj-ptr lisp-local-val-stack-ptr)
                  [
                   (call push-ref-to-gc-stack ptr-to-obj-ptr)
                   (:+= ptr-to-obj-ptr 8)
                   ])
           ; push literals
           (:= block-ptr literals-block-list)
           (while (!= block-ptr 0)
                  (
                   (:= block-size ,(upl-block-list-block-size 'block-ptr))
                   (:= index (- block-size 1))
                   (while (>= index 0)
                          (
                           (call push-ref-to-gc-stack ,(upl-block-list-block-item-ptr 'block-ptr 'index))
                           (:-= index 1)
                           ))
                   (:= block-ptr ,(upl-block-list-block-next 'block-ptr))
                   (:= index 0)
                   ))
           ; push symbol-bound values
           (:= block-ptr symbol-block-list)
           (while (!= block-ptr 0)
                  (
                   (:= block-size ,(upl-block-list-block-size 'block-ptr))
                   (:= index (- block-size 1))
                   (while (>= index 0)
                          (
                           (:= sym ,(upl-block-list-block-item 'block-ptr 'index))
                           (call push-ref-to-gc-stack ,(upl-obj-symbol-binding-ptr 'sym))
                           (:-= index 1)
                           ))
                   (:= block-ptr ,(upl-block-list-block-next 'block-ptr))
                   (:= index 0)
                   ))
           ; copy objects
           ,(upl-print-static-string "gc command stack size: ")
           (call print-number (fcall gc-command-stack-size))
           (call print-newline)
           (:= gc-alloc-bump-ptr gc-active-arena-ptr)
           (while (> (fcall gc-command-stack-size) 0)
                  [
                   ;,(upl-print-static-string "in while\n")
                   (call pop-from-gc-command-stack (-> op-code) (-> op-arg))
                   (cond
                     [(= ,gc-command-move-op-code op-code)
                      [
                       ;,(upl-print-static-string "moving object ") (call print-object op-arg) (call print-newline)
                       (call gc-move-obj op-arg)
                       ]]
                     [(= ,gc-command-update-ref-op-code op-code)
                      [
                       (:= obj (i64@ op-arg))
                       (if (not (and (>= obj gc-active-arena-ptr)
                                (< obj (+ gc-active-arena-ptr gc-active-arena-size))))
                         [
                          (if (= 0 ,(upl-obj-is-moved 'obj))
                            [
                             ,(upl-print-static-string "gc panic: updating ref of a non-moved object\n")
                             ,(upl-exit 1)
                             ])
                          (i64:= op-arg ,(upl-obj-moved-ref 'obj))
                          ])
                       ]]
                     [else 
                       (
                        ,(upl-print-static-string "bad gc command stack op-code: ")
                        (call print-number op-arg)
                        (call print-newline)
                        (call print-stack-trace)
                        ,(upl-exit 1)
                        )]
                     )
                   ])
           (:= run-gc-recursive-guard 0)
           (:= index 0)
           ; TODO remove memzero
           (call memzero gc-inactive-arena-ptr gc-inactive-arena-size)
           ])
    ; gc algorithm
    ;   - gc stack contains commands
    ;       - 'move object command (obj ref arg)' - moves the specified object to the new space
    ;       - 'update object ref (pointer to obj's pointer (another obj field or root slot)' - updates the object field
    ;   - allocate new object space
    ;   - put gc roots to the command stack ('move' and 'update-ref' commands)
    ;       - value stack
    ;       - local value stack
    ;       - literals
    ;   move all objects
    ;   - while there are commands on the stack
    ;       - pop one command from the stack
    ;           - 'move'
    ;               - if the object's 'move' flag is not set, then
    ;                   - copy the object to the new space
    ;                   - set 'moved' flag in the original object
    ;                   - put new address to the first field of the object in the old space
    ;                   - push 'update refs' commands to the command stack
    ;                       - object's refs that are not in the new space
    ;                   - push 'move' commands to the command stack
    ;                       - object's refs that are not in the new space
    ;           - 'update object ref'
    ;               - if object ref is not in the new space yet, then
    ;                   - update the ref

    (func allocate-i64 (num) ((addr 0))
          (
           (:= addr (fcall gc-malloc 10))
           (u8:=  addr 0) ;moved flag
           (u8:=  (+ addr 1) ,i64-obj-type-id)
           (i64:= (+ addr 2) num)
           (return-val addr)))
    (func allocate-pair (first second) ((addr 0))
          (
           (:= addr (fcall gc-malloc 18))
           (u8:=  addr 0)
           (u8:=  (+ addr 1) ,pair-obj-type-id)
           (i64:= (+ addr 2) first)
           (i64:= (+ addr 10) second)
           (return-val addr)
           ))
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
           (:= addr (fcall gc-malloc (+ (* 8 nfields) ,(+ 24 2))))
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
    (func zero-if-heap-object (obj) ([type-id (fcall obj-type-id obj)])
          [
           (cond
             [(= type-id ,nil-obj-type-id) [(return-val 1)]]
             [(= type-id ,boolean-obj-type-id) [(return-val 1)]]
             [(= type-id ,symbol-obj-type-id) [(return-val 1)]]
             [else [(return-val 0)]])
           ])

    (proc call-lisp-procedure (nargs) ((lisp-proc 0))
          (
           (call print-lisp-stack)
           (:= lisp-proc (fcall peek-lisp-stack 0)) 
           ,(upl-check-lisp-type 'lisp-proc procedure-obj-type-id "trying to call a non-procedure object")
           (if (!= nargs ,(upl-obj-procedure-nargs 'lisp-proc))
             [
              ,(upl-print-static-string "lisp procedure ")
              (call print-object lisp-proc)
              ,(upl-print-static-string "takes ")
              (call print-number ,(upl-obj-procedure-nargs 'lisp-proc))
              ,(upl-print-static-string "arguments, not ")
              (call print-number nargs)
              (call print-newline)
              (call print-stack-trace)
              ,(upl-exit 1)
              ])
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
    (func allocate-literal-from-stack () ([handle 0])
          [
           (:= handle (fcall next-value-slot-in-block-list (-> literals-block-list)))
           (i64:= handle (fcall pop-lisp-stack))
           (return-val handle)
           ])
    (proc lisp-procedure-cons () ((pair 0))
          (
           ; stack state: [proc] [car] [cdr]
           (:= pair (fcall allocate-pair
                           (fcall peek-lisp-stack 1)
                           (fcall peek-lisp-stack 2)))
           (call drop-lisp-stack 3)
           (call push-to-lisp-stack pair)
           ))
    (func zero-if-list (list) ((pair-ptr list))
          (
           (while (and (!= pair-ptr 0) (= ,pair-obj-type-id (fcall obj-type-id pair-ptr)))
                  (
                   (:= pair-ptr ,(upl-obj-pair-cdr 'pair-ptr))))
           (return-val pair-ptr)
           ))
    (func obj-type-id (obj) ()
          [
           (cond
             [(= obj 0) [(return-val ,nil-obj-type-id)]]
             [(or (= obj -1) (= obj -2)) [(return-val ,boolean-obj-type-id)]]
             [else [(return-val ,(upl-obj-type-id 'obj))]])
           ])
    (proc print-obj-type-id-name (type-id) ()
          [
           (cond
             [(= type-id ,nil-obj-type-id) [,(upl-print-static-string "nil-type")]]
             [(= type-id ,boolean-obj-type-id) [,(upl-print-static-string "boolean")]]
             [(= type-id ,i64-obj-type-id) [,(upl-print-static-string "i64")]]
             [(= type-id ,procedure-obj-type-id) [,(upl-print-static-string "procedure")]]
             [(= type-id ,pair-obj-type-id) [,(upl-print-static-string "pair")]]
             [else
               [,(upl-print-static-string "unknown obj type: ")
                 (call print-number type-id)]])
           ])
    (proc check-lisp-type (obj type-id msg msg-length) ()
          [
           (if (!= (fcall obj-type-id obj) type-id)
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
                   (= ,symbol-obj-type-id (fcall obj-type-id obj)))
             (
              ,(upl-print-static-string "'")
              ))
           (call print-quoted-object obj)
           ))
    (proc print-quoted-object (obj) ((obj-type-id 0) (tmp 0))
          (
           (cond
             [(= obj 0)
              (
               ,(upl-print-static-string "()")
               (return)
               )]
             [(= obj -1)
              (
               ,(upl-print-static-string "#t")
               (return)
               )]
             [(= obj -2)
              (
               ,(upl-print-static-string "#f")
               (return)
               )]
             [else ()])
           (:= obj-type-id (fcall obj-type-id obj))
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
             [(= obj-type-id ,procedure-obj-type-id)
              (
               ,(upl-print-static-string "<procedure>")
               )]
             [(= obj-type-id ,symbol-obj-type-id)
              (
               (call write-to-stdout ,(upl-obj-symbol-name-buf 'obj) ,(upl-obj-symbol-name-length 'obj))
               )]
             (else (
                    ,(upl-print-static-string "bad obj-type: ")
                    (call print-number obj-type-id)
                    (call print-newline)
                    (call print-stack-trace)
                    ,(upl-exit 1)
                    )))
           ))
    (proc init-globals () ()
          (
           (call init-lisp-stack)
           (call init-lisp-local-val-stack)
           (call init-gc)
           (call init-symbol-allocator)
           (call init-symbol-block-list)
           (call init-literals-block-list)
           ))
    (proc c () () ((i64:= 0 0)))
    (proc b () ((d 0)) ((call c) (:= d 0)))
    (proc a () ((d 0)) ((call b) (:= d 0)))

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
    (proc print-lisp-stack () ([ptr 0])
          [
           (:= ptr lisp-stack-ptr)
           ,(upl-print-static-string "=== lisp stack top ===\n")
           (while (>= ptr lisp-stack-bottom)
                  [
                   (call print-object (i64@ ptr))
                   (call print-newline)
                   (:-= ptr 8)
                   ])
           ,(upl-print-static-string "=== lisp stack bottom ===\n")
           ])
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

(define (make-ffi-no-arg-func func-name)
  (let ([funcs ((upl-compiler-get-functions upl-compiler))]
        [func (findf (lambda (f) (equal? func-name (compiled-upl-func-name f))) funcs)]
        [fptr (compiled-upl-func-ptr func)])
    (lambda () (native-call fptr))))

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
(define ffi-allocate-literal-from-stack (make-ffi-no-arg-func 'allocate-literal-from-stack))
(define ffi-allocate-i64 (make-ffi-one-arg-func 'allocate-i64))

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

(define (compile-lisp-literal expr)
  (cond
    [(symbol? expr) 
     '(
       (call push-to-lisp-stack ,(upl-lisp-symbol (symbol-to-string expr)))
       )]
    [(boolean? expr)
     '(
       (call push-to-lisp-stack ,(if expr boolean-obj-true-value boolean-obj-false-value))
       )]
    [(number? expr)
     '(
       (call push-to-lisp-stack (i64@ ,(ffi-allocate-number-literal expr)))
       )]
    [(empty? expr)
     '(
       (call push-to-lisp-stack 0)
       )]
    [(pair? expr)
     '(
       (block ,(compile-lisp-literal (cdr expr)))
       (block ,(compile-lisp-literal (car expr)))
       (call push-to-lisp-stack 0)
       (call lisp-procedure-cons)
       )]
    [else (panic (list "bad expression: " expr))]))

(define (upl-create-lisp-literal expr)
  ((upl-compiler-run upl-compiler) '()
                                   '((block ,(compile-lisp-literal expr))))
  (ffi-allocate-literal-from-stack))

; to compile lisp expression:
;   - symbol: emit push bound value
;   - literal: emit push literal
;   - list:
;       - define
;       - begin
;       - if
;       - quote
;       - let
;       - lambda
;       - default: compile items in reverse order and emit function call code at the end

(define (find-captured-values-in-let-expression-rec bindings result-expr bound-vals outer-scope-vals)
  (if (empty? bindings)
    (find-captured-values-rec result-expr bound-vals outer-scope-vals)
    (let ([binding (first bindings)])
      (append
        (find-captured-values-rec (second binding) bound-vals outer-scope-vals)
        (find-captured-values-in-let-expression-rec
          (cdr bindings)
          result-expr
          (cons (first binding) bound-vals)
          outer-scope-vals)))))

(define (find-captured-values-rec expr bound-vals outer-scope-vals)
  (cond
    [(symbol? expr) (if
                      (and (empty? (member expr bound-vals)) (non-empty-list? (member expr outer-scope-vals)))
                      (list expr)
                      '())]
    [(boolean? expr) '()]
    [(number? expr) '()]
    [(list? expr)
     (cond
       [(equal? 'if (first expr))
        (--- append
             (find-captured-values-rec (nth 1 expr) bound-vals outer-scope-vals)
             (find-captured-values-rec (nth 2 expr) bound-vals outer-scope-vals)
             (find-captured-values-rec (nth 3 expr) bound-vals outer-scope-vals))]
       [(equal? 'define (first expr)) (panic "define is not supported in lambdas" expr)]
       [(equal? 'begin (first expr)) (flatmap (lambda (e) (find-captured-values-rec e bound-vals outer-scope-vals)) (cdr expr))]
       [(equal? 'quote (first expr)) '()]
       [(equal? 'let (first expr))
          (find-captured-values-in-let-expression-rec (nth 1 expr) (nth 2 expr) bound-vals outer-scope-vals)]
       [(equal? 'lambda (first expr))
        (let ([lambda-args (second expr)])
          (find-captured-values-rec (nth 2 expr) (append lambda-args bound-vals) outer-scope-vals))]
       [else (flatmap (lambda (e) (find-captured-values-rec e bound-vals outer-scope-vals)) expr)])]
    [else (panic (list "bad expression: " expr))]))

(define (find-captured-values expr bound-vals outer-scope-vals)
  (remove-duplicates (find-captured-values-rec expr bound-vals outer-scope-vals)))

(define (compile-lisp-let-expression bindings result-expr local-vals captured-vals)
  (assert-stmt (list "let bindings are a list" bindings) (list? bindings))
  (if (empty? bindings)
    (compile-lisp-expression result-expr local-vals captured-vals)
    (let ([binding (first bindings)])
      (assert-stmt (list "let binding is a list of 2 elements" binding) (and (list? binding) (equal? (length binding) 2)))
      (let ([sym (first binding)]
            [expr (second binding)])
        (assert-stmt (list "let binding's first element is a symbol" ) (symbol? sym))
        '(
          (block ,(compile-lisp-expression expr local-vals captured-vals))
          (call push-to-lisp-local-val-stack)
          (block ,(compile-lisp-let-expression (cdr bindings) result-expr (cons sym local-vals) captured-vals))
          )))))

(define (compile-lisp-expression expr local-vals captured-vals)
  (cond
    [(symbol? expr) 
     (let ([val-index (index-of local-vals expr)])
       (if (empty? val-index)
         (let ([val-index (index-of captured-vals expr)])
           (if (empty? val-index)
             '(
               (call push-symbol-bound-value ,(upl-lisp-symbol (symbol-to-string expr)))
               )
             '(
               (call push-to-lisp-stack ,(upl-obj-procedure-field '(fcall peek-lisp-local-val-stack ,(length local-vals)) val-index))
               )))
         '(
           (call push-to-lisp-stack (fcall peek-lisp-local-val-stack ,val-index))
           )))]
    [(boolean? expr)
     '(
       (call push-to-lisp-stack ,(if expr boolean-obj-true-value boolean-obj-false-value))
       )]
    [(number? expr)
     '(
       (call push-to-lisp-stack (i64@ ,(ffi-allocate-number-literal expr)))
       )]
    [(list? expr)
     (cond
       [(equal? 'if (first expr))
        [begin
          (assert-stmt (list "'if' has 3 args, not: " expr) (equal? (length expr) 4))
          '(
            (block ,(compile-lisp-expression (second expr) local-vals captured-vals))
            ,(upl-check-lisp-type '(fcall peek-lisp-stack 0) boolean-obj-type-id "first if arg")
            (if (= (fcall pop-lisp-stack) ,boolean-obj-true-value)
              [(block ,(compile-lisp-expression (nth 2 expr) local-vals captured-vals))]
              [(block ,(compile-lisp-expression (nth 3 expr) local-vals captured-vals))])
            )]]
       [(equal? 'define (first expr))
        [begin
          (assert-stmt (list "'define' has 2 args, not: " expr) (equal? (length expr) 3))
          (assert-stmt (list "first arg of 'define' must be a symbol: " expr) (symbol? (second expr)))
          '(
            (block ,(compile-lisp-expression (nth 2 expr) local-vals captured-vals))
            (call lisp-define ,(upl-lisp-symbol (symbol-to-string (second expr))))
            )]]
       [(equal? 'begin (first expr))
        (cdr
          (flatmap (lambda (e) (let ([compiled-expr (compile-lisp-expression e local-vals captured-vals)])
                                 (append '((call drop-lisp-stack 1)) compiled-expr)))
                   (cdr expr)))]
       [(equal? 'quote (first expr))
        [begin
          (assert-stmt (list "'quote' has 1 arg, not: " expr) (equal? (length expr) 2))
          '(
            (call push-to-lisp-stack (i64@ ,(upl-create-lisp-literal expr)))
            )]]
       [(equal? 'let (first expr))
        [begin
          (assert-stmt (list "'let' has 2 args, not: " expr) (equal? (length expr) 3))
          (let ([bindings (second expr)]
                [result-expr (nth 2 expr)])
            (assert-stmt (list "let bindings is a list, not:" bindings) (list? bindings))
            '(
              (block ,(compile-lisp-let-expression bindings result-expr local-vals captured-vals))
              (call drop-lisp-local-val-stack ,(length bindings))
              ))]]
       [(equal? 'lambda (first expr))
        [begin
          (assert-stmt (list "'lambda' has 2 args, not: " expr) (equal? (length expr) 3))
          (let ([args (second expr)]
                [body (nth 2 expr)])
            (assert-stmt (list "lambda args is a list, not:" args) (list? args))
            (let (
                  [lambda-captured-vals (find-captured-values body args (append local-vals captured-vals))]
                  [fptr
                    (compile-native-func 'lambda '()
                                         '(
                                           ; push lambda args to local value stack
                                           (block ,(map (lambda (_) '(call push-to-lisp-local-val-stack)) (range (+ (length args) 1))))
                                           (block ,(compile-lisp-expression body (reverse args) lambda-captured-vals))
                                           (call drop-lisp-local-val-stack ,(+ (length args) 1))
                                           ))]
                  )
              '(
                (call push-to-lisp-stack (fcall allocate-procedure ,fptr ,(length args) 0))
                (block ,(map
                          (lambda (v-and-index)
                            '(block (
                              (block ,(compile-lisp-expression (car v-and-index) local-vals captured-vals))
                              ,(upl-obj-procedure-set-field '(fcall peek-lisp-stack 1) (cdr v-and-index) '(fcall pop-lisp-stack))
                              )))
                          (zip-with-index lambda-captured-vals)))
                )))
            ]]
       [else
         '(
           (block ,(flatmap (lambda (e)
                              '(
                                (block, (compile-lisp-expression e local-vals captured-vals))
                                ))
                            (reverse expr)))
           (call call-lisp-procedure ,(- (length expr) 1))
           )
         ])]
    [else (panic (list "bad expression: " expr))]))

(ffi-lisp-define "nil" 0)

(define (compile-top-level-lisp-expression expr)
  (compile-lisp-expression expr '() '()))

((upl-compiler-run upl-compiler) '([handle 0]) 
                                 '(
                                   ;(call tests)
                                   (call push-to-lisp-stack (fcall allocate-i64 42))
                                   (:= handle (fcall allocate-number-literal 42))
                                   (call run-gc)
                                   ,(upl-debug-print-hex-value "literal obj: " '(i64@ handle))
                                   (call print-object (i64@ handle))
                                   (call print-newline)
                                   (call run-gc)

                                  ; (call print-lisp-stack)
                                   (block ,(compile-top-level-lisp-expression 
                                             '((lambda (x) (+ x 2)) 1)
                                             ))
                                   (call run-gc)


                                   ;,(upl-exit 0)
                                   (block ,(compile-top-level-lisp-expression '(define a 42)))
                                   ;(block ,(compile-lisp-expression '(begin (* a nil) 4242) '()))
                                   (block ,(compile-top-level-lisp-expression ''(1 2 3)))
                                   (call print-object (fcall pop-lisp-stack))
                                   (call print-newline)
                                   (block ,(compile-top-level-lisp-expression '(let ([a 1] [b 2] [c 3]) c)))
                                   (call print-object (fcall pop-lisp-stack))
                                   (call print-newline)

                                   (block ,(compile-top-level-lisp-expression '(let ([a 1] [b 2] [c 3]) b)))
                                   (call print-object (fcall pop-lisp-stack))
                                   (call print-newline)

                                   (block ,(compile-top-level-lisp-expression '(let ([a 1] [b 2] [c 3]) a)))
                                   (call print-object (fcall pop-lisp-stack))
                                   (call print-newline)

                                   (block ,(compile-top-level-lisp-expression 
                                             '(((lambda (x) (lambda (y) (+ x y))) 1) 2)
                                               ))
                                   (call print-object (fcall pop-lisp-stack))
                                   (call print-newline)
                                   ;(call drop-lisp-local-val-stack 1)
                                   ;(call test-block-list)
                                   ;(call a)
                                   ))

