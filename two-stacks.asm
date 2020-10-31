        global    _start

%macro  def_nl_terminated_static_string 2
%1:
        db        %2, 10      ; note the newline at the end
%1_size: equ $-%1
%endmacro

%macro  push_registers 0
        push        r11
        push        r12
        push        r13
%endmacro

%macro  pop_registers 0
        pop         r13
        pop         r12
        pop         r11
%endmacro

%macro  bump_data_stack 1
        add         r12, 8 * %1
        check_data_stack_overflow
%endmacro

%macro  check_data_stack_overflow 0
        cmp         r12, data_stack + data_stack_size
        jg          data_stack_overflow_handler
%endmacro

%macro  drop_data_stack 1
        sub         r12, 8 * %1
        check_data_stack_underflow
%endmacro

%macro  check_data_stack_underflow 0
        cmp         r12, data_stack
        jl          data_stack_underflow_handler
%endmacro

%macro  bump_return_stack 1
        add         r13, 8 * %1
        check_return_stack_overflow
%endmacro

%macro  check_return_stack_overflow 0
        cmp         r13, return_stack + return_stack_size
        jg          return_stack_overflow_handler
%endmacro

%macro  drop_return_stack 1
        sub         r13, 8 * %1
        check_return_stack_underflow
%endmacro

%macro  check_return_stack_underflow 0
        cmp         r13, return_stack
        jl          return_stack_underflow_handler
%endmacro

%macro  debug 0
        mov         rdx, rax
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, ss_debug            
        mov         rdx, ss_debug_size               
        push        r11
        syscall
        pop         r11
%endmacro


data_stack_size:        equ    10240
return_stack_size:      equ    10240

        section   .text

; r11 - instruction pointer, points to the next instruction
; r12 - data stack pointer, points to the next slot
; r13 - return stack pointer, points to the next slot

iloop:  
        sub         r11, 8
        jmp         [r11 + 8]

data_stack_overflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_data_stack_overflow_handler
        jmp iloop
        
data_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_data_stack_underflow_handler
        jmp iloop

return_stack_overflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_return_stack_overflow_handler
        jmp iloop

return_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_return_stack_underflow_handler
        jmp iloop

; <number> <number> -> <number>
i_add:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; number
        mov         rdi,  [r12 + 8*0]        ; number
        add         r12, 8
        add         rax, rdi
        mov         [r12 - 8], rax
        jmp iloop

; <a> <b> -> <a - b>
i_sub:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        sub         rax, rdi
        mov         [r12 - 8], rax
        jmp iloop

; <true> -> <false>
; <false> -> <true>
i_not:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; bool
        add         r12, 8
        cmp         rax, 0
        je          i_not__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_not__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a && b>
i_and:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, 0
        je          i_and__false_branch
        cmp         rdi, 0
        je          i_and__false_branch
        mov QWORD   [r12 - 8], 1
        jmp iloop
i_and__false_branch:
        mov QWORD   [r12 - 8], 0
        jmp iloop

; <a> <b> -> <a == b>
i_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        je          i_equal__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a < b>
i_lower:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jl          i_lower__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_lower__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a <= b>
i_lower_or_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jle         i_lower__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_lower_or_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a > b>
i_greater:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jg          i_greater__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_greater__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a >= b>
i_greater_or_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jge          i_greater__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_greater_or_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <address> <value>
i_write_mem_i64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; address
        mov         rdi,  [r12 + 8*0]        ; value
        mov         [rax], rdi
        jmp iloop
        
; <address> -> <value>
i_read_mem_i64:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; address
        mov         rdi, [rax]
        mov         [r12], rdi
        add         r12, 8
        jmp iloop



i_jmp:
        mov         r11, [r11]             ; next word is the instruction pointer
        jmp iloop

;<bool>
i_jmp_if:
        drop_data_stack 1
        sub         r11, 8                  ; move instruction pointer to the next instruction
        mov         rax, [r12]              ; top of the data stack is the boolean
        cmp         rax, 0
        je          i_jmp_if__false_branch
        mov         r11, [r11 + 8]          ; next word is the instruction pointer
i_jmp_if__false_branch:
        jmp iloop

i_call:
        bump_return_stack 1
        mov         rax, [r11]              ; next word is the instruciton pointer
        sub         r11, 8                  ; move instruction pointer to the next instruction
        mov         [r13 - 8], r11          ; save return pointer
        mov         r11, rax                ; jump to callee
        jmp iloop

i_indirect_call:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the instruciton pointer
        bump_return_stack 1
        mov         [r13 - 8], r11          ; save return pointer
        mov         r11, rax                ; jump to callee
        jmp iloop

; <a> <b> <true> -> <a>
; <a> <b> <false> -> <b>
i_value_if:
        drop_data_stack 3
        mov         r10, [r12 + 8*2]        ; a
        mov         r8,  [r12 + 8*1]        ; b
        mov         r9,  [r12 + 8*0]        ; bool value
        add         r12, 8
        cmp         r9, 0
        je          i_value_if__false_branch
        mov         [r12 - 8*1], r10
        jmp iloop
i_value_if__false_branch:
        mov         [r12 - 8*1], r8
        jmp iloop

; peeks n-th value from return stack
; <number>
i_peek_ret_stack:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]       ; read number, TODO: check for non-positive
        check_return_stack_underflow
        imul        rax, 8                  ; mul by word length
        sub         r13, rax                ; move stack pointer
        mov         rdi, [r13]              ; read value from return stack
        add         r13, rax                ; restore stack pointer
        mov         [r12], rdi              ; write value to data stack
        add         r12, 8                  ; bump data stack
        jmp iloop

i_push_to_ret_stack:
        drop_data_stack 1
        bump_return_stack 1
        mov         rax, [r12]              ; top of the data stack
        mov         [r13 - 8], rax          ; save value
        jmp iloop
        
i_pop_from_ret_stack:
        drop_return_stack 1
        bump_data_stack 1
        mov         rax, [r13]              ; restore value
        mov         [r12 - 8], rax          ; write to stack
        jmp iloop

i_return:
        drop_return_stack 1
        mov         r11, [r13]              ; restore return pointer
        jmp iloop

; linux syscall args [ %rdi, %rsi, %rdx, %r10, %r8, %r9 ]        
; <syscall num> <arg 0> <arg 1> ... <arg 5>
i_syscall:
        drop_data_stack 7                   ; move stack pointer
        mov         rax, [r12 + 8*6]
        mov         rdi, [r12 + 8*5]
        mov         rsi, [r12 + 8*4]
        mov         rdx, [r12 + 8*3]
        mov         r10, [r12 + 8*2]
        mov         r8,  [r12 + 8*1]
        mov         r9,  [r12 + 8*0]
        push_registers
        syscall
        pop_registers
        mov         [r12], rax
        bump_data_stack 1
        jmp iloop

i_dup:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]
        bump_data_stack 2
        mov         [r12 - 8*1], rax
        mov         [r12 - 8*2], rax
        jmp iloop

; <n> -> <a>
i_dup_n:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]
        imul         rax, 8                  ; TODO: check positive
        sub         r12, rax
        check_data_stack_underflow
        mov         rdi, [r12 + 8*0]       ; value
        add         r12, rax
        add         r12, 8
        mov         [r12 - 8*1], rdi
        jmp iloop

i_swap:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]
        mov         rdi,  [r12 + 8*0]
        bump_data_stack 2
        mov         [r12 - 8*1], rdi
        mov         [r12 - 8*2], rax
        jmp iloop
                
i_drop:
        drop_data_stack 1
        jmp iloop
                
i_push_to_stack:
        bump_data_stack 1
        mov         rax, [r11]              ; next word is value
        mov         [r12 - 8], rax          ; write to stack
        sub         r11, 8                  ; move instruction pointer to the next instruction
        jmp iloop


_start: 
        mov         r11, f_start
        mov         r12, data_stack
        mov         r13, return_stack
        jmp         iloop        

        section   .data


def_nl_terminated_static_string ss_true, "true"
def_nl_terminated_static_string ss_false, "false"


def_nl_terminated_static_string ss_debug, "debug"
def_nl_terminated_static_string ss_hello_world, "Hello, World"

def_nl_terminated_static_string ss_data_stack_overflow, "Data stack overflow"
def_nl_terminated_static_string ss_data_stack_underflow, "Data stack underflow"

def_nl_terminated_static_string ss_return_stack_overflow, "Return stack overflow"
def_nl_terminated_static_string ss_return_stack_underflow, "Return stack underflow"


%define val(value) value, i_push_to_stack
%define call(value) value, i_call

; data stack overflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(ss_data_stack_overflow), val(ss_data_stack_overflow_size)
f_data_stack_overflow_handler: equ     $-8

; data stack underflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(ss_data_stack_underflow), val(ss_data_stack_underflow_size)
f_data_stack_underflow_handler: equ     $-8
        

; return stack overflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(ss_return_stack_overflow), val(ss_return_stack_overflow_size)
f_return_stack_overflow_handler: equ     $-8

; return stack underflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(ss_return_stack_underflow), val(ss_return_stack_underflow_size)
f_return_stack_underflow_handler: equ     $-8

; <bool function> <true function> <false function>
; if function
        dq          i_return,
        dq          i_indirect_call
        dq          i_value_if
        dq          i_drop, i_pop_from_ret_stack                ; drop bool f
        dq          i_pop_from_ret_stack                        ; true f
        dq          i_pop_from_ret_stack                        ; false f
        dq          i_indirect_call, i_peek_ret_stack, val(3)   ; bool value
        dq          i_push_to_ret_stack                         ; false f -> ret stack
        dq          i_push_to_ret_stack                         ; true f -> ret stack
        dq          i_push_to_ret_stack                         ; bool f -> ret stack
f_if: equ     $-8

; <cond function> <body function>
; while function
        dq          i_return,
        dq          i_drop, i_pop_from_ret_stack                ; drop cond f
        dq          i_drop                                      ; drop body f
f_while__end:
        dq          i_pop_from_ret_stack                        ; pop  body f
        dq          call(f_print_debug)
        dq          f_while__loop + 8, i_jmp                    ; loop
        dq          i_indirect_call, i_peek_ret_stack, val(1)   ; run body f
        dq          f_while__end, i_jmp_if, i_not               ; check condition
        dq          i_indirect_call,                            ; bool value
        dq          i_peek_ret_stack,
f_while__loop:
        dq          val(2)
        dq          i_push_to_ret_stack                         ; body f -> ret stack
        dq          i_push_to_ret_stack                         ; cond f -> ret stack
f_while: equ     $-8



; <buffer pointer> <length>
; print buffer
        dq          i_return, i_drop, i_syscall, 
        dq          val(1), val(1)              ; syscall num, fd
        dq          i_pop_from_ret_stack        ; buffer pointer
        dq          i_pop_from_ret_stack        ; buffer length
        dq          val(1), val(1), val(1)      ; filler
        dq          i_push_to_ret_stack         ; buffer length
        dq          i_push_to_ret_stack         ; buffer pointer
f_print_buffer: equ     $-8

; <buffer pointer> <count> -> <bytes read>
; read to buffer
        dq          i_return, i_syscall, 
        dq          val(0), val(0)              ; syscall num, fd
        dq          i_pop_from_ret_stack        ; buffer pointer
        dq          i_pop_from_ret_stack        ; count
        dq          val(1), val(1), val(1)      ; filler
        dq          i_push_to_ret_stack         ; count
        dq          i_push_to_ret_stack         ; buffer pointer
f_read_from_std_in: equ     $-8

; <length> -> <addr>
; allocate virtual memory pages
        dq          i_return, i_syscall,
        dq          val(9)                      ; mmap syscall num
        dq          val(0)                      ; NULL address
        dq          i_pop_from_ret_stack        ; length
        dq          val(3)                      ; PROT_READ | PROT_WRITE
        dq          val(34),                    ; MAP_ANONYMOUS|MAP_PRIVATE
        dq          val(-1)                     ; fd
        dq          val(0)                      ; offset
        dq          i_push_to_ret_stack         ; length
f_mmap_anon: equ     $-8

; <length> -> <addr>
; malloc
        dq          i_return, i_syscall, 
        dq          val(60)                     ; syscall num
        dq          val(0)                      ; exit code
        dq          val(1), val(1), val(1), val(1), val(1) ; filler
f_malloc: equ     $-8


; exit_0
        dq          i_return, i_syscall, 
        dq          val(60)                     ; syscall num
        dq          val(0)                      ; exit code
        dq          val(1), val(1), val(1), val(1), val(1) ; filler
f_exit_0: equ     $-8

; TESTS
; data stack overflow
        dq          i_return, $ + 8*6, i_jmp, val(13), f_print_hello_world, i_call
f_dstack_overflow: equ     $-8

; return stack overflow
        dq          i_return, $ + 8*7, i_jmp, i_push_to_ret_stack, val(13), f_print_hello_world, i_call
f_rstack_overflow: equ     $-8
; i_peek_ret_stack
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack, i_drop, i_pop_from_ret_stack ; restore ret stack
        dq          i_indirect_call
        dq          i_peek_ret_stack, val(2)    ; peek f_print_hello_world
        dq          i_push_to_ret_stack         ; f_print_data_overflow
        dq          i_push_to_ret_stack         ; f_print_hello_world
        dq          val(f_print_hello_world)
        dq          val(f_print_data_overflow)
f_test_peek_ret_stack: equ     $-8


; print "debug"
        dq          i_return, f_print_buffer, i_call, val(ss_debug), val(ss_debug_size)
f_print_debug: equ     $-8
; print "Hello world"
        dq          i_return, f_print_buffer, i_call, val(ss_hello_world), val(13)
f_print_hello_world: equ     $-8
; print "Data stack underflow"
        dq          i_return, f_print_buffer, i_call, val(ss_data_stack_underflow), val(ss_data_stack_underflow_size)
f_print_data_overflow: equ     $-8
; print "Return stack underflow"
        dq          i_return, f_print_buffer, i_call, val(ss_return_stack_underflow), val(ss_return_stack_underflow_size)
f_print_stack_overflow: equ     $-8

; echo
        dq          i_return, f_print_buffer, i_call, i_swap
        dq          f_read_from_std_in, i_call, i_swap, val(100), i_dup
        dq          f_mmap_anon, i_call, val(100)
f_echo: equ     $-8

; identity
        dq          i_return
f_id: equ     $-8

; <n> <function>
; do n times

        dq          i_return
        dq          i_lower, val(0), i_dup
f_do_n_times__cond: equ     $-8
        dq          i_return
        dq          i_add, val(-1)
        dq          i_pop_from_ret_stack
        dq          i_indirect_call,
        dq          i_dup
        dq          i_push_to_ret_stack,
f_do_n_times__body: equ     $-8
        dq          i_return,

        dq          i_drop, i_drop,
        dq          f_while, i_call, val(f_do_n_times__cond), val(f_do_n_times__body)


f_do_n_times: equ     $-8


; print boolean
        dq          i_return, val(ss_false), val(ss_false_size)
f_print_bool_false: equ     $-8
        dq          i_return, val(ss_true), val(ss_true_size)
f_print_bool_true: equ     $-8
        dq          i_return, f_print_buffer, i_call, f_if, i_call, val(f_id), val(f_print_bool_true), val(f_print_bool_false)
f_print_bool: equ     $-8



; false function
        dq          i_return, val(0)
f_false: equ     $-8
; true function
        dq          i_return, val(1)
f_true: equ     $-8
        
; interpreter's entry point
        dq          f_exit_0, i_call
        dq          f_print_bool, i_call,
        dq          i_and, i_not
        dq              i_equal, val(0), val(1)
        dq          i_and,
        dq              i_equal
        dq                  val(4)
        dq                  i_add, val(2), val(2)
        dq          i_and
        dq              i_equal
        dq                  val(4)
        dq                  i_sub, val(6), val(2)
        dq          i_and, i_not
        dq              i_greater, val(1), val(1)
        dq          i_and
        dq              i_greater, val(10), val(1)
        dq          i_and, i_not
        dq              i_lower, val(1), val(1)
        dq          i_and
        dq              i_lower, val(0), val(1)
        dq          i_and
        dq              i_equal
        dq                  val(0)
        dq                  i_add, val(-1), val(1)
        dq          val(1)

        dq          i_indirect_call, i_dup_n, val(1)
        dq          val(f_print_hello_world), val(f_print_data_overflow), val(f_echo)

        ;dq          jmp_test, i_jmp_if, val(0)
        ;dq          f_do_n_times, i_call, val(3), val(f_print_data_overflow)
        ;dq          call(f_echo)


;        dq          f_exit_0, i_jmp, val(1)



;        dq          f_print_bool, i_call,
;        dq              i_and, val(1), val(0)
;        dq          f_print_bool, i_call,
;        dq              i_and, val(0), val(1)
;        dq          f_print_bool, i_call,
;        dq              i_and, val(0), val(0)
;        dq          f_print_bool, i_call,
;        dq              i_and, val(1), val(1)
;        dq          f_print_bool, i_call,
;        dq              i_not, val(1)
;        dq          f_print_bool, i_call,
;        dq              i_not, val(0)

;        dq          i_indirect_call, i_read_mem_i64, val(heap_pointer)
;        dq          i_write_mem_i64, val(heap_pointer), val(f_print_hello_world)

;        dq          f_print_buffer, i_call, i_indirect_call, i_value_if, val(f_print_bool_true), val(f_print_bool_false), val(0)
;        dq          f_if, i_call, val(f_false), val(f_print_hello_world), val(f_print_data_overflow),
;        dq          f_if, i_call, val(f_true), val(f_print_hello_world), val(f_print_data_overflow),
;        dq          f_dstack_overflow, i_call
;        dq          f_rstack_overflow, i_call
;        dq          i_indirect_call, i_indirect_call, i_indirect_call, 
;        dq          val(f_print_hello_world), val(f_print_data_overflow), val(f_echo)
f_start: equ     $-8

        section   .bss
data_stack:         resb    data_stack_size
return_stack:       resb    return_stack_size
heap_pointer:       resq    1
