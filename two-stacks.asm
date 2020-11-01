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
i_mul:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; number
        mov         rdi,  [r12 + 8*0]        ; number
        add         r12, 8
        imul        rax, rdi
        mov         [r12 - 8], rax
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

; <a> <b> -> <a || b>
i_or:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, 0
        jne          i_or__true_branch
        cmp         rdi, 0
        jne          i_or__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_or__true_branch:
        mov QWORD   [r12 - 8], 1
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
i_less:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jl          i_less__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_less__true_branch:
        mov QWORD   [r12 - 8], 1
        jmp iloop

; <a> <b> -> <a <= b>
i_less_or_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jle         i_less__true_branch
        mov QWORD   [r12 - 8], 0
        jmp iloop
i_less_or_equal__true_branch:
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

; <address> <value>
i_write_mem_byte:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; address
        mov         rdi,  [r12 + 8*0]        ; value
        mov         [rax], dil
        jmp iloop

; <address> -> <value>
i_read_mem_byte:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; address
        mov         rdi,  0
        mov         dil, [rax]
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

i_2dup:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]
        mov         rdi,  [r12 + 8*0]
        bump_data_stack 4
        mov         [r12 - 8*1], rax
        mov         [r12 - 8*2], rdi
        mov         [r12 - 8*3], rax
        mov         [r12 - 8*4], rdi
        jmp iloop

; <a> <b> <c> -> <c> <a> <b>
i_rot:
        drop_data_stack 3
        mov         r8,  [r12 + 8*2]
        mov         r9,  [r12 + 8*1]
        mov         r10, [r12 + 8*0]
        add         r12, 8*3
        mov         [r12 - 8*1], r10
        mov         [r12 - 8*2], r8
        mov         [r12 - 8*3], r9
        jmp iloop

; <a> <b> <c> -> <b> <c> <a>
i_rev_rot:
        drop_data_stack 3
        mov         r8, [r12 + 8*2]
        mov         r9,  [r12 + 8*1]
        mov         r10,  [r12 + 8*0]
        add         r12, 8*3
        mov         [r12 - 8*1], r9
        mov         [r12 - 8*2], r10
        mov         [r12 - 8*3], r8
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


def_nl_terminated_static_string ss_test_atoi, "-123"

def_nl_terminated_static_string ss_syscall_error, "syscall error"
def_nl_terminated_static_string ss_panic, "panic"
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

; <size> -> <addr>
; allocate virtual memory pages
        dq          i_return, i_syscall,
        dq          val(9)                      ; mmap syscall num
        dq          val(0)                      ; NULL address
        dq          i_pop_from_ret_stack        ; length
        dq          val(3)                      ; PROT_READ | PROT_WRITE
        dq          val(34),                    ; MAP_A NONYMOUS|MAP_PRIVATE
        dq          val(-1)                     ; fd
        dq          val(0)                      ; offset
        dq          i_push_to_ret_stack         ; length
f_mmap_anon: equ     $-8

; <addr> <size>
; deallocate virtual memory pages
        dq          i_return, i_syscall,
        dq          val(11)                     ; munmap syscall num
        dq          i_pop_from_ret_stack        ; address
        dq          i_pop_from_ret_stack        ; length
        dq          val(0), val(0), val(0), val(0)
        dq          i_push_to_ret_stack         ; length
        dq          i_push_to_ret_stack         ; address
f_munmap: equ     $-8

; <size> -> <addr>
; malloc
        dq          i_return
        dq          i_add, val(8)                       ; <addr>
        dq          i_write_mem_i64                     ; <addr>
        dq          i_rev_rot, i_dup                    ; <addr> <size> <addr>
        dq          call(f_log_syscall_error)           ; <addr> <size>
        dq          call(f_mmap_anon), i_add, val(8)    ; <addr> <size>
        dq          i_dup                               ; <size> <size>
f_malloc: equ     $-8

; <addr>
; free
        dq          i_return
        dq          i_drop, call(f_log_syscall_error)
        dq          call(f_munmap)                      ; <err code>
        dq          i_swap                              ; <addr> <size>
        dq          i_read_mem_i64, i_dup               ; <size> <addr>
        dq          i_add, val(-8)                      ; <addr>
f_free: equ     $-8


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

; test malloc/free
        dq          i_read_mem_i64
        dq          call(f_free)
        dq          i_dup
        dq          call(f_print_bool)
        dq          i_equal
        dq          val(42)
        dq          i_read_mem_i64
        dq          i_dup
        dq          i_write_mem_i64, i_swap, val(42)
        dq          i_dup
        dq          call(f_malloc), val(100)
f_test_malloc_free: equ     $-8

; test byte array
        dq           i_return
        dq           call(f_free), i_pop_from_ret_stack
        dq              call(f_byte_array_print), i_peek_ret_stack, val(1)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(5), val(10)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(4), val(111)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(3), val(108)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(2), val(108)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(1), val(101)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(0), val(104)
        dq           i_push_to_ret_stack, call(f_make_byte_array), val(6)
f_test_byte_array: equ     $-8



; check syscall error
        dq          i_return
        dq          i_and
        dq              i_less_or_equal, val(-4095), i_swap
        dq              i_greater_or_equal, val(-1)
        dq          i_dup, i_dup
f_log_syscall_error__is_error: equ     $-8
        dq          i_return,
        dq          call(f_print_buffer), val(ss_syscall_error), val(ss_syscall_error_size)
f_log_syscall_error__log_error: equ     $-8
        dq          i_return,
        dq          call(f_if), val(f_log_syscall_error__is_error), val(f_log_syscall_error__log_error), val(f_id)
f_log_syscall_error: equ     $-8


; make byte array
; <n> -> <addr>
        dq          i_return, i_greater, val(0), i_dup
f_make_byte_array__is_negative: equ     $-8
        dq          i_return,
        dq          i_write_mem_i64                                         ; <addr>
        dq          i_rev_rot, i_dup                                        ; <addr> <size> <addr>
        dq          call(f_malloc)                                          ; <addr> <n>
        dq          i_add, val(8), i_dup                                    ; <n + 8> <n>
        dq          call(f_panic_if), val(f_make_byte_array__is_negative)   ; <n>
f_make_byte_array: equ     $-8

; byte array pointer
; <addr> -> <addr>
        dq          i_return, i_add, val(8)
f_byte_array_pointer: equ     $-8

; byte array size
; <addr> -> <n>
        dq          i_return, i_read_mem_i64
f_byte_array_size: equ     $-8

; byte array get byte
; <addr> <n> -> <value>
        dq          i_return
        dq          i_or                            ; <bool>
        dq              i_greater, val(0), i_swap   ; <bool> <bool>
        dq              i_greater_or_equal          ; <bool> <n>
        dq          i_rev_rot, i_dup                ; <n> <size> <n>
        dq          i_swap                          ; <n> <size>
f_byte_array_get__is_not_in_range: equ     $-8
        dq          i_return, i_read_mem_byte
        dq          i_add, val(8)               ; <addr>    skip size
        dq          i_add                       ; <addr>    add index
        dq          call(f_panic_if), val(f_byte_array_get__is_not_in_range) ; <n> <addr>
        dq          i_rot                       ; <size> <n> <n> <addr>
        dq          i_dup                       ; <n> <n> <size> <addr>
        dq          i_rot                       ; <n> <size> <addr>
        dq          i_read_mem_i64, i_dup       ; <size> <addr> <n>
f_byte_array_get: equ     $-8

; byte array set byte
; <addr> <n> <value>
        dq          i_return
        dq          i_or                           ; <bool>
        dq              i_greater, val(0), i_swap   ; <bool> <bool>
        dq              i_greater_or_equal          ; <bool> <n>
        dq          i_rev_rot, i_dup                ; <n> <size> <n>
        dq          i_swap                          ; <n> <size>
f_byte_array_set__is_not_in_range: equ     $-8
        dq          i_return, i_write_mem_byte
        dq          i_add, val(8)               ; <addr> <value>   skip size
        dq          i_add                       ; <addr> <value>   add index
        dq          call(f_panic_if), val(f_byte_array_set__is_not_in_range) ; <n> <addr> <value>
        dq          i_rot                       ; <size> <n> <n> <addr> <value>
        dq          i_dup                       ; <n> <n> <size> <addr> <value>
        dq          i_rot                       ; <n> <size> <addr> <value>
        dq          i_read_mem_i64, i_dup       ; <size> <addr> <n> <value>
f_byte_array_set: equ     $-8

; write byte array to stdout
; <addr>
        dq          i_return
        dq          call(f_print_buffer), i_add, val(8), i_swap
        dq          i_read_mem_i64, i_dup       ; <size> <addr>
f_byte_array_print: equ     $-8

; atoi, string to int
; <addr> <size> -> <number>
; loop invariant: <char index> <number> <buffer> <string size>
        dq          i_return
        dq          i_swap, i_mul, val(-1), i_swap
f_atoi__mul_by_minus_one: equ     $-8
        dq          i_return
        dq          i_add, val(1)
f_atoi__add_one: equ     $-8
        dq          i_return
        dq          i_equal
        dq              val(45)
        dq              i_read_mem_byte, i_dup_n, val(3)
f_atoi__first_char_is_minus: equ     $-8
        dq          i_return
        dq          i_not, i_or
        dq              i_equal, i_dup_n, val(2), i_dup_n, val(5) ; size == char index
        dq              i_or
        dq                  i_less, val(57)
        dq                  i_swap
        dq                  i_greater, val(48)
        dq                      i_dup, i_read_mem_byte
        dq                          i_add, i_dup_n, val(2),  i_dup_n, val(3),
f_atoi__while_cond: equ     $-8
        dq          i_return
        dq          i_add, val(1) ; advanсe char index
        dq          i_swap
        dq          i_add
        dq              i_sub,   ; next digit
        dq                  i_read_mem_byte
        dq                      i_add, i_dup_n, val(4), i_dup_n, val(4), ; add <char index> and <buffer>
        dq                  val(48) ; <val 48> <number> <char index>  <buffer> <string size>
        dq              i_mul, val(10)                              ; multiply number by 10
        dq          i_swap ; <number> <char index>  <buffer> <string size>
f_atoi__consume_one_char: equ     $-8
        dq          i_return
        dq          i_drop, i_swap
        dq          i_drop, i_swap
        dq          i_drop
        dq          call(f_if), val(f_atoi__first_char_is_minus), val(f_atoi__mul_by_minus_one), val(f_id)
        dq          call(f_while), val(f_atoi__while_cond), val(f_atoi__consume_one_char)
        dq          call(f_if), val(f_atoi__first_char_is_minus), val(f_atoi__add_one), val(f_id)
        dq          val(0)                          ; <char index>
        dq          val(0)                          ; <number>
f_atoi: equ     $-8

; panic_if function
        dq          call(f_exit_0), call(f_print_panic)
f_panic_if__do_panic: equ     $-8
        dq          i_return,
        dq          call(f_if), i_rot, val(f_panic_if__do_panic), val(f_id)
f_panic_if: equ     $-8

; print "debug"
        dq          i_return, f_print_buffer, i_call, val(ss_debug), val(ss_debug_size)
f_print_debug: equ     $-8
; print "panic"
        dq          i_return, f_print_buffer, i_call, val(ss_panic), val(ss_panic_size)
f_print_panic: equ     $-8
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
        dq          i_less, val(0), i_dup
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

; check print and exit
        dq          call(f_exit_0), call(f_print_bool), i_equal
f_check: equ     $-8

        
; interpreter's entry point
        dq          call(f_exit_0)
        dq          f_print_bool, i_call,
        dq          i_and, i_not
        dq              i_or, val(0), val(0)
        dq          i_and,
        dq              i_or, val(1), val(0)
        dq          i_and,
        dq              i_or, val(0), val(1)
        dq          i_and,
        dq              i_or, val(1), val(1)
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
        dq              i_less, val(1), val(1)
        dq          i_and
        dq              i_less, val(0), val(1)
        dq          i_and
        dq              i_equal
        dq                  val(0)
        dq                  i_add, val(-1), val(1)
        dq          i_and
        dq              i_equal
        dq                  val(101)
        dq                  i_read_mem_byte, i_add, val(1), val(ss_hello_world)
        dq          i_and
        dq              i_equal
        dq                  val(10)
        dq                  i_read_mem_i64, call(f_make_byte_array), val(10)
        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(42)
        dq                  call(f_byte_array_get), i_peek_ret_stack, val(1), val(3)
        dq              call(f_byte_array_set), i_peek_ret_stack, val(1), val(3), val(42)
        dq              i_push_to_ret_stack, call(f_make_byte_array), val(10)
        dq          i_and
        dq              i_equal
        dq                  val(-123)
        dq                  call(f_atoi), val(ss_test_atoi), val(ss_test_atoi_size)
        dq          val(1)

        ;dq          call(f_test_byte_array)
        ;dq          call(f_print_buffer),call(f_print_buffer), i_2dup, val(ss_hello_world), val(13)

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
