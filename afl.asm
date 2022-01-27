        global    _start

%macro  print_bin_num 1
        mov         rax, %1
        push rax
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, rsp
        ;add         rsi, 8
        mov         rdx, 8
        syscall
        pop rax
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
        push        r14
        syscall
        pop         r14
%endmacro

%macro  jump_to_next_instruction 0
        sub         r14, 8
        jmp         [r14 + 8]
%endmacro

data_stack_size:        equ    10240
return_stack_size:      equ    102400
perm_mem_size:          equ    1024000

        section   .text

; r12 - data stack pointer, points to the next free slot
; r13 - return stack pointer, points to the next free slot
; r14 - instruction pointer, points to the next instruction

; noop handler, all logic is in the restorer
sigsegv_handler:
        push   rbp
        mov    rbp,rsp
        leave
        ret
sigsegv_restorer:
        ; ucontext_t.uc_mcontext.__ctx = 40 bytes offset
        mov         [rdx + 40 + 4 * 8], r12
        mov         [rdx + 40 + 5 * 8], r13
        mov         [rdx + 40 + 6 * 8], r14
        bump_data_stack 1
        mov         [r12 - 8], r14 ; push instruction pointer to the data stack
        mov         r14, [sigsegv_handler_pointer]
        jump_to_next_instruction

data_stack_overflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r14, f_data_stack_overflow_handler
        jump_to_next_instruction
        
data_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r14, f_data_stack_underflow_handler
        jump_to_next_instruction

return_stack_overflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r14, f_return_stack_overflow_handler
        jump_to_next_instruction

return_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r14, f_return_stack_underflow_handler
        jump_to_next_instruction

; <number> <number> -> <number>
i_mul:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; number
        mov         rdi,  [r12 + 8*0]       ; number
        add         r12, 8
        imul        rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <number> <number> -> <bool> <number>
i_checked_mul:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; number
        mov         rdi,  [r12 + 8*0]       ; number
        add         r12, 16
        imul        rax, rdi
        jo i_checked_add_overflow
        mov QWORD   [r12 - 8], 1
        mov         [r12 - 16], rax
        jump_to_next_instruction
i_checked_mul_overflow:
        mov QWORD   [r12 - 8], 0
        mov         [r12 - 16], rax
        jump_to_next_instruction

; <a> <b> -> <a / b>
i_div:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        cqo                                 ; mov sign-extend of rax to rdx
        idiv         rdi                    ; divide rdx:rax by rdi
        mov         [r12 - 8], rax          ; rax is the quotient
        jump_to_next_instruction

; <a> <b> -> <a % b>
i_mod:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        cqo                                 ; mov sign-extend of rax to rdx
        idiv         rdi                    ; divide rdx:rax by rdi
        mov         [r12 - 8], rdx          ; rdx is the remainder
        jump_to_next_instruction

; <number> <number> -> <number>
i_add:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; number
        mov         rdi,  [r12 + 8*0]       ; number
        add         r12, 8
        add         rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <number> <number> -> <bool> <number>
i_checked_add:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; number
        mov         rdi,  [r12 + 8*0]       ; number
        add         r12, 16
        add         rax, rdi
        jo i_checked_add_overflow
        mov QWORD   [r12 - 8], 1
        mov         [r12 - 16], rax
        jump_to_next_instruction
i_checked_add_overflow:
        mov QWORD   [r12 - 8], 0
        mov         [r12 - 16], rax
        jump_to_next_instruction

; <a> <b> -> <a - b>
i_sub:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        sub         rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <a> <b> -> <a ^ b>
i_bit_xor_64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        xor         rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <a> <b> -> <a | b>
i_bit_or_64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        or          rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <a> <b> -> <a & b>
i_bit_and_64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rdi,  [r12 + 8*0]       ; b
        add         r12, 8
        and         rax, rdi
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <a> <b> -> <a << b>
i_bit_rshift_i64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rcx,  [r12 + 8*0]       ; b
        add         r12, 8
        sar         rax, cl
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <a> <b> -> <a << b>
i_bit_lshift_i64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]       ; a
        mov         rcx,  [r12 + 8*0]       ; b
        add         r12, 8
        sal         rax, cl
        mov         [r12 - 8], rax
        jump_to_next_instruction

; <true> -> <false>
; <false> -> <true>
i_not:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; bool
        add         r12, 8
        cmp         rax, 0
        je          i_not__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_not__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

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
        jump_to_next_instruction
i_and__false_branch:
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction

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
        jump_to_next_instruction
i_or__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <a> <b> -> <a == b>
i_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        je          i_equal__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <a> <b> -> <a < b>
i_less:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jl          i_less__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_less__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <a> <b> -> <a <= b>
i_less_or_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jle         i_less__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_less_or_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <a> <b> -> <a > b>
i_greater:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jg          i_greater__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_greater__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <a> <b> -> <a >= b>
i_greater_or_equal:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; a
        mov         rdi,  [r12 + 8*0]        ; b
        add         r12, 8
        cmp         rax, rdi
        jge          i_greater__true_branch
        mov QWORD   [r12 - 8], 0
        jump_to_next_instruction
i_greater_or_equal__true_branch:
        mov QWORD   [r12 - 8], 1
        jump_to_next_instruction

; <address> <value>
i_write_mem_i64:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; address
        mov         rdi,  [r12 + 8*0]        ; value
        mov         [rax], rdi
        jump_to_next_instruction
        
; <address> -> <value>
i_read_mem_i64:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; address
        mov         rdi, [rax]
        mov         [r12], rdi
        add         r12, 8
        jump_to_next_instruction

; <address> <value>
i_write_mem_i32:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; address
        mov         rdi,  [r12 + 8*0]        ; value
        mov         [rax], edi
        jump_to_next_instruction
        
; <address> -> <value>
i_read_mem_i32:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; address
        mov         rdi,  0
        mov         edi, [rax]
        mov         [r12], rdi
        add         r12, 8
        jump_to_next_instruction

; <address> <value>
i_write_mem_byte:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]        ; address
        mov         rdi,  [r12 + 8*0]        ; value
        mov         [rax], dil
        jump_to_next_instruction

; <address> -> <value>
i_read_mem_byte:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]        ; address
        mov         rdi,  0
        mov         dil, [rax]
        mov         [r12], rdi
        add         r12, 8
        jump_to_next_instruction

i_memcopy:
        drop_data_stack 3
        mov         rcx,  [r12 + 8*2]        ; count
        mov         rsi,  [r12 + 8*1]        ; src address
        mov         rdi,  [r12 + 8*0]        ; dest address
i_memcopy_loop_8_bytes:
        cmp         rcx, 7
        jle         i_memcopy_loop_byte
        mov         rdx, [rsi]
        mov         [rdi], rdx
        sub         rcx, 8
        add         rsi, 8
        add         rdi, 8
        jmp         i_memcopy_loop_8_bytes
i_memcopy_loop_byte:
        cmp         rcx, 0
        jle         i_memcopy_end
        mov         dl, [rsi]
        mov         [rdi], dl
        sub         rcx, 1
        add         rsi, 1
        add         rdi, 1
        jmp         i_memcopy_loop_byte
i_memcopy_end:
        jump_to_next_instruction

i_jmp:
        mov         r14, [r14]             ; next word is the instruction pointer
        jump_to_next_instruction

i_indirect_jmp:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the instruciton pointer
        mov         r14, rax                ; jump to callee
        jump_to_next_instruction

;<bool>
i_jmp_if:
        drop_data_stack 1
        sub         r14, 8                  ; move instruction pointer to the next instruction
        mov         rax, [r12]              ; top of the data stack is the boolean
        cmp         rax, 0
        je          i_jmp_if__false_branch
        mov         r14, [r14 + 8]          ; next word is the instruction pointer
i_jmp_if__false_branch:
        jump_to_next_instruction

i_call:
        bump_return_stack 1
        mov         rax, [r14]              ; next word is the instruciton pointer
        sub         r14, 8                  ; move instruction pointer to the next instruction
        mov         [r13 - 8], r14          ; save return pointer
        mov         r14, rax                ; jump to callee
        jump_to_next_instruction

i_indirect_call:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the instruciton pointer
        bump_return_stack 1
        mov         [r13 - 8], r14          ; save return pointer
        mov         r14, rax                ; jump to callee
        jump_to_next_instruction

i_native_indirect_call:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the native instruciton pointer
        call        rax
        add         r12, 8
        mov         [r12 - 8*1], rax        ; write return value to data stack
        jump_to_next_instruction

native_indirect_call_test:
        mov         rbx, -9223372036854775808
        mov         rdx, -9223372036854775808
        mov         rcx, -9223372036854775808
        mov         rax, -9223372036854775808
        ret

i_late_bind_and_call_word:
        bump_return_stack 1
        mov         rax, [r14]              ; next word is a [dict, name] struct
        sub         r14, 8                  ; move instruction pointer to the next instruction
        mov         [r13 - 8], r14          ; save return pointer
        bump_data_stack 2
        mov         [r12 - 8*1], rax        ; write value to data stack
        mov         r11, r14                ; save ip
        add         r11, 16
        mov         [r12 - 8*2], r11        ; write ip to data stack
        mov         r14, f_late_bind_and_call_word ; jump to bind_and_call function
        jump_to_next_instruction

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
        jump_to_next_instruction
i_value_if__false_branch:
        mov         [r12 - 8*1], r8
        jump_to_next_instruction

; peeks n-th value from return stack
; <number>
i_peek_ret_stack_first:
        bump_data_stack 1
        mov         rax, [r13 - 8]          ; read value from return stack
        mov         [r12 - 8*1], rax        ; write value to data stack
        jump_to_next_instruction

; peeks n-th value from return stack
; <number>
i_peek_ret_stack:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]       ; read number, TODO: check for non-positive
        imul        rax, 8                  ; mul by word length
        sub         r13, rax                ; move stack pointer
        check_return_stack_underflow
        mov         rdi, [r13]              ; read value from return stack
        add         r13, rax                ; restore stack pointer
        mov         [r12], rdi              ; write value to data stack
        add         r12, 8                  ; bump data stack
        jump_to_next_instruction

i_push_to_ret_stack:
        drop_data_stack 1
        bump_return_stack 1
        mov         rax, [r12]              ; top of the data stack
        mov         [r13 - 8], rax          ; save value
        jump_to_next_instruction
        
i_pop_from_ret_stack:
        drop_return_stack 1
        bump_data_stack 1
        mov         rax, [r13]              ; restore value
        mov         [r12 - 8], rax          ; write to stack
        jump_to_next_instruction

i_return:
        drop_return_stack 1
        mov         r14, [r13]              ; restore return pointer
        jump_to_next_instruction

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
        syscall
        mov         [r12], rax
        bump_data_stack 1
        jump_to_next_instruction

i_ret_stack_top:
        bump_data_stack 1
        mov         [r12 - 8*1], r13
        jump_to_next_instruction

i_stack_depth:
        mov         rax, r12
        sub         rax, data_stack
        cqo                             ; mov sign-extend of rax to rdx
        mov         r9, 8
        idiv        r9                  ; divide rdx:rax by 8

        bump_data_stack 1
        mov         [r12 - 8*1], rax    ; rax is the quotient
        jump_to_next_instruction

i_ret_stack_depth:
        mov         rax, r13
        sub         rax, return_stack
        cqo                             ; mov sign-extend of rax to rdx
        mov         r9, 8
        idiv        r9                  ; divide rdx:rax by 8

        bump_data_stack 1
        mov         [r12 - 8*1], rax    ; rax is the quotient
        jump_to_next_instruction

i_dup:
        drop_data_stack 1
        mov         rax,  [r12 + 8*0]
        bump_data_stack 2
        mov         [r12 - 8*1], rax
        mov         [r12 - 8*2], rax
        jump_to_next_instruction

i_over:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]
        mov         rdi,  [r12 + 8*0]
        bump_data_stack 3
        mov         [r12 - 8*1], rdi
        mov         [r12 - 8*2], rax
        mov         [r12 - 8*3], rdi
        jump_to_next_instruction

i_2dup:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]
        mov         rdi,  [r12 + 8*0]
        bump_data_stack 4
        mov         [r12 - 8*1], rax
        mov         [r12 - 8*2], rdi
        mov         [r12 - 8*3], rax
        mov         [r12 - 8*4], rdi
        jump_to_next_instruction

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
        jump_to_next_instruction

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
        jump_to_next_instruction

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
        jump_to_next_instruction

i_swap:
        drop_data_stack 2
        mov         rax,  [r12 + 8*1]
        mov         rdi,  [r12 + 8*0]
        bump_data_stack 2
        mov         [r12 - 8*1], rdi
        mov         [r12 - 8*2], rax
        jump_to_next_instruction
                
i_drop:
        drop_data_stack 1
        jump_to_next_instruction
                
i_push_to_stack:
        bump_data_stack 1
        mov         rax, [r14]              ; next word is value
        mov         [r12 - 8], rax          ; write to stack
        sub         r14, 8                  ; move instruction pointer to the next instruction
        jump_to_next_instruction

i_argc:
        bump_data_stack 1
        mov         rax, [rsp]
        mov         [r12 - 8], rax
        jump_to_next_instruction
i_argv:
        bump_data_stack 1
        mov         rax, [rsp + 8]
        mov         [r12 - 8], rax
        jump_to_next_instruction

_start: 
        mov         r15, rdi
        mov         rax, f_default_sigsegv_handler
        mov         QWORD [sigsegv_handler_pointer], rax
        mov         QWORD [the_dictionary], 0
        ; init segfault handler
        mov         rax, 13 ; sigaction syscall
        mov         rdi, 11 ; sigsegv signal
        mov         rsi, sigaction ; sigaction struct
        mov         rdx, 0 ; old sigaction struct
        mov         r10, 8 ; sigmask size
        syscall
        mov         r14, f_start
        mov         r12, data_stack
        mov         r13, return_stack
        jump_to_next_instruction

        section   .data


%macro  def_nl_terminated_static_string 2
%1:
        db        %2, 10      ; note the newline at the end
%1_size: equ $-%1
%endmacro

%macro  def_static_string 2
%1:
        db        %2
%1_size: equ $-%1
%endmacro

def_static_string ss_true, "true"
def_static_string ss_false, "false"

def_nl_terminated_static_string ss_newline, ""


def_nl_terminated_static_string ss_test_atoi, "-123"

def_static_string ss_syscall_error, "syscall error: "
def_nl_terminated_static_string ss_panic, "panic"
def_nl_terminated_static_string ss_debug, "debug"
def_nl_terminated_static_string ss_hello_world, "Hello, World"

def_nl_terminated_static_string ss_data_stack_overflow, "Data stack overflow"
def_nl_terminated_static_string ss_data_stack_underflow, "Data stack underflow"

def_nl_terminated_static_string ss_return_stack_overflow, "Return stack overflow"
def_nl_terminated_static_string ss_return_stack_underflow, "Return stack underflow"

def_nl_terminated_static_string ss_perm_mem_exausted, "Perm mem is exausted, increase perm_mem_size"

def_nl_terminated_static_string ss_sigsegv_handler_msg, "Segmentation fault, to set sigsegv handler use sigsegv_handler_pointer constant"

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

        dq          call(f_exit), val(139), call(f_print_buffer), val(ss_sigsegv_handler_msg), val(ss_sigsegv_handler_msg_size)
f_default_sigsegv_handler: equ     $-8

; <bool function> <true function> <false function>
; if function
f_if_end:
        dq          i_indirect_jmp
        dq          i_value_if
        dq          i_drop, i_pop_from_ret_stack                ; drop bool f
        dq          i_pop_from_ret_stack                        ; true f
        dq          i_pop_from_ret_stack                        ; false f
        dq          i_indirect_call, i_peek_ret_stack, val(3)   ; bool value
        dq          i_push_to_ret_stack                         ; false f -> ret stack
        dq          i_push_to_ret_stack                         ; true f -> ret stack
        dq          i_push_to_ret_stack                         ; bool f -> ret stack
f_if: equ     $-8
        dq          f_if_end

; cond_end [cond_default <func>] cond_when <bool func 1> <func 1> cond_when <bool func 2> <func 2> cond_when ... cond_start

f_cond_when_end:
        dq          i_return,
        dq          val(0)  ; false
        dq          i_drop  ; drop <func>
f_cond_when__run_branch_not_run: equ     $-8
        dq          i_return,
        dq          val(1)  ; true
        dq          i_indirect_call
f_cond_when__run_branch_run: equ     $-8
        dq          i_return,
        dq          call(f_if), val(f_id), val(f_cond_when__run_branch_run), val(f_cond_when__run_branch_not_run)   ; <bool>
        dq          i_swap                  ; <bool> <func>
        dq          i_pop_from_ret_stack    ; <func> <bool>
        dq          i_indirect_call
        dq          i_push_to_ret_stack     ; <bool func>
        dq          i_swap                  ; <func> <bool func>
        dq          i_drop                  ; <bool func> <func>
f_cond_when__run_branch: equ     $-8
        dq          i_return,
        dq          i_drop
        dq          i_drop
        dq          i_rev_rot
f_cond_when__drop_funcs: equ     $-8
        dq          i_return,
        dq          i_not, i_dup
f_cond_when__is_false: equ     $-8
        dq          i_return,
        dq          call(f_if), val(f_cond_when__is_false), val(f_cond_when__run_branch), val(f_cond_when__drop_funcs) ; <branch was found bool>
        dq          i_rot                   ; <branch was found bool> <bool func> <func>
f_cond_when: equ     $-8
        dq          f_cond_when_end,

        dq          i_return,
        dq          i_not
f_cond_default__is_false: equ     $-8
        dq          i_return,
        dq          val(1), ; true
        dq          call(f_if), val(f_cond_default__is_false), i_swap, val(f_id)
f_cond_default: equ     $-8

f_cond_start_end:
        dq          i_return,
        dq          val(0) ; false
f_cond_start: equ     $-8
        dq          f_cond_start_end

f_cond_end_end:
        dq          i_return,
        dq          i_drop ; drop bool
f_cond_end: equ     $-8
        dq          f_cond_end_end,


; <cond function> <body function>
; while function
f_while_first_ptr:
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
        dq          f_while_first_ptr


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
f_read_from_std_in_end:
        dq          i_return, i_syscall, 
        dq          val(0), val(0)              ; syscall num, fd
        dq          i_pop_from_ret_stack        ; buffer pointer
        dq          i_pop_from_ret_stack        ; count
        dq          val(1), val(1), val(1)      ; filler
        dq          i_push_to_ret_stack         ; count
        dq          i_push_to_ret_stack         ; buffer pointer
f_read_from_std_in: equ     $-8
        dq          f_read_from_std_in_end

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

; <addr> <old size> <new size> -> <new addr>
; remap virtual memory pages
        dq          i_return, i_syscall,
        dq          val(25)                     ; mremap syscall num
        dq          i_pop_from_ret_stack        ; address
        dq          i_pop_from_ret_stack        ; old size
        dq          i_pop_from_ret_stack        ; new size
        dq          val(1)                      ; MREMAP_MAYMOVE
        dq          val(0)                      ; new addr filler
        dq          val(0)
        dq          i_push_to_ret_stack         ; new size
        dq          i_push_to_ret_stack         ; old size
        dq          i_push_to_ret_stack         ; address
f_mremap: equ     $-8

; <size> -> <addr>
; malloc
f_malloc_end:
        dq          i_return
        dq          i_add, val(8)                       ; <addr>
        dq          i_write_mem_i64                     ; <addr>
        dq          i_rev_rot, i_dup                    ; <addr> <size> <addr>
        dq          call(f_log_syscall_error)           ; <addr> <size>
        dq          call(f_mmap_anon)                   ; <addr> <size>
        dq          i_dup, i_add, val(8)                ; <size> <size>
f_malloc: equ     $-8
        dq          f_malloc_end

; <size> -> <addr>
; malloc
f_perm_malloc_end:
        dq          i_return
        dq          i_write_mem_i64, val(perm_mem_ptr), i_add ; [size] [perm_mem_ptr] [perm_mem_ptr]
        dq          i_rot, i_dup, i_read_mem_i64, val(perm_mem_ptr)   ; [size]
        dq          call(f_panic_msg_if), val(ss_perm_mem_exausted), val(ss_perm_mem_exausted_size)  ; [perm_mem exausted?] [size]
        dq          i_less, val(perm_mem + perm_mem_size)   ; [size + perm_mem_ptr] [size]
        dq          i_add, i_read_mem_i64, val(perm_mem_ptr), i_dup  ; [size]
f_perm_malloc: equ     $-8
        dq          f_perm_malloc_end

; <addr> <new size> -> <addr>
; realloc
f_realloc_end:
        dq          i_return
        dq          i_add, val(8)                       ; <addr>
        dq          i_write_mem_i64                     ; <addr>
        dq          i_rev_rot, i_dup                    ; <addr> <new size> <addr>
        dq          call(f_log_syscall_error)           ; <addr> <new size>
        dq          call(f_mremap),                     ; <addr> <new size>
        dq          i_swap, i_read_mem_i64, i_dup       ; <orig addr> <old size> <new size> <new size>
        dq          i_add, val(-8)                      ; <orig addr> <new size> <new size>
        dq          i_rot, i_dup, i_add, val(8), i_swap ; <addr> <new size> <new size>
f_realloc: equ     $-8
        dq          f_realloc_end

; <addr>
; free
f_free_end:
        dq          i_return
        dq          i_drop, call(f_log_syscall_error)
        dq          call(f_munmap)                      ; <err code>
        dq          i_swap                              ; <addr> <size>
        dq          i_read_mem_i64, i_dup               ; <size> <addr>
        dq          i_add, val(-8)                      ; <addr>
f_free: equ     $-8
        dq          f_free_end


; exit
        dq          i_return, i_syscall, 
        dq          val(60)                     ; syscall num
        dq          i_pop_from_ret_stack                      ; exit code
        dq          val(1), val(1), val(1), val(1), val(1) ; filler
        dq          i_push_to_ret_stack
f_exit: equ     $-8

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


        dq          i_return
        dq          call(f_print_debug)
        dq          call(f_byte_vector_append_byte), i_swap, val(10), i_dup ; 'a'
        dq          call(f_byte_vector_append_byte), i_swap, val(97), i_dup ; '\n'
f_append_line: equ     $-8
        dq          i_return
        dq          call(f_echo_tokens)
        dq          call(f_exit_0)
        dq          call(f_byte_vector_destroy), i_pop_from_ret_stack
        dq              call(f_print_byte_vector), i_peek_ret_stack, val(1)
        dq              call(f_do_n_times), val(4099), val(f_append_line)
        dq              i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack, call(f_byte_vector_make)
f_test_byte_vector_append: equ     $-8



; log syscall error
        dq          i_return
        dq          i_and
        dq              i_less_or_equal, val(-4095), i_swap
        dq              i_greater_or_equal, val(-1)
        dq          i_dup, i_dup
f_log_syscall_error__is_error: equ     $-8
        dq          i_return,
        dq          call(f_print_newline)
        dq          call(f_print_number), i_mul, val(-1), i_dup
        dq          call(f_print_buffer), val(ss_syscall_error), val(ss_syscall_error_size)
f_log_syscall_error__log_error: equ     $-8
        dq          i_return,
        dq          call(f_if), val(f_log_syscall_error__is_error), val(f_log_syscall_error__log_error), val(f_id)
f_log_syscall_error: equ     $-8


; make byte vector [size, capacity, pointer]
; -> <addr>
        dq          i_return,
        dq          i_write_mem_i64, i_swap, call(f_malloc), val(1)         ; <addr>  write pointer
        dq          i_add, val(8 * 2), i_dup                                ; <pointer addr> <addr>
        dq          i_write_mem_i64, i_swap, val(4098), i_add, val(8), i_dup   ; <cap addr> <addr>  write capacity
        dq          i_write_mem_i64, i_swap, val(0), i_dup                  ; <size addr> <addr> write size
        dq          call(f_malloc), val(8 * 3)                              ; <addr>
f_byte_vector_make: equ     $-8

; copy byte vector to perm_mem 
; <vector-> -> <vector>
        dq          i_return,
        dq          i_drop, i_pop_from_ret_stack, i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(16), i_peek_ret_stack_first ; [perm buffer]
        dq          i_memcopy, call(f_byte_vector_size), i_peek_ret_stack_first, call(f_byte_vector_pointer), i_peek_ret_stack, val(2), i_dup ; [perm buffer]
        dq          call(f_perm_malloc), call(f_byte_vector_size), i_peek_ret_stack, val(2) ;
        dq          i_write_mem_i64, i_add, val(8), i_peek_ret_stack_first, call(f_byte_vector_size), i_peek_ret_stack, val(2) ; set cap = size
        dq          i_write_mem_i64, i_peek_ret_stack_first, call(f_byte_vector_size), i_peek_ret_stack, val(2)
        dq          i_push_to_ret_stack, call(f_perm_malloc), val(8 * 3)
        dq          i_push_to_ret_stack ; [vector]
f_byte_vector_copy_to_perm: equ     $-8
; byte vector size
; <addr> -> <n>
        dq          i_return, i_read_mem_i64
f_byte_vector_size: equ     $-8

; byte vector capacity
; <addr> -> <n>
        dq          i_return, i_read_mem_i64, i_add, val(8)
f_byte_vector_capacity: equ     $-8

; byte vector get byte
; <vector> <n> -> <value>
        dq          i_return
        dq          i_or                            ; <bool>
        dq              i_greater, val(0), i_swap   ; <bool> <bool>
        dq              i_greater_or_equal          ; <bool> <n>
        dq          i_rev_rot, i_dup                ; <n> <size> <n>
f_byte_vector_get__is_not_in_range: equ     $-8
        dq          i_return, i_read_mem_byte
        dq          i_add                               ; <value pointer>
        dq          call(f_byte_vector_pointer), i_swap ; <pointer> <n>
        dq          call(f_panic_if), val(f_byte_vector_get__is_not_in_range) ; <n> <vector>
        dq          i_rev_rot, i_dup                    ; <n> <size> <n> <vector>
        dq          i_rot                               ; <n> <size> <vector>
        dq          call(f_byte_vector_size), i_dup     ; <size> <vector> <n>
f_byte_vector_get: equ     $-8

; byte vector set byte
; <addr> <n> <value>
        dq          i_return
        dq          i_or                           ; <bool>
        dq              i_greater, val(0), i_swap   ; <bool> <bool>
        dq              i_greater_or_equal          ; <bool> <n>
        dq          i_rev_rot, i_dup                ; <n> <size> <n>
        dq          i_swap                          ; <n> <size>
f_byte_vector_set__is_not_in_range: equ     $-8
        dq          i_return, i_write_mem_byte
        dq          i_add                       ; <value addr> <value>
        dq          i_read_mem_i64              ; <array addr> <n> <value>
        dq          i_add, val(8 * 2)           ; <pointer addr> <n> <value>
        dq          i_swap                      ; <addr> <n> <value>
        dq          call(f_panic_if), val(f_byte_vector_set__is_not_in_range) ; <n> <addr> <value>
        dq          i_rot                       ; <size> <n> <n> <addr> <value>
        dq          i_dup                       ; <n> <n> <size> <addr> <value>
        dq          i_rot                       ; <n> <size> <addr> <value>
        dq          i_read_mem_i64, i_dup       ; <size> <addr> <n> <value>
f_byte_vector_set: equ     $-8

; byte vector append byte
; <addr> <byte value>
        dq          i_return
        dq          i_write_mem_i64, i_add, val(8 * 2), i_over              ; <addr> <value>
        dq          call(f_realloc)                                         ; <new array addr> <addr> <value>
        dq          i_read_mem_i64, i_add, val(8*2), i_over                 ; <array addr> <capacity * 2> <addr> <value> write capacity
        dq          i_write_mem_i64, i_add, val(8), i_swap, i_2dup          ; <capacity * 2> <addr> <value> write capacity
        dq          i_mul, val(2)                                           ; <capacity * 2> <addr> <value>
        dq          i_read_mem_i64, i_add, val(8), i_dup                    ; <capacity> <addr> <value>
f_byte_vector_append_byte__double_capacity: equ     $-8
        dq          i_return, i_equal
f_byte_vector_append_byte__is_equal: equ     $-8
        dq          i_return
        dq          i_write_mem_byte                                        ;
        dq          i_add, i_add, val(-1)                                   ; <value addr> <value>
        dq          i_read_mem_i64, i_add, val(8 * 2), i_swap               ; <array addr> <size> <value>
        dq          i_read_mem_i64, i_dup                                   ; <size> <addr> <value>
        dq          i_write_mem_i64, i_over                                 ; <addr> <value>
        dq          i_add, val(1)                                           ; <size + 1> <addr> <value>
        dq          i_read_mem_i64, i_dup                                   ; <size> <addr> <value>
        dq          call(f_if), val(f_byte_vector_append_byte__is_equal)         ; <addr> <value>
        dq          val(f_byte_vector_append_byte__double_capacity), val(f_id)
        dq          i_read_mem_i64, i_add, val(8), i_over                   ; <capacity> <size> <addr> <value>
        dq          i_read_mem_i64, i_dup                                   ; <size> <addr> <value>
f_byte_vector_append_byte: equ     $-8

; <vector> <i64 value>
        dq          i_return
        dq          i_write_mem_i64
        dq          i_add, i_add, val(-8)     ; <pointer> <size> <value>
        dq          call(f_byte_vector_pointer), i_swap     ; <pointer> <size> <value>
        dq          call(f_byte_vector_size), i_dup ; <size> <vector> <value>
        dq          call(f_byte_vector_append_byte), call(f_byte_vector_append_byte), i_2dup
        dq          call(f_byte_vector_append_byte), i_2dup, call(f_byte_vector_append_byte), i_2dup
        dq          call(f_byte_vector_append_byte), i_2dup, call(f_byte_vector_append_byte), i_2dup
        dq          call(f_byte_vector_append_byte), i_2dup, call(f_byte_vector_append_byte), i_2dup
        dq          i_swap, val(0), i_dup           ; <vector> <0 byte> <vector> <value>
f_byte_vector_append_i64: equ     $-8

; byte vector pointer
; <addr> -> <array addr>
        dq          i_return
        dq          i_read_mem_i64, i_add, val(8 * 2)
f_byte_vector_pointer: equ     $-8
        dq          i_return
        dq          call(f_free), call(f_free)
        dq          i_read_mem_i64, i_add, val(8 * 2), i_dup                ; <array pointer> <addr>
f_byte_vector_destroy: equ     $-8

; write byte vector to stdout
; <addr>
f_print_byte_vector_end:
        dq          i_return
        dq          i_drop
        dq          call(f_print_buffer)
        dq          i_read_mem_i64, i_add, val(8*2), i_over ; <array pointer> <size> <addr>
        dq          call(f_byte_vector_size), i_dup       ; <size> <addr>
f_print_byte_vector: equ     $-8
        dq          f_print_newline_end

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

; <number> -> <number string size>
; loop invariant <number> <string size>
        dq          i_return
        dq          i_swap, i_add, val(1), i_swap
        dq          i_div, i_swap, val(10)
f_i64_str_size__while_body: equ     $-8
        dq          i_return
        dq          i_greater, val(-9), i_dup
f_i64_str_size__less_than_minus_9: equ     $-8
        dq          i_return
        dq          i_mul, val(-1)
f_i64_str_size__if_non_neg: equ     $-8
        dq          i_return
        dq          i_swap, i_add, val(1), i_swap ; add 1 to str size for minus sign
f_i64_str_size__if_neg: equ     $-8
        dq          i_return
        dq          i_greater, val(0), i_dup
f_i64_str_size__is_negative: equ     $-8
        dq          i_return
        dq          i_drop      
        dq          call(f_while), val(f_i64_str_size__less_than_minus_9), val(f_i64_str_size__while_body)
        dq          call(f_if), val(f_i64_str_size__is_negative), val(f_i64_str_size__if_neg), val(f_i64_str_size__if_non_neg)
        dq          i_swap  ; <number> <string size>
        dq          val(1)
f_i64_str_size: equ     $-8


; number to string
; <number> <buffer address> <buffer size> -> <string size>

; loop invariant: <last digit address> <number> <string size>
        dq          i_return
        dq          i_mul, val(-1)
f_i64_to_string__negate: equ     $-8
        dq          i_return
        dq          i_greater, val(0), i_dup
f_i64_to_string__is_negative: equ     $-8
        dq          i_return
        dq          i_write_mem_byte, i_swap, val(45), i_over ; write '-'
f_i64_to_string__if_negative_case: equ     $-8
        dq          i_return
        dq          i_write_mem_byte, i_swap, val(48), i_over ; write '0'
f_i64_to_string__if_positive_case: equ     $-8
        dq          i_return
        dq          i_swap, i_div, i_swap, val(10), i_swap ; <last digit address> <number>
        dq          i_add, val(-1) ; <last digit address> <number>
        dq          i_write_mem_byte, i_over ; <last digit address> <number>
        dq          i_add, val(48), i_mul, val(-1), i_mod, i_swap, val(10) ; <last digit> <last digit address> <number>
        dq          i_over
f_i64_to_string__write_digit: equ     $-8
        dq          i_return
        dq          i_not, i_equal, val(0)
        dq          i_over
f_i64_to_string__number_is_not_zero: equ     $-8
        dq          i_return
        dq          i_greater
f_i64_to_string__greater: equ     $-8
        dq          i_return
        dq          i_drop, i_drop
        dq          call(f_while), val(f_i64_to_string__number_is_not_zero), val(f_i64_to_string__write_digit)

        dq          i_swap ; <last digit address> <number> <string size>
        dq          call(f_if), val(f_i64_to_string__is_negative)
        dq              val(f_id)
        dq              val(f_i64_to_string__negate)
        dq          i_swap

        dq          i_rev_rot, call(f_i64_str_size), i_over ; ; <last digit address> <number> <string size>

        dq          i_add, i_add, val(-1) ; <last digit address> <number>
        dq          i_rot ; <buffer address> <number str size> <number>
        dq          call(f_i64_str_size), i_dup ; <number str size> <number> <buffer address>
        dq          call(f_if), val(f_i64_to_string__is_negative) ; <number> <buffer address>
        dq              val(f_i64_to_string__if_negative_case)
        dq              val(f_i64_to_string__if_positive_case)
        dq          i_write_mem_byte, i_swap, val(48), i_over ; <number> <buffer address>
        dq          call(f_panic_if), val(f_i64_to_string__greater), ; <number> <buffer address>
        dq              call(f_i64_str_size)
        dq              i_over ;  <number> <buffer size> <number> <buffer address>
        dq          i_rot ; <buffer size> <number> <buffer address>
f_i64_to_string: equ     $-8

; -> <scanner>
; scanner_make
        dq          i_return
        dq          i_drop, call(f_read_from_std_in), i_over, val(1)
        dq          call(f_malloc), val(1)
f_scanner_make: equ     $-8
; <scanner> -> <byte>
; scanner_peek
        dq          i_return
        dq          i_read_mem_byte
f_scanner_peek: equ     $-8
; <scanner>
; scanner_advance
        dq          i_return
        dq          i_drop, call(f_read_from_std_in), i_swap, val(1)
f_scanner_advance: equ     $-8

; next_token number or word
; <scanner> -> <token byte vector>
; loop invariant: <token byte vector> <scanner>
        dq          i_return
        dq          i_equal, val(92) ; '\'
        dq          call(f_scanner_peek), i_over           ; <byte> <token byte vector> <scanner>
f_read_next_token__is_backslash: equ     $-8
        dq          i_return
        dq          i_equal, val(34) ; '"'
        dq          call(f_scanner_peek), i_over           ; <byte> <token byte vector> <scanner>
f_read_next_token__is_quote: equ     $-8
        dq          i_return
        dq          i_and
        dq          i_not, i_equal, val(10), i_swap ; '\n'
        dq          i_not, i_equal, val(34), i_dup ; '"'
        dq          call(f_scanner_peek), i_over           ; <byte> <token byte vector> <scanner>
f_read_next_token__is_not_quote_or_newline: equ     $-8
        dq          i_return
        dq          i_not, call(f_is_whitespace)
        dq          call(f_scanner_peek), i_over           ; <byte> <token byte vector> <scanner>
f_read_next_token__is_not_whitespace: equ     $-8
        dq          i_return
        dq          call(f_scanner_advance), i_over        ; <token byte vector> <scanner>
        dq          call(f_byte_vector_append_byte), i_over     ; <token byte vector> <scanner>
        dq          call(f_scanner_peek), i_over           ; <byte> <token byte vector> <scanner>
f_read_next_token__read_next_byte: equ     $-8
        dq          i_return
        dq          call(f_read_next_token__read_next_byte)
        dq          call(f_if), val(f_read_next_token__is_backslash), val(f_read_next_token__read_next_byte), val(f_id)
f_read_next_token__read_next_byte_with_escape: equ     $-8
        dq          i_return
        dq          call(f_while), val(f_read_next_token__is_not_whitespace), val(f_read_next_token__read_next_byte)
f_read_next_token__read_non_string_literal: equ     $-8
        dq          i_return
        dq          call(f_if), val(f_read_next_token__is_quote), val(f_read_next_token__read_next_byte), val(f_id) ; if last char is '"', then put it into vector, otherwise do nothing (invalid token)
        dq          call(f_while), val(f_read_next_token__is_not_quote_or_newline), val(f_read_next_token__read_next_byte_with_escape)
        dq          call(f_read_next_token__read_next_byte_with_escape)
f_read_next_token__read_string_literal: equ     $-8
f_read_next_token_end:
        dq          i_return
        dq          i_drop, i_swap
        dq          call(f_if), val(f_read_next_token__is_quote), val(f_read_next_token__read_string_literal), val(f_read_next_token__read_non_string_literal)
        dq          call(f_byte_vector_make)                ; <token byte vector> <scanner>
        dq          call(f_skip_comments_and_whitespace), i_dup
f_read_next_token: equ     $-8
        dq          f_read_next_token_end
; <byte> -> <bool>
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack
        dq          i_or
        dq              i_equal, val(32), i_peek_ret_stack, val(1) ; ' '
        dq          i_or
        dq              i_equal, val(9), i_peek_ret_stack, val(1)  ; '\t'
        dq          i_or
        dq              i_equal, val(10), i_peek_ret_stack, val(1) ; '\n'
        dq              i_equal, val(13), i_peek_ret_stack, val(1) ; '\r'
        dq          i_push_to_ret_stack
f_is_whitespace: equ     $-8

; skip whitespace
; loop invariant: <scanner>
; <scanner> ->
        dq          i_return
        dq          call(f_is_whitespace)
        dq          call(f_scanner_peek), i_dup           ; <byte> <scanner>
f_skip_whitespace__is_whitespace: equ     $-8
        dq          i_return
        dq          call(f_scanner_advance), i_dup        ; <scanner>
f_skip_whitespace__read_next_byte: equ     $-8
        dq          i_return
        dq          i_drop
        dq          call(f_while), val(f_skip_whitespace__is_whitespace), val(f_skip_whitespace__read_next_byte)
f_skip_whitespace: equ     $-8

; skip comments and whitespace
; loop invariant: <scanner>
; <scanner> ->
        dq          i_return
        dq          i_not, i_equal, val(10)
        dq          call(f_scanner_peek), i_dup           ; <byte> <scanner>
f_skip_comments_and_whitespace__is_not_newline: equ     $-8
        dq          i_return
        dq          i_equal, val(35)
        dq          call(f_scanner_peek), i_dup           ; <byte> <scanner>
f_skip_comments_and_whitespace__is_hash_sign: equ     $-8
        dq          i_return
        dq          call(f_scanner_advance), i_dup        ; <scanner>
f_skip_comments_and_whitespace__read_next_byte: equ     $-8
        dq          i_return
        dq          call(f_skip_whitespace), i_dup
        dq          call(f_while), val(f_skip_comments_and_whitespace__is_not_newline), val(f_skip_comments_and_whitespace__read_next_byte)
f_skip_comments_and_whitespace__loop: equ     $-8
        dq          i_return
        dq          i_drop
        dq          call(f_while), val(f_skip_comments_and_whitespace__is_hash_sign), val(f_skip_comments_and_whitespace__loop)
        dq          call(f_skip_whitespace), i_dup
f_skip_comments_and_whitespace: equ     $-8

; tests if token is a number
; <token byte vector> -> <bool>
        dq          i_return
        dq          call(f_free), i_pop_from_ret_stack              ; <bool>
        dq          i_equal, val(0), call(f_memcmp)                 ; <bool>
        dq              i_peek_ret_stack, val(1)                    ; <number str addr> <token addr> <number str size>
        dq              call(f_byte_vector_pointer)                 ; <token addr> <number str size>
        dq          i_swap                                          ; <token> <number str size>
        dq          i_drop, call(f_i64_to_string),                  ; <number str size> <token>
        dq              i_swap, i_peek_ret_stack, val(1)            ; <number> <temp buffer> <number str size> <number str size> <token>
        dq              i_rot, i_dup                                ; <number> <number str size> <number str size> <token>
        dq          i_push_to_ret_stack, call(f_malloc), i_dup      ; <number str size> <number> <token>
        dq          i_drop                                          ; <number str size> <number> <token>
f_is_number_token__check_string: equ     $-8
        dq          i_return
        dq          i_drop, i_swap                                  ; <bool>
        dq          i_drop, i_drop, i_rev_rot                       ; <bool> <token>
f_is_number_token__drop_all_but_bool: equ     $-8
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack                     ; token
        dq          call(f_if), val(f_id), val(f_is_number_token__check_string), val(f_is_number_token__drop_all_but_bool) ; <bool>
        dq          i_dup, i_equal                                         ; <bool> <bool> <number str size> <number> <token>
        dq              call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq              i_dup
        dq          i_rev_rot                                       ; <number str size> <number> <token>
        dq          i_peek_ret_stack, val(1)                        ; <token> <number str size> <number>
        dq          call(f_i64_str_size), i_dup                     ; <number str size> <number>
        dq          call(f_atoi)                                    ; <number>
        dq              call(f_byte_vector_pointer), i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack             ; token
f_is_number_token: equ     $-8

; tests if token is a string literal
; <token byte vector> -> <bool> <string vector>
; loop invariant: <string is OK bool> <n> <token vector> <string vector>
        dq          i_return
        dq          i_equal, val('"')
                                                        ; <n byte> <n> <token vector> <string vector>
f_string_literal_token_to_string__last_byte_case: equ     $-8
        dq          i_return
        dq          i_equal, i_add, val(-1)             ; <bool> <n byte> <n> <token vector> <string vector>
        dq          call(f_byte_vector_size)            ; <token size> <n> <n byte> <n> <token vector> <string vector>
        dq          i_dup_n, val(4)                     ; <token vector> <n> <n byte> <n> <token vector> <string vector>
        dq          i_over                              ; <n> <n byte> <n> <token vector> <string vector>
f_string_literal_token_to_string__is_last_byte: equ     $-8
        dq          i_return
        dq          i_equal, i_add, val(1)              ; <ok bool> <n> <token vector> <string vector>
        dq          i_over                              ; <n> <size> <n> <token vector> <string vector>
        dq          call(f_byte_vector_size), i_over    ; <size> <n> <token vector> <string vector>
        dq          i_drop                              ; <n> <token vector> <string vector>
f_string_literal_token_to_string__quote_case: equ     $-8
        dq          i_return
        dq          i_equal, val('"'), i_dup
f_string_literal_token_to_string__is_quote: equ     $-8
        dq          i_return
        dq          i_equal, val(92), i_dup
f_string_literal_token_to_string__is_backslash: equ     $-8
        dq          i_return
        dq          val(0)  ; <ok bool> <n> <token vector> <string vector>
        dq          i_drop  ; <n> <token vector> <string vector>
f_string_literal_token_to_string__backslash_case_bad_escape: equ     $-8
        dq          i_return
        dq          call(f_if), val(f_id)   ; <ok bool> <n + 1> <token vector> <string vector>
        dq          val(f_string_literal_token_to_string__case_append_byte)
        dq          val(f_string_literal_token_to_string__backslash_case_bad_escape)
        dq          i_and                                       ; <ok bool> <decoded byte> <n + 1> <token vector> <string vector>
        dq          i_greater_or_equal, i_add, val(-2)          ; <good n bool> <good escape bool> <decoded byte> <n + 1> <token vector> <string vector>
        dq          call(f_byte_vector_size)                    ; <token size> <n> <good escape bool> <decoded byte> <n + 1> <token vector> <string vector>
        dq          i_dup_n, val(5), i_dup_n, val(3)            ; <token vector> <n> <good escape bool> <decoded byte> <n + 1> <token vector> <string vector>
        dq          i_not, i_equal, val(-1), i_dup              ; <good escape bool> <decoded byte> <n + 1> <token vector> <string vector>
        dq          call(f_string_literal_escape_char_decode)   ; <decoded byte> <n + 1> <token vector> <string vector>
        dq          call(f_byte_vector_get) ; <n + 1 byte> <n + 1> <token vector> <string vector>
        dq          i_swap, i_2dup          ; <token vector> <n + 1> <n + 1> <token vector> <string vector>
        dq          i_add, val(1)           ; <n + 1> <token vector> <string vector>
        dq          i_drop                  ; <n> <token vector> <string vector>
f_string_literal_token_to_string__backslash_case: equ     $-8
        dq          i_return
        dq          val(1)                                      ; <ok bool> <n> <token vector> <string vector>
        dq          call(f_byte_vector_append_byte), i_dup_n, val(4) ; <n> <token vector> <string vector>
                                                                ; <n byte> <n> <token vector> <string vector>
f_string_literal_token_to_string__case_append_byte: equ     $-8
        dq          i_return
        dq          i_pop_from_ret_stack    ; <continue bool> <OK bool> <n> <vector>
        dq          i_pop_from_ret_stack    ; <OK bool> <n> <vector>
        dq          i_swap                  ; <n> <vector>
        dq          i_push_to_ret_stack     ; <vector> <n>
        dq          i_push_to_ret_stack     ; <OK bool> <vector> <n>
        dq          i_rev_rot               ; <continue bool> <OK bool> <vector> <n>
        dq          i_pop_from_ret_stack    ; <vector> <continue bool> <OK bool> <n>
        dq          i_and, i_over           ; <continue bool> <OK bool> <n>
        dq          i_greater               ; <bool> <OK bool> <n>
        dq              call(f_byte_vector_size), i_peek_ret_stack, val(1) ; <vector size> <n> <OK bool> <n>
        dq          i_over                  ; <n> <OK bool> <n>
        dq          i_push_to_ret_stack     ; <OK bool> <n>
        dq          i_rot                   ; <vector> <OK bool> <n>
; checks n < size and continue bool
f_string_literal_token_to_string__continue: equ     $-8
        dq          i_return
        dq          i_swap, i_add, val(1), i_swap   ; <ok bool> <n + 1> <token vector> <string vector>
        dq          call(f_cond_end)            ; <ok bool> <n> <token vector> <string vector>
        dq          call(f_cond_default), val(f_string_literal_token_to_string__case_append_byte)
        dq          call(f_cond_when), val(f_string_literal_token_to_string__is_last_byte), val(f_string_literal_token_to_string__last_byte_case)
        dq          call(f_cond_when), val(f_string_literal_token_to_string__is_quote), val(f_string_literal_token_to_string__quote_case)
        dq          call(f_cond_when), val(f_string_literal_token_to_string__is_backslash), val(f_string_literal_token_to_string__backslash_case)
        dq          call(f_cond_start)
        dq          call(f_byte_vector_get)     ; <n byte> <n> <token vector> <string vector>
        dq          i_swap, i_2dup              ; <token vector> <n> <n> <token vector> <string vector>
        dq          i_drop                      ; <n> <token vector> <string vector>
f_string_literal_token_to_string__loop_body: equ     $-8
f_string_literal_token_to_string_end:
        dq          i_return
        dq          i_drop, i_drop, i_rev_rot
        dq          call(f_while)           ; <ok bool> <n> <token vector> <string vector>
        dq          val(f_string_literal_token_to_string__continue), val(f_string_literal_token_to_string__loop_body)
        dq          i_equal                 ; <string is OK bool> <n> <token vector> <string vector>
        dq              val('"')
        dq              call(f_byte_vector_get), i_swap, val(0), i_over
        dq          val(1)                  ; <n> <vector>
        dq          i_swap
        dq          call(f_byte_vector_make)
f_string_literal_token_to_string: equ     $-8
        dq          f_string_literal_token_to_string_end

; checks bad string literal tokens
; <token vector> -> <bool>
f_is_bad_string_literal_token_end:
        dq          i_return
        dq          i_and
        dq          call(f_byte_vector_destroy), i_swap
        dq          i_not, call(f_string_literal_token_to_string), i_swap  ; <bool> <vector> <bool>
        dq          i_equal         ; <bool> <vector>
        dq              val('"')
        dq              call(f_byte_vector_get), i_swap, val(0), i_dup
f_is_bad_string_literal_token: equ     $-8
        dq          f_is_bad_string_literal_token_end

; decodes escape chars, returns -1 if not an escaped char
; <escaped char code>  -> <char>
        dq          i_return
        dq          val(-1), i_drop
f_string_literal_escape_char_decode__bad_char: equ     $-8
        dq          i_return
        dq          val(9), i_drop
f_string_literal_escape_char_decode__tab: equ     $-8
        dq          i_return
        dq          i_equal, val('t'), i_dup
f_string_literal_escape_char_decode__tab_check: equ     $-8
        dq          i_return
        dq          val(10), i_drop
f_string_literal_escape_char_decode__newline: equ     $-8
        dq          i_return
        dq          i_equal, val('n'), i_dup
f_string_literal_escape_char_decode__newline_check: equ     $-8
        dq          i_return
        dq          val(13), i_drop
f_string_literal_escape_char_decode__cr: equ     $-8
        dq          i_return
        dq          i_equal, val('r'), i_dup
f_string_literal_escape_char_decode__cr_check: equ     $-8
        dq          i_return
        dq          call(f_cond_end)
        dq          call(f_cond_default), val(f_string_literal_escape_char_decode__bad_char)
        dq          call(f_cond_when), val(f_string_literal_escape_char_decode__cr_check), val(f_string_literal_escape_char_decode__cr)
        dq          call(f_cond_when), val(f_string_literal_escape_char_decode__newline_check), val(f_string_literal_escape_char_decode__newline)
        dq          call(f_cond_when), val(f_string_literal_escape_char_decode__tab_check), val(f_string_literal_escape_char_decode__tab)
        dq          call(f_cond_start)
f_string_literal_escape_char_decode: equ     $-8

; tests if the token is a semicolon
; <token vector> -> <bool>
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack
        dq          i_and
        dq              i_equal, val(';'), call(f_byte_vector_get)
        dq                  i_peek_ret_stack, val(1)
        dq                  val(0)
        dq              i_equal, val(1), call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack
f_is_semicolon_token: equ     $-8

; tests if the token is an open paren
; <token vector> -> <bool>
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack
        dq          i_and
        dq              i_equal, val('{'), call(f_byte_vector_get)
        dq                  i_peek_ret_stack, val(1)
        dq                  val(0)
        dq              i_equal, val(1), call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack
f_is_open_paren_token: equ     $-8

; tests if the token is a close paren
; <token vector> -> <bool>
        dq          i_return
        dq          i_drop, i_pop_from_ret_stack
        dq          i_and
        dq              i_equal, val('}'), call(f_byte_vector_get)
        dq                  i_peek_ret_stack, val(1)
        dq                  val(0)
        dq              i_equal, val(1), call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack
f_is_close_paren_token: equ     $-8

; memcmp
; <addr1> <addr2> <n> -> <bool>
; loop invariant: <previous cmp> <addr1> <addr2> <n>
        dq          i_return,
        dq          i_and,
        dq              i_equal, val(0), i_over
        dq              i_not, i_equal, val(0), i_dup_n, val(4)
f_memcmp__equal_and_n_is_nonzero: equ     $-8
        dq          i_return,
        dq          i_swap, i_add, val(1), i_pop_from_ret_stack   ; <cmp> <addr 1 + 1> <addr 2 + 1> <n - 1>
        dq          i_swap, i_add, val(1), i_pop_from_ret_stack   ; <cmp> <addr 2 + 1> <n - 1>
        dq          i_swap, i_add, val(-1), i_swap                ; <cmp> <n - 1>
        dq          i_sub                                         ; <cmp> <n>
        dq              i_read_mem_byte, i_peek_ret_stack, val(2) ; addr 1
        dq              i_read_mem_byte, i_peek_ret_stack, val(1) ; addr 2
        dq          i_push_to_ret_stack     ; addr 2
        dq          i_push_to_ret_stack     ; addr 1
        dq          i_drop
f_memcmp__loop: equ     $-8
        dq          i_return,
        dq          i_drop, i_swap
        dq          i_drop, i_drop, i_rev_rot,
        dq          call(f_while), val(f_memcmp__equal_and_n_is_nonzero), val(f_memcmp__loop)
        dq          val(0)
        dq          call(f_panic_if), val(f_id), i_greater, val(0), i_dup_n, val(3)
f_memcmp: equ     $-8

; creates empty dictionary
; -> <dict>
        dq          i_return
        dq          i_write_mem_i64, i_swap, val(0)
        dq          i_dup                                       ; <pointer> <pointer>
        dq          call(f_malloc), val(8*3)
f_dictionary_make: equ     $-8

; creates dictionary record
; <name vector> <word def> <next> -> <dict record>
        dq          i_return
        dq          i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(16), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_add, val(8), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack, call(f_malloc), val(8*3)
f_dictionary_make_record: equ     $-8

; <dict record> -> <name>
        dq          i_return
        dq          i_read_mem_i64
f_dictionary_record_name: equ     $-8

; <dict record> <name> ->
        dq          i_return
        dq          i_write_mem_i64
f_dictionary_record_set_name: equ     $-8

; <dict record> -> <word def>
        dq          i_return
        dq          i_read_mem_i64, i_add, val(8)
f_dictionary_record_word_def: equ     $-8

; <dict record> -> <next>
        dq          i_return
        dq          i_read_mem_i64, i_add, val(16)
f_dictionary_record_next: equ     $-8

; <func pointer or instruction> <is_function bool> -> <word def>
        dq          i_return
        dq          i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(8), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack, call(f_malloc), val(16)
f_word_def_make: equ     $-8

; <word def> -> <func pointer or instruction>
        dq          i_return
        dq          i_read_mem_i64
f_word_def_func_pointer_or_inst: equ     $-8

; <word def> -> <is function bool>
        dq          i_return
        dq          i_read_mem_i64, i_add, val(8)
f_word_def_is_function: equ     $-8


; <vector> <vector> -> bool
        dq          i_return
        dq          i_equal
        dq          call(f_byte_vector_size), i_swap
        dq          call(f_byte_vector_size)
        dq          i_2dup
f_byte_vector_equals__size_is_equal: equ     $-8
        dq          i_return
        dq          val(0)
        dq          i_drop, i_drop
f_byte_vector_equals__case_not_equal_size: equ     $-8
        dq          i_return
        dq          i_equal, val(0), call(f_memcmp) ; <bool>
        dq          call(f_byte_vector_pointer), i_swap ; <pointer> <pointer> <size>
        dq          call(f_byte_vector_pointer)
        dq          i_rev_rot                       ; <vector> <vector> <size>
        dq          call(f_byte_vector_size), i_dup
f_byte_vector_equals__case_equal_size: equ     $-8
        dq          i_return
        dq          call(f_if), val(f_byte_vector_equals__size_is_equal)
        dq          val(f_byte_vector_equals__case_equal_size)
        dq          val(f_byte_vector_equals__case_not_equal_size)
f_byte_vector_equals: equ     $-8


; <first record pointer> <name vector> -> <record pointer or 0>
; if { = 0 dup } { drop swap } { if { vector_equals record_name 2dup } { drop swap } { dictionary_find_record dictionary_record_next } }
        dq          i_return
        dq          i_equal, val(0), i_dup
f_dictionary_find_record__equal_zero: equ     $-8
        dq          i_return
        dq          i_drop, i_swap
f_dictionary_find_record__case_zero: equ     $-8
        dq          f_if, i_jmp, val(f_dictionary_find_record__if_names_are_equal), val(f_dictionary_find_record__case_zero), val(f_dictionary_find_record__case_names_are_not_equal)
f_dictionary_find_record__case_non_zero: equ     $-8
        dq          f_byte_vector_equals, i_jmp
        dq          call(f_dictionary_record_name)
        dq          i_2dup
f_dictionary_find_record__if_names_are_equal: equ     $-8
        dq          f_dictionary_find_record_loop, i_jmp
        dq          call(f_dictionary_record_next)
f_dictionary_find_record__case_names_are_not_equal: equ     $-8
        dq          f_if, i_jmp, val(f_dictionary_find_record__equal_zero)
        dq          val(f_dictionary_find_record__case_zero), val(f_dictionary_find_record__case_non_zero)
f_dictionary_find_record_loop: equ     $-8
f_dictionary_find_record_end:
        dq          i_return
        dq          call(f_dictionary_find_record_loop)
f_dictionary_find_record: equ     $-8
        dq          f_dictionary_find_record_end

; dictionary_add <dict> <name vector> <word def>
;                    # tries to find a record with the name
;                    # panics if name is already taken
;                    # adds new record to the front of the list
        dq          i_return
        dq          call(f_exit_0)
        dq          call(f_print_panic)
        dq          call(f_print_newline)
        dq          call(f_print_byte_vector)
f_dictionary_add__word_exists: equ     $-8
        dq          i_return
        dq          i_equal, val(0), i_dup
f_dictionary_add__equal_zero: equ     $-8
        dq          i_return
        dq          i_write_mem_i64, i_pop_from_ret_stack
        dq          call(f_dictionary_make_record) ; <record pointer>
        dq          i_rev_rot ; <name> <word def> <record pointer>
        dq          i_read_mem_i64, i_peek_ret_stack, val(1) ; <record pointer> <name> <word def>
        dq          i_push_to_ret_stack
        dq          i_rot ; <dict> <name> <word def>
        dq          call(f_if), val(f_id), val(f_dictionary_add__word_exists), val(f_id), i_not, i_equal, val(0) ; <name> <word def> <dict>
        dq          call(f_dictionary_find_record)  ; <found record pointer> <name> <word def> <dict>
        dq          i_swap, i_over  ; <record pointer> <name> <name> <word def> <dict>
        dq          i_read_mem_i64, i_dup_n, val(3)         ; <record pointer> <name> <word def> <dict>
        dq          call(f_byte_vector_copy_to_perm)   ; <name> <word def> <dict>
        dq          i_rev_rot   ; <name> <word def> <dict>
f_dictionary_add: equ     $-8


; panic_if function
        dq          call(f_exit_0), call(f_print_panic)
f_panic_if__do_panic: equ     $-8
        dq          i_return,
        dq          call(f_if), i_rot, val(f_panic_if__do_panic), val(f_id)
f_panic_if: equ     $-8

; panic_msg_if function
; <msg> <msg size> <bool?
        dq          i_return, i_drop, i_drop
f_panic_msg_if__do_not_panic: equ     $-8
        dq          call(f_exit_0), call(f_print_buffer)
f_panic_msg_if__do_panic: equ     $-8
        dq          i_return,
        dq          call(f_if), val(f_id), val(f_panic_msg_if__do_panic), val(f_panic_msg_if__do_not_panic)
        dq          i_rot
f_panic_msg_if: equ     $-8

; print newline
f_print_newline_end:
        dq          i_return, f_print_buffer, i_call, val(ss_newline), val(ss_newline_size)
f_print_newline: equ     $-8
        dq          f_print_newline_end

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
        dq          i_pop_from_ret_stack
        dq          i_indirect_call,
        dq          i_push_to_ret_stack,
        dq          i_dup
        dq          i_push_to_ret_stack,
f_do_n_times__body: equ     $-8
        dq          i_return,
        dq          i_drop, i_drop,
        dq          call(f_while), val(f_do_n_times__cond), val(f_do_n_times__body)
f_do_n_times: equ     $-8


; print boolean
f_print_bool_end:
        dq          i_return, val(ss_false), val(ss_false_size)
f_print_bool_false: equ     $-8
        dq          i_return, val(ss_true), val(ss_true_size)
f_print_bool_true: equ     $-8
        dq          i_return, f_print_buffer, i_call, call(f_if), val(f_id), val(f_print_bool_true), val(f_print_bool_false)
f_print_bool: equ     $-8
        dq          f_print_bool_end

; print number
f_print_number_end:
        dq              i_return
        dq              call(f_free), i_pop_from_ret_stack
        dq              call(f_print_buffer),
        dq                  i_peek_ret_stack, val(1)
        dq                  call(f_i64_to_string)
        dq                      i_rot
        dq                      i_peek_ret_stack, val(1)
        dq                      val(20)
        dq              i_push_to_ret_stack, call(f_malloc), val(20)
f_print_number: equ     $-8
        dq              f_print_number

; print char (byte)
f_write_byte_to_stdout_end:
        dq              i_return
        dq              call(f_free), i_pop_from_ret_stack
        dq              call(f_print_buffer),
        dq                  i_peek_ret_stack, val(1)
        dq                  val(1)
        dq                  i_write_mem_byte
        dq                      i_peek_ret_stack, val(1)
        dq              i_push_to_ret_stack, call(f_malloc), val(1)
f_write_byte_to_stdout: equ     $-8
        dq              f_write_byte_to_stdout_end

; print stack
; loop invariant: <index> <stack depth>
        dq              i_return
        dq              i_not, i_equal, i_2dup
f_print_data_stack__is_not_equal: equ     $-8
        dq              i_return
        dq              call(f_write_byte_to_stdout), val(32),
        dq              call(f_print_number), i_dup_n,
        dq              i_dup, i_add, val(1)
f_print_data_stack__print_number: equ     $-8
f_print_data_stack_end:
        dq              i_return
        dq              i_drop, i_drop
        dq              call(f_write_byte_to_stdout), val(10),
        dq              call(f_write_byte_to_stdout), val(59),
        dq              call(f_while), val(f_print_data_stack__is_not_equal), val(f_print_data_stack__print_number)
        dq              val(2) ; <index> <stack depth>
        dq              i_add, val(2), i_stack_depth ; <stack depth>
f_print_data_stack: equ     $-8
        dq              f_print_data_stack_end

; make byte vector from bytes
; <n> <byte 0> <byte 1> .. <byte n-1> -> <vector>
        dq              i_return
        dq              call(f_byte_vector_append_byte)
        dq              i_rev_rot    ; <vector> <byte 0> <vector> 
        dq              i_dup    ; <vector> <vector> <byte 0>
f_byte_vector_from_bytes__append_one_byte: equ     $-8
        dq              i_return
        dq              call(f_do_n_times)
        dq              i_swap, val(f_byte_vector_from_bytes__append_one_byte) ; <n> <func> <vector> <byte 0> ...
        dq              i_swap
        dq              call(f_byte_vector_make)
f_byte_vector_from_bytes: equ     $-8

; false function
f_false_end:
        dq          i_return, val(0)
f_false: equ     $-8
        dq          f_false_end
; true function
f_true_end:
        dq          i_return, val(1)
f_true: equ     $-8
        dq          f_true_end
; check print and exit
        dq          call(f_exit_0), call(f_print_bool), i_equal
f_check: equ     $-8

; echo tokens
; loop invariant: <scanner>
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(9), val(f_write_byte_to_stdout)
        dq          val('s'), val('e'), val('m'), val('i'), val('c'), val('o'), val('l'), val('o'), val('n')
f_echo_tokens__print_semicolon: equ     $-8
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(14), val(f_write_byte_to_stdout)
        dq          val('c'), val('l'), val('o'), val('s'), val('e'), val('d'), val(' ')
        dq          val('b'), val('r'), val('a'), val('c'), val('k'), val('e'), val('t')
f_echo_tokens__print_closed_bracket: equ     $-8
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(12), val(f_write_byte_to_stdout)
        dq          val('o'), val('p'), val('e'), val('n'), val(' ')
        dq          val('b'), val('r'), val('a'), val('c'), val('k'), val('e'), val('t')
f_echo_tokens__print_open_bracket: equ     $-8
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(6), val(f_write_byte_to_stdout)
        dq          val('n'), val('u'), val('m'), val('b'), val('e'), val('r')
f_echo_tokens__print_number: equ     $-8
        dq          i_return
        dq          call(f_byte_vector_destroy)
        dq          call(f_print_newline)
        dq          call(f_print_byte_vector), i_dup
        dq          i_drop
        dq          call(f_string_literal_token_to_string), i_dup
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(14), val(f_write_byte_to_stdout)
        dq          val('s'), val('t'), val('r'), val('i'), val('n'), val('g'), val(' '),
        dq          val('l'), val('i'), val('t'), val('e'), val('r'), val('a'), val('l')
f_echo_tokens__print_string_literal: equ     $-8
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(18), val(f_write_byte_to_stdout)
        dq          val('b'), val('a'), val('d'), val(' ')
        dq          val('s'), val('t'), val('r'), val('i'), val('n'), val('g'), val(' '),
        dq          val('l'), val('i'), val('t'), val('e'), val('r'), val('a'), val('l')
f_echo_tokens__print_bad_string_literal: equ     $-8
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_do_n_times), val(4), val(f_write_byte_to_stdout)
        dq          val('w'), val('o'), val('r'), val('d')
f_echo_tokens__print_word: equ     $-8
        dq          i_return
        dq          call(f_is_number_token), i_dup
f_echo_tokens__is_num: equ     $-8
        dq          i_return
        dq          call(f_byte_vector_destroy), i_swap, call(f_string_literal_token_to_string), i_dup
f_echo_tokens__is_string_literal: equ     $-8
        dq          i_return
        dq          call(f_is_bad_string_literal_token), i_dup
f_echo_tokens__is_bad_string_literal: equ     $-8
        dq          i_return
        dq          call(f_is_semicolon_token), i_dup
f_echo_tokens__is_semicolon: equ     $-8
        dq          i_return
        dq          call(f_byte_vector_destroy),

        dq          call(f_cond_end)
        dq          call(f_cond_default), val(f_echo_tokens__print_word)
        dq          call(f_cond_when), val(f_echo_tokens__is_bad_string_literal), val(f_echo_tokens__print_bad_string_literal)
        dq          call(f_cond_when), val(f_echo_tokens__is_string_literal), val(f_echo_tokens__print_string_literal)
        dq          call(f_cond_when), val(f_echo_tokens__is_num), val(f_echo_tokens__print_number)
        dq          call(f_cond_when), val(f_echo_tokens__is_semicolon), val(f_echo_tokens__print_semicolon)
        dq          call(f_cond_start)
        
        dq          call(f_print_newline)
        dq          call(f_print_byte_vector), i_dup
        dq          call(f_read_next_token), i_dup
f_echo_tokens_loop: equ     $-8
        dq          i_return
        dq          call(f_free)
        dq          call(f_while), val(f_true), val(f_echo_tokens_loop)
        dq          call(f_scanner_make)
f_echo_tokens: equ     $-8

; <value> -> <value func>
f_capture_end:
        dq          i_return
        dq          i_add, val(16), i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(0), i_peek_ret_stack, val(1), val(i_return)
        dq          i_write_mem_i64, i_add, val(8), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_add, val(16), i_peek_ret_stack, val(1), val(i_push_to_stack)
        dq          i_write_mem_i64, i_add, val(24), i_peek_ret_stack, val(1), i_peek_ret_stack, val(1) 
        dq          i_push_to_ret_stack, call(f_malloc), val(32)
f_capture: equ     $-8
        dq          f_capture_end

; <func 1> <func 2> -> <func>
f_func_concat_end:
        dq          i_return
        dq          i_add, val(8*4), i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(8*0), i_peek_ret_stack, val(1), val(i_return)
        dq          i_write_mem_i64, i_add, val(8*1), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_add, val(8*2), i_peek_ret_stack, val(1), val(i_call)
        dq          i_write_mem_i64, i_add, val(8*3), i_peek_ret_stack, val(1)
        dq          i_write_mem_i64, i_add, val(8*4), i_peek_ret_stack, val(1), val(i_call)
        dq          i_write_mem_i64, i_add, val(8*5), i_peek_ret_stack, val(1), i_peek_ret_stack, val(1) 
        dq          i_push_to_ret_stack, call(f_malloc), val(8*6)
        dq          i_swap
f_func_concat: equ     $-8
        dq          f_func_concat_end

; reads and compiles code
; <scanner> <dict> -> <ok bool> <code vector>
; loop invariant: <ok bool> <token> <scanner> <word def> <dict>
f_read_and_compile_code__emit_code_for_string_literal_end:
        dq          i_return                                ; [ok bool] [scanner] [code vector] [dict]
        dq          val(1)                                  ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_swap, val(i_push_to_stack), i_over ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_dup_n, val(3)         ; [string] [scanner] [code vector] [dict]
        dq          call(f_byte_vector_destroy), i_swap                     ; [string] [token] [scanner] [code vector] [dict]
        dq          i_drop, call(f_string_literal_token_to_string), i_dup   ; [token] [scanner] [code vector] [dict]
f_read_and_compile_code__emit_code_for_string_literal: equ     $-8
        dq          f_read_and_compile_code__emit_code_for_string_literal_end
        dq          i_return
        dq          i_equal
        dq              val('"')                                        ; [first byte] [token]
        dq              call(f_byte_vector_get), i_swap, val(0), i_dup  ; [token]
f_read_and_compile_code__is_string_literal: equ     $-8
        dq          i_return
        dq          val(0)                              ; [ok bool] [scanner] [code vector] [dict]
        dq          call(f_print_newline)
        dq          call(f_byte_vector_destroy)         ; [token] [scanner] [code vector] [dict]
        dq          call(f_print_byte_vector), i_dup    ; [token] [scanner] [code vector] [dict]
        dq          call(f_print_newline)
        dq          call(f_byte_vector_destroy), call(f_print_byte_vector), i_dup, call(f_byte_vector_from_bytes)
        dq          val(19), val('b'), val('a'), val('d'), val(' '), val('s'), val('t'), val('r'), val('i'), val('n'), val('g')  ; [token] [scanner] [code vector] [dict]
        dq          val(' '), val('l'), val('i'), val('t'), val('e'), val('r'), val('a'), val('l'), val(':')  ; [token] [scanner] [code vector] [dict]
f_read_and_compile_code__print_bad_string_literal: equ     $-8
        dq          i_return
        dq          call(f_is_bad_string_literal_token), i_dup
f_read_and_compile_code__is_bad_string_literal: equ     $-8
        dq          i_return
        dq          call(f_is_open_paren_token), i_dup
f_read_and_compile_code__is_open_paren: equ     $-8
        dq          i_return
        dq          call(f_is_number_token), i_dup
f_read_and_compile_code__is_num: equ     $-8
f_read_and_compile_code__emit_code_for_num_end:
        dq          i_return
        dq          val(1)                                  ; <ok bool> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64), i_swap, val(i_push_to_stack), i_over ; <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64), i_dup_n, val(3) ; <scanner> <code vector> <dict>
        dq          call(f_byte_vector_destroy), i_swap     ; <num> <scanner> <code vector> <dict>
        dq          call(f_atoi)                            ; <num> <token> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_pointer), i_over     ; <token pointer> <token size> <token> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_size), i_dup         ; <token size> <token> <scanner> <code vector> <dict>
f_read_and_compile_code__emit_code_for_num: equ     $-8
        dq          f_read_and_compile_code__emit_code_for_num_end
        dq          i_return                                    ; [ok bool] [scanner] [code vector] [dict]
        dq          i_pop_from_ret_stack                        ; [scanner] [code vector] [dict]
        dq          call(f_free), i_pop_from_ret_stack ; partially destroy sub vector [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_swap, val(i_push_to_stack), i_over ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_dup_n, val(3) ; [sub func pointer] [scanner] [code vector] [dict]
        dq          i_add, val(-16)                              ; [sub func pointer + 16] [scanner] [code vector] [dict] ; code vector has function end pointer as the last element
        dq          i_add
        dq              call(f_byte_vector_pointer), i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack, i_push_to_ret_stack    ; [sub ok bool] [sub code vector] [scanner] [code vector] [dict]
        dq          call(f_read_and_compile_code__sub)          ; [scanner] [dict] [scanner] [code vector] [dict]
        dq          i_over, i_dup_n, val(3)                     ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_destroy)                 ; drop open paren [token] [scanner] [code vector] [dict]
f_read_and_compile_code__emit_code_for_anon_func: equ     $-8
        dq          i_return
        dq          val(1)                          ; <ok bool> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64), i_swap, val(i_call), i_over  ; <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64)  ; <scanner> <code vector> <dict>
        dq          i_dup_n, val(3)     ; <code vector> <func or inst> <scanner> <code vector> <dict>
f_read_and_compile_code__emit_call: equ     $-8
        dq          i_return
        dq          val(1)                          ; <ok bool> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64)  ; <scanner> <code vector> <dict>
        dq          i_dup_n, val(3)     ; <code vector> <func or inst> <scanner> <code vector> <dict>
f_read_and_compile_code__emit_instruction: equ     $-8
        dq          i_return
        dq          call(f_if), val(f_id), val(f_read_and_compile_code__emit_call), val(f_read_and_compile_code__emit_instruction)
        dq          call(f_word_def_is_function), i_swap     ; <is func bool> <func or inst> <scanner> <code vector> <dict>
        dq          call(f_word_def_func_pointer_or_inst), i_dup     ; <func or inst> <word def> <scanner> <code vector> <dict>
        dq          call(f_dictionary_record_word_def)      ; <word def> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_destroy), i_swap      ; <record> <scanner> <code vector> <dict>
f_read_and_compile_code__emit_word: equ     $-8
        dq          i_return
        dq          val(1) ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_swap, val(i_late_bind_and_call_word), i_over  ; [scanner] [code vector] [dict]
        dq          call(f_byte_vector_append_i64), i_dup_n, val(3) ; [struct] [scanner] [code vector] [dict]
        dq          call(f_create_struct_dict_string), i_dup_n, val(4) ; [token] [scanner] [code vector] [dict]
        dq          i_drop ; drop null pointer ; [null record] [token] [scanner] [code vector] [dict]
f_read_and_compile_code__no_such_word_case: equ     $-8
f_read_and_compile_code__emit_code_for_word_end:
        dq          i_return
        dq          call(f_if), val(f_id), val(f_read_and_compile_code__no_such_word_case), val(f_read_and_compile_code__emit_word)
        dq          i_equal, val(0), i_dup                  ; <bool> <record pointer> <token> <scanner> <code vector> <dict>
        dq          call(f_dictionary_find_record)          ; <record pointer> <token> <scanner> <code vector> <dict>
        dq          i_swap, i_over                          ; <record pointer> <token> <token> <scanner> <code vector> <dict>
        dq          i_read_mem_i64                          ; <record pointer> <token> <scanner> <code vector> <dict>
        dq          i_dup_n, val(4)                         ; <dict> <token> <scanner> <code vector> <dict>
f_read_and_compile_code__emit_code_for_word: equ     $-8
        dq          f_read_and_compile_code__emit_code_for_word_end
        dq          i_return
        dq          call(f_print_newline)
        dq          call(f_byte_vector_destroy), call(f_print_byte_vector), i_dup, call(f_byte_vector_from_bytes)
        dq          val(9), val('b'), val('a'), val('d'), val(' '), val('p'), val('a'), val('r'), val('e'), val('n')
f_read_and_compile_code__print_unmatched_paren: equ     $-8
        dq          i_return                                        ; [bool] [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_and                                           ; [ok bool] [is not semicolon] [ok bool] [token] [scanner] [code vector] [dict]
        dq              i_over                                      ; [is not semicolon] [ok bool] [token] [scanner] [code vector] [dict]
        dq              i_not, call(f_is_semicolon_token), i_over   ; [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_and                                           ; [is not bad paren] [ok bool] [token] [scanner] [code vector] [dict]
        dq          call(f_if), val(f_id), val(f_id), val(f_read_and_compile_code__print_unmatched_paren), i_dup  ; [is not bad paren] [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_not, call(f_is_close_paren_token), i_over     ; [ok bool] [token] [scanner] [code vector] [dict]
f_read_and_compile_code__is_not_semicolon_and_fail_on_close_paren: equ     $-8
f_read_and_compile_code__emit_code_for_next_token_end:
        dq          i_return
        dq          i_swap, call(f_read_next_token)
        dq          i_over
        dq          call(f_cond_end) ; <continue bool> <scanner> <code vector> <dict>
        dq          call(f_cond_default), val(f_read_and_compile_code__emit_code_for_word)
        dq          call(f_cond_when), val(f_read_and_compile_code__is_num), val(f_read_and_compile_code__emit_code_for_num)
        dq          call(f_cond_when), val(f_read_and_compile_code__is_open_paren), val(f_read_and_compile_code__emit_code_for_anon_func)
        dq          call(f_cond_when), val(f_read_and_compile_code__is_string_literal), val(f_read_and_compile_code__emit_code_for_string_literal)
        dq          call(f_cond_when), val(f_read_and_compile_code__is_bad_string_literal), val(f_read_and_compile_code__print_bad_string_literal)
        dq          call(f_cond_start)
        dq          i_drop ; drop ok bool [ok bool] [token] [scanner] [code vector] [dict]
f_read_and_compile_code__emit_code_for_next_token: equ     $-8
        dq          f_read_and_compile_code__emit_code_for_next_token_end
        dq          i_return        ; [ok bool] [code vector]
        dq          i_drop, i_rot   ; [ok bool] [code vector] [dict]
        dq          i_drop, i_swap  ; [ok bool] [scanner] [code vector] [dict]
        dq          i_drop ; [scanner] [ok bool] [scanner] [code vector] [dict]
        dq          call(f_if), val(f_id), val(f_id), val(f_read_and_compile_code_skip_to_semicolon) ; [is semicolon] [scanner] [ok bool] [scanner] [code vector] [dict]
        dq          i_swap, i_dup_n, val(3)         ; [is semicolon] [ok bool] [scanner] [code vector] [dict]
        dq          call(f_byte_vector_destroy), i_swap     ; [is semicolon] [token] [ok bool] [scanner] [code vector] [dict]
        dq          call(f_is_semicolon_token), i_dup, i_swap   ; [ok bool] [token] [scanner] [code vector] [dict]
        dq          call(f_while)  ; <ok bool> <token> <scanner> <code vector> <dict>
        dq          val(f_read_and_compile_code__is_not_semicolon_and_fail_on_close_paren), val(f_read_and_compile_code__emit_code_for_next_token)
        dq          val(1)                                  ; <ok bool> <token> <scanner> <code vector> <dict>
        dq          call(f_read_next_token), i_dup, i_swap  ; <token> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64), i_swap, val(i_return), i_dup
        dq          call(f_byte_vector_make)                ; <code vector> <scanner> <dict> 
f_read_and_compile_code__main: equ     $-8
        dq          i_return                                        ; [bool] [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_and                                           ; [ok bool] [is not close paren] [ok bool] [token] [scanner] [code vector] [dict]
        dq              i_over                                      ; [is not close paren] [ok bool] [token] [scanner] [code vector] [dict]
        dq              i_not, call(f_is_close_paren_token), i_over ; [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_and                                           ; [is semicolon] [ok bool] [token] [scanner] [code vector] [dict]
        dq          call(f_if), val(f_id), val(f_id), val(f_read_and_compile_code__print_unmatched_paren), i_dup ; [is semicolon] [ok bool] [token] [scanner] [code vector] [dict]
        dq          i_not, call(f_is_semicolon_token), i_over       ; [ok bool] [token] [scanner] [code vector] [dict]
f_read_and_compile_code__check_not_close_paren_and_fail_on_semicolon: equ     $-8
        dq          i_return        ; [ok bool] [code vector]
        dq          i_swap, call(f_byte_vector_append_i64), i_swap, call(f_byte_vector_pointer), i_dup, i_dup    ; [code vector] [ok bool]
        dq          i_swap          ; [ok bool] [code vector]
        dq          i_drop, i_rot   ; [ok bool] [code vector] [dict]
        dq          i_drop, i_swap  ; [ok bool] [scanner] [code vector] [dict]
        dq          call(f_byte_vector_destroy), i_swap     ; [ok bool] [token] [scanner] [code vector] [dict]
        dq          call(f_while)                           ; <ok bool> <token> <scanner> <code vector> <dict>
        dq          val(f_read_and_compile_code__check_not_close_paren_and_fail_on_semicolon), val(f_read_and_compile_code__emit_code_for_next_token)
        dq          val(1)                                  ; <ok bool> <token> <scanner> <code vector> <dict>
        dq          call(f_read_next_token), i_dup, i_swap  ; <token> <scanner> <code vector> <dict>
        dq          call(f_byte_vector_append_i64), i_swap, val(i_return), i_dup
        dq          call(f_byte_vector_make)                ; <code vector> <scanner> <dict> 
f_read_and_compile_code__sub: equ     $-8
        dq          i_return
        dq          call(f_if), val(f_id), val(f_id), val(f_read_and_compile_code_skip_to_semicolon)  ; [is semicolon] [scanner]
        dq          call(f_byte_vector_destroy), i_swap     ; [is semicolon] [token] [scanner]
        dq          call(f_is_semicolon_token), i_dup       ; [token] [scanner]
        dq          call(f_read_next_token), i_dup          ; [scanner]
f_read_and_compile_code_skip_to_semicolon: equ     $-8
f_read_and_compile_code_end:
        dq          i_return
        dq          call(f_read_and_compile_code__main)
f_read_and_compile_code: equ     $-8
        dq          f_read_and_compile_code_end


; <dict> <string> -> <struct>
        dq          i_return
        dq          i_pop_from_ret_stack
        dq          i_write_mem_i64, i_add, val(8), i_peek_ret_stack_first ; [string]
        dq          i_write_mem_i64, i_peek_ret_stack_first ; [dict] [string]
        dq          i_push_to_ret_stack, call(f_malloc), val(16) ; [dict] [string]
f_create_struct_dict_string: equ     $-8

        dq          call(f_exit), val(1) 
        dq          call(f_print_newline)
        dq          call(f_print_byte_vector)
        dq          call(f_byte_vector_destroy), call(f_print_byte_vector), i_dup, call(f_byte_vector_from_bytes), val(10), val('b'), val('a'), val('d'), val(' '), val('w'), val('o'), val('r'), val('d'), val(':'), val(' ')
        dq          i_drop ; drop null pointer ; <token>
f_late_bind_and_call_word_no_such_word_case: equ     $-8
; <struct <dict> <string>> <instruction pointer> -> ... 
        dq          i_indirect_jmp; [func]
        dq          i_write_mem_i64, i_add, val(-8) ; [ip] [func] [func]
        dq          i_write_mem_i64, i_swap, val(i_call), i_dup ; [ip] [func] [func]
        dq          i_rot, i_dup ; [func] [ip]
        dq          call(f_word_def_func_pointer_or_inst), call(f_dictionary_record_word_def); [record] [ip]
        dq          i_drop, i_swap ; [record] [token] [ip]
        dq          call(f_if), val(f_id), val(f_late_bind_and_call_word_no_such_word_case), val(f_id), i_equal, val(0), i_dup ; [record] [token] [ip]
        dq          call(f_dictionary_find_record), i_swap, i_over ; [record] [token] [ip]
        dq          i_read_mem_i64 ; [dict] [string] [ip]
        dq          i_read_mem_i64, i_swap ; [string] [struct] [ip]
        dq          i_read_mem_i64, i_add, val(8), i_dup ; [struct] [ip]
f_late_bind_and_call_word: equ     $-8

; main loop
; <scanner> <dict> ->
; loop invariant: <scanner> <dict>
        dq          i_return
        dq          i_pop_from_ret_stack
        dq          i_pop_from_ret_stack
        dq          i_pop_from_ret_stack
        dq          i_indirect_call
        dq          i_add, val(-8)
        dq          i_add
        dq              call(f_byte_vector_pointer), i_peek_ret_stack, val(3)
        dq              call(f_byte_vector_size), i_peek_ret_stack, val(3)
        dq          i_push_to_ret_stack
        dq          i_push_to_ret_stack
        dq          i_push_to_ret_stack
f_f_read_compile_run_loop__run: equ     $-8
        dq          i_return
        dq          call(f_exit_0)
        dq          call(f_byte_vector_destroy), call(f_print_byte_vector), i_dup, call(f_byte_vector_from_bytes)
        dq          val(6), val('e'), val('r'), val('r'), val('o'), val('r'), val(10)
f_f_read_compile_run_loop__error: equ     $-8
        dq          i_return
        dq          call(f_byte_vector_destroy)     ; <scanner> <dict> TODO: destroy sub-functions
        dq          call(f_if), val(f_id), val(f_f_read_compile_run_loop__run), val(f_f_read_compile_run_loop__error)
        dq          call(f_read_and_compile_code)   ; <ok bool> <code vector> <scanner> <dict>
        dq          i_2dup
f_f_read_compile_run_loop__loop: equ     $-8
f_read_compile_run_loop_end:
        dq          i_return
        dq          call(f_while), val(f_true), val(f_f_read_compile_run_loop__loop)
f_read_compile_run_loop: equ     $-8
        dq          f_read_compile_run_loop_end

        dq          i_return
        dq          call(f_exit_0)
        dq          call(f_print_panic)
        dq          call(f_print_newline)
        dq          call(f_print_byte_vector)
f_rename_word_word_exists: equ     $-8
; <dict> <old name> <new name> ->
        dq          i_return
        dq          call(f_dictionary_record_set_name) ; [the record] [new name]
        dq          call(f_if), val(f_id), val(f_rename_word_word_exists), val(f_id), i_equal, val(0), i_dup ; [the record] [new name]
        dq          call(f_dictionary_find_record)  ; [record] [old name] [new name]
        dq          i_read_mem_i64  ; [dict] [old name] [new name]
f_rename_word: equ     $-8
        
; <dict> <name> <func> ->
        dq          i_return
        dq          call(f_dictionary_add)                  ; [dict] [name] [word_def]
        dq          i_rev_rot                               ; [word_def] [dict] [name]
        dq          call(f_word_def_make), i_swap, val(1)   ; [func] [dict] [name]
        dq          i_rot                                   ; [dict] [name] [func]
f_define_word: equ     $-8
        
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
        dq                  val(10)
        dq                  i_div, val(100), val(10)
        dq          i_and
        dq              i_equal
        dq                  val(-10)
        dq                  i_div, val(-100), val(10)
        dq          i_and
        dq              i_equal
        dq                  val(-9223372036854775807)
        dq                  i_div, val(9223372036854775807), val(-1)

        dq          i_and
        dq              i_equal
        dq                  val(0)
        dq                  i_mod, val(100), val(10)
        dq          i_and
        dq              i_equal
        dq                  val(-1)
        dq                  i_mod, val(-101), val(10)
        dq          i_and
        dq              i_equal
        dq                  val(-8)
        dq                  i_mod, val(-9223372036854775808), val(10)
        
        dq          i_and
        dq              i_equal
        dq                  val(101)
        dq                  i_read_mem_byte, i_add, val(1), val(ss_hello_world)
        dq          i_and
        dq              i_equal
        dq                  val(-123)
        dq                  call(f_atoi), val(ss_test_atoi), val(ss_test_atoi_size)
; test f_i64_str_size
        dq          i_and
        dq              i_equal
        dq                  val(3)
        dq                  call(f_i64_str_size), val(101)
        dq          i_and
        dq              i_equal
        dq                  val(4)
        dq                  call(f_i64_str_size), val(-101)
        dq          i_and
        dq              i_equal
        dq                  val(20)
        dq                  call(f_i64_str_size), val(-9223372036854775808)
        dq          i_and
        dq              i_equal
        dq                  val(1)
        dq                  call(f_i64_str_size), val(0)
        dq          i_and
        dq              i_equal
        dq                  val(19)
        dq                  call(f_i64_str_size), val(9223372036854775807)
; test f_i64_to_string <number> <buffer address> <buffer size>
%macro  test_i64_to_string__atoi__roundtrip 1
        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(%1)
        dq                  call(f_atoi), i_peek_ret_stack, val(1), val(100)
        dq              i_and
        dq                  i_equal
        dq                      call(f_i64_str_size), val(%1)
        dq                      call(f_i64_to_string), val(%1), i_peek_ret_stack, val(1), val(100)
        dq              i_push_to_ret_stack, call(f_malloc), val(100)
%endmacro
                    test_i64_to_string__atoi__roundtrip(0)
                    test_i64_to_string__atoi__roundtrip(100)
                    test_i64_to_string__atoi__roundtrip(9223372036854775807)
                    test_i64_to_string__atoi__roundtrip(-1)
                    test_i64_to_string__atoi__roundtrip(-11)
                    test_i64_to_string__atoi__roundtrip(-9223372036854775808)

        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(42)
        dq                  i_read_mem_i64, i_peek_ret_stack, val(1)
        dq                  i_drop, i_read_mem_i64, i_add, val(9000), i_peek_ret_stack, val(1)
        dq              i_push_to_ret_stack, call(f_realloc), i_pop_from_ret_stack, val(10000)
        ;dq                  i_write_mem_i64, i_add, val(9000) i_peek_ret_stack, val(1), val(42)
        dq                  i_write_mem_i64, i_peek_ret_stack, val(1), val(42)
        dq              i_push_to_ret_stack, call(f_malloc), val(100)
        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(42)
        dq                  i_read_mem_i64, i_add, val(1024 * 8), i_peek_ret_stack, val(1)
        dq              i_write_mem_i64
        dq                  i_add, val(1024 * 8), i_peek_ret_stack, val(1)
        dq                  val(42)
        dq              i_push_to_ret_stack, call(f_realloc)
        dq                  call(f_malloc), val(100)
        dq                  val(10000)

        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(0)
        dq                  call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq              i_push_to_ret_stack, call(f_byte_vector_make)
        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(1)
        dq                  call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(1)
        dq              i_push_to_ret_stack, call(f_byte_vector_make)
        dq          i_and
        dq              call(f_free), i_pop_from_ret_stack
        dq              i_and
        dq                  i_equal
        dq                      val(4)
        dq                      call(f_byte_vector_capacity), i_peek_ret_stack, val(1)
        dq                  i_equal
        dq                      val(4)
        dq                      call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(1)
        dq              i_push_to_ret_stack, call(f_byte_vector_make)
        dq          i_and
        dq              call(f_byte_vector_destroy), i_pop_from_ret_stack
        dq              i_equal
        dq                  val(1234567890)
        dq                  call(f_atoi)
        dq                  call(f_byte_vector_pointer), i_peek_ret_stack, val(1)
        dq                  call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(48)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(57)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(56)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(55)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(54)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(53)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(52)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(51)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(50)
        dq                  call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(49)
        dq              i_push_to_ret_stack, call(f_byte_vector_make)


        dq          val(1)



        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(1)
        dq          i_and
        dq              i_equal, val(0),
        dq                  call(f_memcmp), val(ss_hello_world), val(ss_hello_world), val(ss_hello_world_size)
        dq          i_and
        dq              i_not, i_equal, val(0),
        dq                  call(f_memcmp), val(ss_hello_world), val(ss_debug), val(ss_debug_size)
        dq          val(1)

        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(2)
        dq          i_and
        dq              i_equal, val(10), call(f_string_literal_escape_char_decode), val('n')
        dq          i_and
        dq              i_equal, val(9), call(f_string_literal_escape_char_decode), val('t')
        dq          i_and
        dq              i_equal, val(13), call(f_string_literal_escape_char_decode), val('r')
        dq          i_and
        dq              i_equal, val(-1), call(f_string_literal_escape_char_decode), val('e')
        dq          val(1)

        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(3)
        dq          call(f_byte_vector_destroy), i_pop_from_ret_stack
        dq          i_and
        dq              i_equal
        dq                  val(13)
        dq                  call(f_byte_vector_get)
        dq                      i_peek_ret_stack, val(1)
        dq                      val(3)
        dq          i_and
        dq              i_equal
        dq                  val(10)
        dq                  call(f_byte_vector_get)
        dq                      i_peek_ret_stack, val(1)
        dq                      val(0)
        dq          call(f_byte_vector_set), i_peek_ret_stack, val(1), val(3), val(13)
        dq          call(f_byte_vector_set), i_peek_ret_stack, val(1), val(2), val(12)
        dq          call(f_byte_vector_set), i_peek_ret_stack, val(1), val(1), val(11)
        dq          call(f_byte_vector_set), i_peek_ret_stack, val(1), val(0), val(10)

        dq          call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(0)
        dq          call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(0)
        dq          call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(0)
        dq          call(f_byte_vector_append_byte), i_peek_ret_stack, val(1), val(0)
        dq          i_push_to_ret_stack, call(f_byte_vector_make)
        dq          val(1)

        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(4)
        dq          call(f_byte_vector_destroy), i_pop_from_ret_stack
        dq          i_and
        dq              i_equal
        dq                  val(16)
        dq                  call(f_byte_vector_size)
        dq                      i_peek_ret_stack, val(1)
        dq          i_and
        dq              i_equal
        dq                  val(-2)
        dq                  i_read_mem_i64
        dq                  i_add, val(8), call(f_byte_vector_pointer)
        dq                      i_peek_ret_stack, val(1)
        dq          i_and
        dq              i_equal
        dq                  val(-1)
        dq                  i_read_mem_i64
        dq                  call(f_byte_vector_pointer)
        dq                      i_peek_ret_stack, val(1)
        dq          call(f_byte_vector_append_i64), i_peek_ret_stack, val(1), val(-2)
        dq          call(f_byte_vector_append_i64), i_peek_ret_stack, val(1), val(-1)
        dq          i_push_to_ret_stack, call(f_byte_vector_make)
        dq          val(1)

        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(5)
        dq          call(f_byte_vector_destroy), i_pop_from_ret_stack
        dq          i_and
        dq              i_equal
        dq                  val(4)
        dq                  call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_and
        dq              i_equal
        dq                  val(1234),
        dq                  call(f_atoi)
        dq                      call(f_byte_vector_pointer), i_peek_ret_stack, val(1)
        dq                      call(f_byte_vector_size), i_peek_ret_stack, val(1)
        dq          i_push_to_ret_stack, call(f_byte_vector_from_bytes), val(4), val('1'), val('2'), val('3'), val('4')
        dq          val(1)

        dq          call(f_print_bool)
        dq          call(f_print_newline)
        dq          call(f_print_number), val(6)
        dq          i_and, i_equal, i_pop_from_ret_stack, i_stack_depth
        dq          call(f_free), i_pop_from_ret_stack
        dq          i_and
        dq              call(f_byte_vector_equals)
        dq                  call(f_byte_vector_from_bytes), val(4), val('1'), val('2'), val('3'), val('4')
        dq                  call(f_dictionary_record_word_def)
        dq                  call(f_dictionary_find_record)
        dq                      i_read_mem_i64, i_peek_ret_stack, val(1)
        dq                      call(f_byte_vector_from_bytes), val(2), val('a'), val('a')
        dq          call(f_dictionary_add)
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val('a'), val('b')
        dq              call(f_byte_vector_from_bytes), val(4), val('4'), val('2'), val('3'), val('4')
        dq          call(f_dictionary_add)
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val('a'), val('a')
        dq              call(f_byte_vector_from_bytes), val(4), val('1'), val('2'), val('3'), val('4')
        dq          i_push_to_ret_stack, call(f_dictionary_make)
        dq          i_push_to_ret_stack, i_stack_depth
        dq          val(1)

f_tests: equ     $-8



;        dq          call(f_echo_tokens)

;        dq          i_indirect_jmp, val(f_tests)
;        dq          i_indirect_jmp, val(f_read_compile_run_loop)
        dq          call(f_read_compile_run_loop)
        dq          call(f_scanner_make)
        dq          i_pop_from_ret_stack
        dq          call(f_dictionary_add)
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(1), val('r')
        dq              call(f_word_def_make)
        dq                  call(f_func_concat), val(f_rename_word), call(f_capture), i_peek_ret_stack, val(1)
        dq                  val(1)
        dq          call(f_dictionary_add)
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(1), val('d')
        dq              call(f_word_def_make)
        dq                  call(f_func_concat), val(f_define_word), call(f_capture), i_peek_ret_stack, val(1)
        dq                  val(1)
        dq          call(f_dictionary_add)
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val('d'), val('i')
        dq              call(f_word_def_make)
        dq                  call(f_capture), i_peek_ret_stack, val(1)
        dq                  val(1)

%macro  def_instruction_word 2
        dq          call(f_dictionary_add),
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(1), val(%1)
        dq              call(f_word_def_make), val(%2), val(0)
%endmacro
%macro  def_instruction_word_2 3
        dq          call(f_dictionary_add),
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val(%1), val(%2)
        dq              call(f_word_def_make), val(%3), val(0)
%endmacro

%macro  def_function_word 2
        dq          call(f_dictionary_add),
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(1), val(%1)
        dq              call(f_word_def_make), val(%2), val(1)
%endmacro

%macro  def_function_word_2 3
        dq          call(f_dictionary_add),
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val(%1), val(%2)
        dq              call(f_word_def_make), val(%3), val(1)
%endmacro

%macro  def_constant_word_2 3
        dq          call(f_dictionary_add),
        dq              i_peek_ret_stack, val(1)
        dq              call(f_byte_vector_from_bytes), val(2), val(%1), val(%2)
        dq              call(f_word_def_make), call(f_capture), val(%3), val(1)
%endmacro


                    def_instruction_word '+', i_add
                    def_instruction_word_2 'c', '+', i_checked_add
                    def_instruction_word '*', i_mul
                    def_instruction_word_2 'c', '*', i_checked_mul
                    def_instruction_word '-', i_sub
                    def_instruction_word '/', i_div
                    def_instruction_word '%', i_mod

                    def_instruction_word_2 'b','&', i_and
                    def_instruction_word_2 'b','|', i_or
                    def_instruction_word_2 'b','!', i_not

                    def_instruction_word_2 '<', '<', i_bit_lshift_i64
                    def_instruction_word_2 '>', '>', i_bit_rshift_i64
                    def_instruction_word '^', i_bit_xor_64
                    def_instruction_word '|', i_bit_or_64
                    def_instruction_word '&', i_bit_and_64
                    
                    def_instruction_word '=', i_equal
                    def_instruction_word '>', i_greater
                    def_instruction_word_2 '>', '=', i_greater_or_equal
                    def_instruction_word '<', i_less
                    def_instruction_word_2 '<', '=', i_less_or_equal


                    def_instruction_word_2 'd', 'r', i_drop
                    def_instruction_word 'o', i_over
                    def_instruction_word_2 'd', 'u', i_dup
                    def_instruction_word_2 '2', 'd', i_2dup
                    def_instruction_word_2 'd', 'n', i_dup_n
                    def_instruction_word 's', i_swap
                    def_instruction_word_2 'r', 'o', i_rot
                    def_instruction_word_2 'r', 'r', i_rev_rot
                    
                    def_instruction_word_2 'r', '>', i_pop_from_ret_stack
                    def_instruction_word_2 '>', 'r', i_push_to_ret_stack
                    def_instruction_word_2 'r', 'p', i_peek_ret_stack
                    def_instruction_word_2 'r', '@', i_peek_ret_stack_first

                    def_instruction_word_2 'r', 'd', i_ret_stack_depth
                    def_instruction_word_2 'r', 't', i_ret_stack_top
                    def_constant_word_2    'r', 's', return_stack
                    
                    def_instruction_word_2 'w', 'b', i_write_mem_byte
                    def_instruction_word_2 'r', 'b', i_read_mem_byte
                    def_instruction_word_2 'w', 'i', i_write_mem_i32
                    def_instruction_word_2 'r', 'i', i_read_mem_i32
                    def_instruction_word_2 'w', 'l', i_write_mem_i64
                    def_instruction_word_2 'r', 'l', i_read_mem_i64
                    def_instruction_word_2 'm', 'c', i_memcopy
                    
                    def_instruction_word_2 's', 'c', i_syscall
                    def_instruction_word_2 'a', 'c', i_argc
                    def_instruction_word_2 'a', 'v', i_argv

                 
                    def_instruction_word 'i', i_indirect_call
                    def_instruction_word_2 'n','i', i_native_indirect_call
                    def_instruction_word_2 'n','t', native_indirect_call_test
                    def_instruction_word 'j', i_indirect_jmp
                    def_instruction_word 'l', i_late_bind_and_call_word

                    def_function_word_2 '.','s', f_print_data_stack
                    def_function_word_2 '.','b', f_print_bool
                    def_function_word '.', f_print_number
                    def_function_word_2 'n', 'l', f_print_newline
                    def_function_word_2 '.', 'c', f_write_byte_to_stdout
                    def_function_word_2 'r', '0', f_read_from_std_in


                    def_function_word 'p', f_print_byte_vector
                    
                    def_function_word_2 'i','f', f_if
                    def_function_word_2 'c','s', f_cond_start
                    def_function_word_2 'c','e', f_cond_end
                    def_function_word_2 'c','w', f_cond_when
                    def_function_word_2 'c','d', f_cond_default
                    def_function_word 'w', f_while
                    
                    def_function_word 't', f_true
                    def_function_word 'f', f_false

                    def_function_word_2 'f','c', f_func_concat
                    def_function_word_2 'c','a', f_capture

                    def_function_word_2 'c','p', f_byte_vector_copy_to_perm
                    def_function_word_2 'p','m', f_perm_malloc
                    
                    def_constant_word_2 'h','p', sigsegv_handler_pointer    
                    def_constant_word_2 'i','j', i_jmp    

                    def_function_word_2 'p','1', f_read_compile_run_loop
                    def_function_word_2 'p','2', f_read_and_compile_code
                    def_function_word_2 'p','3', f_read_and_compile_code__emit_code_for_next_token
                    def_function_word_2 'p','4', f_free
                    def_function_word_2 'p','5', f_realloc
                    def_function_word_2 'p','6', f_read_and_compile_code__emit_code_for_string_literal
                    def_function_word_2 'p','7', f_read_and_compile_code__emit_code_for_num
                    def_function_word_2 'p','8', f_read_and_compile_code__emit_code_for_word
                    def_function_word_2 'p','9', f_malloc
                    def_function_word_2 'p','a', f_dictionary_find_record
                    def_function_word_2 'p','b', f_is_bad_string_literal_token
                    def_function_word_2 'p','c', f_read_next_token
                    def_function_word_2 'p','d', f_string_literal_token_to_string
        dq          i_push_to_ret_stack, val(the_dictionary)
        dq          i_write_mem_i64, val(perm_mem_ptr), val(perm_mem)
f_start: equ     $-8

sigaction:
        dq          sigsegv_handler,
        dq          67108868, ; flags SA_SIGINFO | SA_RESTORER
        dq          sigsegv_restorer,
        dq          0, ; sig mask

        section   .bss
data_stack:         resb    data_stack_size
return_stack:       resb    return_stack_size
sigsegv_handler_pointer:
                    resq    1
the_dictionary:
                    resq    1
perm_mem_ptr:
                    resq    1
perm_mem:
                    resb    perm_mem_size
