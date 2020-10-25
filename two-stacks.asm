        global    _start

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

%macro  drop_data_stack 1
        sub         r12, 8 * %1
        cmp         r12, data_stack
        jl          data_stack_underflow_handler
%endmacro

%macro  drop_return_stack 1
        sub         r13, 8 * %1
        cmp         r13, return_stack
        jl          return_stack_underflow_handler
%endmacro

%macro  debug 0
        mov         rdx, rax
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, debug_message            
        mov         rdx, 6               
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

data_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_data_stack_underflow_handler
        jmp iloop

return_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, f_return_stack_underflow_handler
        jmp iloop


i_call:
        mov         rax, [r11]              ; next word is the instruciton pointer
        sub         r11, 8                  ; move instruction pointer to the next instruction
        mov         [r13], r11              ; save return pointer
        add         r13, 8                  ; bump return stack, TODO: check for overflow
        mov         r11, rax                ; jump to callee
        jmp iloop

i_indirect_call:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the instruciton pointer
        mov         [r13], r11              ; save return pointer
        add         r13, 8                  ; bump return stack, TODO: check for overflow
        mov         r11, rax                ; jump to callee
        jmp iloop

i_push_to_ret_stack:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack
        mov         [r13], rax              ; save value
        add         r13, 8                  ; bump return stack, TODO: check for overflow
        jmp iloop
        
i_pop_from_ret_stack:
        drop_return_stack 1
        mov         rax, [r13]              ; restore value
        mov         [r12], rax              ; write to stack
        add         r12, 8                  ; move stack pointer, TODO: check for overflow
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
        jmp iloop
                
i_push_to_stack:
        mov         rax, [r11]              ; next word is value
        mov         [r12], rax              ; write to stack
        sub         r11, 8                  ; move instruction pointer to the next instruction
        add         r12, 8                  ; move stack pointer, TODO: check for overflow
        jmp iloop

_start: 
        mov         r11, f_start
        mov         r12, data_stack
        mov         r13, return_stack
        jmp         iloop        

        section   .data
message:
        db        "Hello, World", 10      ; note the newline at the end
data_stack_underflow_message:
        db        "Data stack underflow", 10
data_stack_underflow_message_size: equ $-data_stack_underflow_message

return_stack_underflow_message:
        db        "Return stack underflow", 10
return_stack_underflow_message_size: equ $-return_stack_underflow_message

debug_message:
        db        "debug", 10


%define val(value) value, i_push_to_stack

; data stack underflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(data_stack_underflow_message), val(data_stack_underflow_message_size)
f_data_stack_underflow_handler: equ     $-8
        
; return stack underflow handler
        dq          f_exit_0, i_call, f_print_buffer, i_call, val(return_stack_underflow_message), val(return_stack_underflow_message_size)
f_return_stack_underflow_handler: equ     $-8

; <buffer pointer> <length>        
; print buffer
        dq          i_return, i_syscall, 
        dq          val(1), val(1)              ; syscall num, fd
        dq          i_pop_from_ret_stack        ; buffer pointer
        dq          i_pop_from_ret_stack        ; buffer length
        dq          val(1), val(1), val(1)      ; filler
        dq          i_push_to_ret_stack         ; buffer length
        dq          i_push_to_ret_stack         ; buffer pointer
f_print_buffer: equ     $-8
        
; exit_0
        dq          i_return, i_syscall, 
        dq          val(60)                     ; syscall num
        dq          val(0)                      ; exit code
        dq          val(1), val(1), val(1), val(1), val(1) ; filler
f_exit_0: equ     $-8
        
; print "Hello world"
        dq          i_return, f_print_buffer, i_call, val(message), val(13)
f_print_hello_world: equ     $-8
; print "Data stack underflow"
        dq          i_return, f_print_buffer, i_call, val(data_stack_underflow_message), val(data_stack_underflow_message_size)
f_print_data_overflow: equ     $-8
; print "Return stack underflow"
        dq          i_return, f_print_buffer, i_call, val(return_stack_underflow_message), val(return_stack_underflow_message_size)
f_print_stack_overflow: equ     $-8
        
; interpreter's entry point
        dq          f_exit_0, i_call
        dq          f_print_hello_world, i_call
;        dq          i_indirect_call, i_indirect_call, i_indirect_call, 
;        dq          val(f_print_hello_world), val(f_print_data_overflow), val(f_print_stack_overflow)
f_start: equ     $-8

        section   .bss
data_stack:         resb    data_stack_size
return_stack:       resb    return_stack_size
