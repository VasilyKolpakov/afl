        global    _start

%macro  def_static_string 2
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
        cmp         r12, data_stack + data_stack_size
        jg          data_stack_overflow_handler
%endmacro

%macro  drop_data_stack 1
        sub         r12, 8 * %1
        cmp         r12, data_stack
        jl          data_stack_underflow_handler
%endmacro

%macro  bump_return_stack 1
        add         r13, 8 * %1
        cmp         r13, return_stack + return_stack_size
        jg          return_stack_overflow_handler
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


i_rjmp:
        add         r11, [r11]             ; next word is the offset
        jmp iloop
        


i_call:
        bump_return_stack 1
        mov         rax, [r11]              ; next word is the instruciton pointer
        sub         r11, 8                  ; move instruction pointer to the next instruction
        mov         [r13 - 8], r11           ; save return pointer
        mov         r11, rax                ; jump to callee
        jmp iloop

i_indirect_call:
        drop_data_stack 1
        mov         rax, [r12]              ; top of the data stack is the instruciton pointer
        bump_return_stack 1
        mov         [r13 - 8], r11          ; save return pointer
        mov         r11, rax                ; jump to callee
        jmp iloop

i_push_to_ret_stack:
        drop_data_stack 1
        bump_return_stack 1
        mov         rax, [r12]              ; top of the data stack
        mov         [r13], rax              ; save value
        jmp iloop
        
i_pop_from_ret_stack:
        drop_return_stack 1
        bump_data_stack 1
        mov         rax, [r13 + 8]              ; restore value
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


def_static_string ss_debug, "debug"
def_static_string ss_hello_world, "Hello, World"

def_static_string ss_data_stack_overflow, "Data stack overflow"
def_static_string ss_data_stack_underflow, "Data stack underflow"

def_static_string ss_return_stack_overflow, "Return stack overflow"
def_static_string ss_return_stack_underflow, "Return stack underflow"


%define val(value) value, i_push_to_stack

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
; allocate virtual memory
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
        
; exit_0
        dq          i_return, i_syscall, 
        dq          val(60)                     ; syscall num
        dq          val(0)                      ; exit code
        dq          val(1), val(1), val(1), val(1), val(1) ; filler
f_exit_0: equ     $-8

; data stack overflow
        dq          i_return, 40, i_rjmp, val(13), f_print_hello_world, i_call
f_dstack_overflow: equ     $-8

; return stack overflow
        dq          i_return, 48, i_rjmp, i_push_to_ret_stack, val(13), f_print_hello_world, i_call
f_rstack_overflow: equ     $-8

        
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
        
; interpreter's entry point
        dq          f_exit_0, i_call
        dq          f_echo, i_call
;        dq          f_dstack_overflow, i_call
;        dq          f_rstack_overflow, i_call
;        dq          i_indirect_call, i_indirect_call, i_indirect_call, 
;        dq          val(f_print_hello_world), val(f_print_data_overflow), val(f_echo)
f_start: equ     $-8

        section   .bss
data_stack:         resb    data_stack_size
return_stack:       resb    return_stack_size
read_buffer:        resb    1000
