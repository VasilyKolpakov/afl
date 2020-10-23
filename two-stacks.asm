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



stdin_read_buffer_size: equ    1024
data_stack_size:        equ    10240
return_stack_size:      equ    10240
dictionary_size:        equ    10240

        section   .text

; r11 - instruction pointer
; r12 - data stack pointer
; r13 - return stack pointer

iloop:  
        sub         r11, 8
        jmp         [r11 + 8]

data_stack_underflow_handler:
        mov         r12, data_stack
        mov         r13, return_stack
        mov         r11, i_data_stack_underflow_handler
        jmp iloop



;<buffer pointer> <length>
print_buffer:
        drop_data_stack 2                   ; move stack pointer
        mov         rax, 1
        mov         rdi, 1
        mov         rsi, [r12 + 8]          ; buffer pointer
        mov         rdx, [r12]              ; length
        push_registers
        syscall
        pop_registers
        jmp iloop

push_to_stack:
        mov         rax, [r11]              ; next word is value
        mov         [r12], rax              ; write to stack
        sub         r11, 8                  ; move instruction pointer to the next instruction
        add         r12, 8                  ; move stack pointer
        jmp iloop

exit:
        mov         rax, 60                 ; system call for exit
        xor         rdi, rdi                ; exit code 0
        syscall
            
_start: 
        mov         r11, i_start
        mov         r12, data_stack
        mov         r13, return_stack
        jmp         iloop
        
;        mov       rax, 0                 ; read
;        mov       rdi, 0                  
;        mov       rsi, buffer            
;        mov       rdx, buffer_len                 
;        syscall



        section   .data
message:
        db        "Hello, World", 10      ; note the newline at the end
stack_underflow_message:
        db        "Stack underflow", 10
stack_underflow_message_size: equ $-stack_underflow_message

debug_message:
        db        "debug", 10
        
; data stack underflow handler
        dq          exit, print_buffer, stack_underflow_message, push_to_stack, stack_underflow_message_size, push_to_stack
i_data_stack_underflow_handler: equ     $-8
        
; interpreter's entry point
        dq          exit, 
        dq          print_buffer, message, push_to_stack, 13, push_to_stack
        dq          print_buffer, message, push_to_stack, 13, push_to_stack
i_start: equ     $-8

        section   .bss
stdin_read_buffer:  resb    stdin_read_buffer_size
data_stack:         resb    data_stack_size
return_stack:       resb    return_stack_size
