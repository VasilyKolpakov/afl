#!/bin/bash
(cat bootstrap.2s \
core_lib.2s \
syscalls.2s \
the_dictionary.2s \
stacktrace.2s \
lib.2s \
byte_vector.2s \
block_allocator.2s \
allocator.2s \
array_list.2s \
switch_stmt.2s \
stack_effect.2s \
code_buffer.2s \
i64_to_text.2s \
the_dictionary_utils.2s \
print_function.2s \
lisp_interpreter.2s ; echo "(import \"$1\") (exit 0)") | ./run_boot.sh
