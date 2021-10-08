nasm -felf64 -g afl.asm && ld -o afl afl.o && ./afl "$@"
