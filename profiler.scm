(import "core.scm")
(import "linux-error-names.scm")

;(enable-REPL-print)

(define sleep
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (millis)
      (begin
        (write-mem-i64 buf (/ millis 1000)) ; seconds
        (write-mem-i64 (+ buf 8) (* (- millis (* (/ millis 1000) 1000)) 1000000)) ; nanoseconds
        (syscall 35 buf buf 1 2 3 4)))))

(define (fork) (syscall 57 1 2 3 4 5 6))
(define (getpid) (syscall 39 1 2 3 4 5 6))
(define SIGINT 2)
(define (kill pid sig) (syscall 62 pid sig 1 2 3 4))
(define (wait4 pid stat-addr options) (checked-syscall 61 pid stat-addr options 0 1 2))

(define USER-REGS-STRUCT-FIELDS
  '(r15 r14 r13 r12 rbp rbx r11 r10 r9 r8 rax rcx rdx
    rsi rdi orig_rax rip cs eflags rsp ss fs_base gs_base ds
    es fs gs))

(define PTRACE_TRACEME 0)
(define PTRACE_PEEKDATA 2)
(define PTRACE_POKEDATA 5)
(define PTRACE_CONT 7)
(define PTRACE_GETREGS 12)

(define (ptrace req pid addr data)
  (checked-syscall 101 req pid addr data 1 2))

(define ptrace-peek
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (pid addr)
      (begin
        (if (equal? (ptrace PTRACE_PEEKDATA pid addr buf) 0)
          (read-mem-i64 buf)
          (begin (print-string "ptrace error\n") (exit 1)))))))

(define ptrace-get-reg
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (pid reg)
      (begin
        (if (equal? (ptrace PTRACE_GETREGS pid 0 buf) 0)
          (read-mem-i64 (+ buf (* (index-of USER-REGS-STRUCT-FIELDS reg) 8)))
          (begin (print-string "ptrace GETREGS error\n") (exit 1)))))))

(define (process-vm-readv pid lvec lcnt rvec rcnt)
  (syscall 310 pid lvec lcnt rvec rcnt 0))

(define read-process-mem
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (pid remote-addr local-addr len)
      (begin
        (write-mem-i64 (+  0 buf) local-addr)
        (write-mem-i64 (+  8 buf) len)
        (write-mem-i64 (+ 16 buf) remote-addr)
        (write-mem-i64 (+ 24 buf) len)
        (if (> (process-vm-readv pid buf 1 (+ buf 16) 1) -1)
          '()
          (begin (print-string "process-vm-readv error\n") (exit 1)))))))


(define (loop b) (begin (b) (loop b)))

(define buffer (syscall-mmap-anon 4096))

(define (child-code)
  (begin
    (println (ptrace PTRACE_TRACEME 0 0 0))
    (execve-second-cli-arg)
    (print-string "child exited\n")
    (exit 0)))

(define (read-process-dict pid)
  (read-process-dict-rec pid (ptrace-peek pid the-s2-dictionary-ptr)))

(define read-process-dict-rec
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (pid dict-record-ptr)
      (if (equal? dict-record-ptr 0)
        '()
        (let
          ((dict-record-name-2s-string (ptrace-peek pid dict-record-ptr))
           (dict-record-word-def (ptrace-peek pid (+ dict-record-ptr 8)))
           (dict-record-word-def-addr (ptrace-peek pid dict-record-word-def))
           (function-first-byte (ptrace-peek pid (+ dict-record-word-def-addr 8)))
           (dict-record-word-def-is-function (not (equal?
                                                    (ptrace-peek pid (+ dict-record-word-def 8))
                                                    0)))
           (dict-record-next (ptrace-peek pid (+ dict-record-ptr 16)))
           (dict-record-name-2s-string-buffer (ptrace-peek pid (+ dict-record-name-2s-string 16)))
           (dict-record-name-2s-string-size (ptrace-peek pid dict-record-name-2s-string)))
           (begin
             (read-process-mem pid dict-record-name-2s-string-buffer buf dict-record-name-2s-string-size)
             (let ((record-name (string-from-native-buffer buf dict-record-name-2s-string-size)))
               (if dict-record-word-def-is-function
                 (cons (list function-first-byte dict-record-word-def-addr record-name) (read-process-dict-rec pid dict-record-next))
                 (read-process-dict-rec pid dict-record-next)))))))))

(define (find-function-name-in-dict process-dict inst-ptr)
  (if (empty? process-dict)
    '()
    (let ((dict-item (car process-dict)))
      (let ((func-start-ptr (car dict-item))
            (func-last-ptr (car (cdr dict-item)))
            (func-name (car (cdr (cdr dict-item)))))
        (if (>= inst-ptr func-start-ptr)
          (if (<= inst-ptr func-last-ptr)
            func-name
            (find-function-name-in-dict (cdr process-dict) inst-ptr))
          (find-function-name-in-dict (cdr process-dict) inst-ptr))))))
          
(define read-process-call-stack
  (let ((buf (syscall-mmap-anon 4096)))
    (lambda (pid)
      (let ((return-stack-byte-size (- (ptrace-get-reg pid 'r13) s2-return-stack-ptr))
            (process-dict (read-process-dict pid)))
        (begin
          (read-process-mem pid s2-return-stack-ptr buf return-stack-byte-size)
          (write-mem-i64 (+ buf return-stack-byte-size) (ptrace-get-reg pid 'r14))
          ;(read-process-mem pid (- (ptrace-get-reg pid 'r13) 80) buf 80)
          (filter (lambda (item) (not (empty? item)))
                  (map (lambda (i)
                         (find-function-name-in-dict
                           process-dict
                           (read-mem-i64 (+ buf (* i 8)))))
                       (range (+ 1 (/ return-stack-byte-size 8))))))))))

(define (parent-code child-pid)
  (begin
    (sleep 1000)
    (print-string "parent pid:")
    (println (getpid))
    (print-string "parent's child pid:")
    (println child-pid)
    (print-string "parent waiting\n")
    (wait4 child-pid buffer 0)
    (ptrace PTRACE_CONT child-pid 0 0)
    (wait4 child-pid buffer 0)
    (println buffer)
    (print-string "parent waited\n")
    (ptrace PTRACE_CONT child-pid 0 0)
    (loop (lambda ()
            (begin
              (sleep 10)
              (kill child-pid SIGINT)
              (wait4 child-pid buffer 0)
              ;(read-process-mem child-pid buffer buffer 8)
              ;(println (read-mem-i64 buffer))
              ;(println (ptrace-get-reg child-pid 'r15))
              ;(println (read-process-dict child-pid))
              ;(println (length (read-process-dict child-pid)))
              (println (read-process-call-stack child-pid))
              ;(println (ptrace-peek child-pid buffer))
              ;(println (ptrace PTRACE_POKEDATA child-pid buffer 0))
              (ptrace PTRACE_CONT child-pid 0 0))))
    (exit 0)))

(define (string-to-c-string str buf)
  (string-to-native-buffer str buf)
  (write-mem-byte (+ buf (string-length str)) 0)
  (+ (string-length str) 1))

(define (execve exec-filename)
  (let ((filename-buf (syscall-mmap-anon (+ (string-length exec-filename) 1)))
        (argv-buf (syscall-mmap-anon 4096))
        (envp-buf (syscall-mmap-anon 4096)))
    (string-to-c-string exec-filename filename-buf)
    (write-mem-i64 argv-buf filename-buf)
    (write-mem-i64 (+ argv-buf 8) 0)
    (write-mem-i64 envp-buf 0)
    (checked-syscall 59 filename-buf argv-buf envp-buf 0 0 0)))

(let ((child-pid (fork)))
  (if (equal? 0 child-pid)
    (begin
      (println (ptrace PTRACE_TRACEME 0 0 0))
      (execve "./test.sh"))
    (parent-code child-pid)))

