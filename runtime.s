.intel_syntax noprefix
.section .text

.global crust_write
.type crust_write, @function

# crust_write(buffer: *u8, length: u64) -> i64
#
# System V function arguments:
#   RDI = buffer
#   RSI = length
#
# Linux write syscall:
#   RAX = syscall number
#   RDI = file descriptor
#   RSI = buffer
#   RDX = length
crust_write:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, 1          # stdout
    mov eax, 1          # SYS_write
    syscall
    ret

.size crust_write, .-crust_write

.global crust_exit
.type crust_exit, @function

# crust_exit(code: u8) -> !
#
# System V function arguments:
#   RDI = code
#
# Linux write syscall:
#   RAX = syscall number
#   RDI = exit code
crust_exit:
    mov eax, 60         # SYS_write
    syscall
    ret

.size crust_exit, .-crust_exit
