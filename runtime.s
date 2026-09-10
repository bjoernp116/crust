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


.global crust_mmap
.type crust_mmap, @function

# crust_mmap(length: usize) -> void *
#
# System V function arguments:
#   RDI = length
#
# Linux write syscall:
#   RAX = syscall number
#   RDI = exit code
crust_mmap:
    mov eax, 9
    mov rsi, rdi
    xor rdi, rdi
    mov rdx, 3
    mov r10, 0x22
    mov r8, -1
    xor r9, r9
    syscall
    ret


.size crust_mmap, .-crust_mmap

.global crust_munmap
.type crust_munmap, @function

# crust_munmap(addr: &u8, length: usize) -> u16
#
# System V function arguments:
#   RDI = address 
#   RSI = length
#
# Linux write syscall:
#   RAX = syscall number
#   RDI = exit code
crust_munmap:
    mov eax, 11
    syscall
    ret


.size crust_munmap, .-crust_munmap
