.intel_syntax noprefix
.section .text
.global main
.type main, @function
main:
.Lmain.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 16

	mov word ptr [rbp - 2], 4

	movzx eax, word ptr [rbp - 2]
	mov word ptr [rbp - 4], ax
	
	mov word ptr [rbp - 6], 3

	movzx eax, word ptr [rbp - 6]
	mov word ptr [rbp - 8], ax
	
.type add, @function
add:
.Ladd.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 16

	mov word ptr [rbp - 2], 0

	movzx eax, word ptr [rbp - 2]
	mov rsp, rbp
	pop rbp
	ret

