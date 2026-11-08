.intel_syntax noprefix
.section .text
.global main
.type main, @function
main:
.Lmain.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x7c0

	# %1 = 4096
	mov rdi, 4096

	# %0 = call f2(%1)
	push rdi
	push rsi
	call crust_mmap
	mov r9, rax
	pop rsi
	pop rdi
	
	# %2 = 4096
	mov rsi, 4096

	# call f3(%0, %2)
	push rdi
	push rsi
	mov rdi, r9
	call crust_munmap
	pop rsi
	pop rdi
	
	# %3 = 10
	mov qword ptr[rbp - 0x2a8], 10

	# %5 = &[s0 + 0]
	lea r8, [rbp - 0x18 + 0x0]
	# call f16(%5)
	push rdi
	push rsi
	mov rdi, r8
	call allocator_new
	pop rsi
	pop rdi
	
	# [s1 + 0] = copy [s0 + 0]
	mov r10, qword ptr[rbp - 0x18]
	mov qword ptr[rbp - 0x40], r10
	mov r10, qword ptr[rbp - 0x10]
	mov qword ptr[rbp - 0x38], r10
	mov r10, qword ptr[rbp - 0x8]
	mov qword ptr[rbp - 0x30], r10
	
	# %6 = 1
	mov r8, 1

	# [s2 + 0] = store %6
	mov qword ptr[rbp - 0x60], r8
	# %7 = 2
	mov r8, 2

	# [s2 + 8] = store %7
	mov qword ptr[rbp - 0x58], r8
	# %8 = 3
	mov r8, 3

	# [s2 + 16] = store %8
	mov qword ptr[rbp - 0x50], r8
	# %9 = 4
	mov r8, 4

	# [s2 + 24] = store %9
	mov qword ptr[rbp - 0x48], r8
	# [s3 + 0] = copy [s2 + 0]
	mov r10, qword ptr[rbp - 0x60]
	mov qword ptr[rbp - 0x80], r10
	mov r10, qword ptr[rbp - 0x58]
	mov qword ptr[rbp - 0x78], r10
	mov r10, qword ptr[rbp - 0x50]
	mov qword ptr[rbp - 0x70], r10
	mov r10, qword ptr[rbp - 0x48]
	mov qword ptr[rbp - 0x68], r10
	# %11 = &[s3 + 0]
	lea r8, [rbp - 0x80 + 0x0]
	# %12 = 2
	mov rdx, 2

	# %13 = [%11 + %12]
	mov rcx, [r8 + rdx * 0x8]
	# %10 = move %13
	mov r8, rcx
	# %15 = cast %10
	mov edx, r8d
	# %16 = %15 + %3
	mov r11, qword ptr[rbp - 0x2a8]
	add rdx, r11
	mov rcx, rdx
	
	# %4 = 20
	mov rdx, 20

	# %14 = call f5(%16, %4)
	push rdi
	push rsi
	mov rdi, rcx
	mov rsi, rdx
	call add
	mov qword ptr[rbp - 0x2b0], rax
	pop rsi
	pop rdi
	
	# %17 = 1
	mov cl, 1

	# %18 = 33
	mov rax, 33

	# call f14(%17, %14, %18)
	push rdi
	push rsi
	mov dil, cl
	mov rsi, qword ptr[rbp - 0x2b0]
	mov rdx, rax
	call assert_eq
	pop rsi
	pop rdi
	
	# %20 = 5
	mov rcx, 5

	# %21 = 7
	mov rax, 7

	# %22 = %20 + %21
	add rcx, rax
	mov qword ptr[rbp - 0x2b8], rcx
	
	# %19 = move %22
	mov r10, qword ptr[rbp - 0x2b8]
	mov qword ptr[rbp - 0x2c0], r10
	# %23 = 2
	mov byte ptr[rbp - 0x2c1], 2

	# %24 = 12
	mov qword ptr[rbp - 0x2d0], 12

	# call f14(%23, %19, %24)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x2c1]
	mov rsi, qword ptr[rbp - 0x2c0]
	mov rdx, qword ptr[rbp - 0x2d0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %25 = 2
	mov qword ptr[rbp - 0x2d8], 2

	# %28 = 3
	mov qword ptr[rbp - 0x2e0], 3

	# %29 = %25 + %28
	mov r10, qword ptr[rbp - 0x2d8]
	mov r11, qword ptr[rbp - 0x2e0]
	add r10, r11
	mov qword ptr[rbp - 0x2e8], r10
	
	# %27 = move %29
	mov r10, qword ptr[rbp - 0x2e8]
	mov qword ptr[rbp - 0x2f0], r10
	# %30 = 2
	mov qword ptr[rbp - 0x2f8], 2

	# %31 = %27 * %30
	mov r10, qword ptr[rbp - 0x2f0]
	mov r11, qword ptr[rbp - 0x2f8]
	imul r10, r11
	mov qword ptr[rbp - 0x300], r10
	
	# %26 = move %31
	mov r10, qword ptr[rbp - 0x300]
	mov qword ptr[rbp - 0x308], r10
	# %32 = 3
	mov byte ptr[rbp - 0x309], 3

	# %33 = 10
	mov qword ptr[rbp - 0x318], 10

	# call f14(%32, %26, %33)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x309]
	mov rsi, qword ptr[rbp - 0x308]
	mov rdx, qword ptr[rbp - 0x318]
	call assert_eq
	pop rsi
	pop rdi
	
	# %35 = 20
	mov qword ptr[rbp - 0x320], 20

	# %36 = %14 > %35
	mov r10, qword ptr[rbp - 0x2b0]
	mov r11, qword ptr[rbp - 0x320]
	cmp r10, r11
	seta byte ptr[rbp - 0x321]

	# Branch
	cmp byte ptr[rbp - 0x321], 0
	je .Lmain.if_else.2
	jmp .Lmain.if_then.1

.Lmain.if_then.1:
	# %34 = 8
	mov qword ptr[rbp - 0x330], 8

	# Jump
	jmp .Lmain.if_merge.3

.Lmain.if_else.2:
	# %34 = 4
	mov qword ptr[rbp - 0x330], 4

	# Jump
	jmp .Lmain.if_merge.3

.Lmain.if_merge.3:
	# %37 = 4
	mov byte ptr[rbp - 0x331], 4

	# %38 = 8
	mov qword ptr[rbp - 0x340], 8

	# call f14(%37, %34, %38)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x331]
	mov rsi, qword ptr[rbp - 0x330]
	mov rdx, qword ptr[rbp - 0x340]
	call assert_eq
	pop rsi
	pop rdi
	
	# %40 = 8
	mov qword ptr[rbp - 0x348], 8

	# %41 = %34 == %40
	mov r10, qword ptr[rbp - 0x330]
	mov r11, qword ptr[rbp - 0x348]
	cmp r10, r11
	sete byte ptr[rbp - 0x349]

	# Branch
	cmp byte ptr[rbp - 0x349], 0
	je .Lmain.if_else.5
	jmp .Lmain.if_then.4

.Lmain.if_then.4:
	# %42 = 12
	mov qword ptr[rbp - 0x358], 12

	# %43 = %19 == %42
	mov r10, qword ptr[rbp - 0x2c0]
	mov r11, qword ptr[rbp - 0x358]
	cmp r10, r11
	sete byte ptr[rbp - 0x359]

	# Branch
	cmp byte ptr[rbp - 0x359], 0
	je .Lmain.if_else.6
	jmp .Lmain.if_then.5

.Lmain.if_then.5:
	# %39 = 11
	mov qword ptr[rbp - 0x368], 11

	# Jump
	jmp .Lmain.if_merge.7

.Lmain.if_else.6:
	# %39 = 100
	mov qword ptr[rbp - 0x368], 100

	# Jump
	jmp .Lmain.if_merge.7

.Lmain.if_merge.7:
	# Jump
	jmp .Lmain.if_merge.6

.Lmain.if_else.5:
	# %39 = 200
	mov qword ptr[rbp - 0x368], 200

	# Jump
	jmp .Lmain.if_merge.6

.Lmain.if_merge.6:
	# %44 = 5
	mov byte ptr[rbp - 0x369], 5

	# %45 = 11
	mov qword ptr[rbp - 0x378], 11

	# call f14(%44, %39, %45)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x369]
	mov rsi, qword ptr[rbp - 0x368]
	mov rdx, qword ptr[rbp - 0x378]
	call assert_eq
	pop rsi
	pop rdi
	
	# %46 = 50
	mov qword ptr[rbp - 0x380], 50

	# [s4 + 0] = store %46
	mov r10, qword ptr[rbp - 0x380]
	mov qword ptr[rbp - 0x88], r10
	# [s5 + 0] = copy [s4 + 0]
	mov r10, qword ptr[rbp - 0x88]
	mov qword ptr[rbp - 0x90], r10
	
	# %48 = &[s5 + 0]
	lea r10, [rbp - 0x90 + 0x0]
	mov qword ptr[rbp - 0x388], r10
	# %47 = move %48
	mov r10, qword ptr[rbp - 0x388]
	mov qword ptr[rbp - 0x390], r10
	# %50 = load [*%47 + 0]
	mov r10, qword ptr[rbp - 0x390]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x398], r10
	# %49 = move %50
	mov r10, qword ptr[rbp - 0x398]
	mov qword ptr[rbp - 0x3a0], r10
	# %51 = 6
	mov byte ptr[rbp - 0x3a1], 6

	# %52 = 50
	mov qword ptr[rbp - 0x3b0], 50

	# call f14(%51, %49, %52)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x3a1]
	mov rsi, qword ptr[rbp - 0x3a0]
	mov rdx, qword ptr[rbp - 0x3b0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %54 = 5
	mov qword ptr[rbp - 0x3b8], 5

	# %53 = call f7(%54, %47)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x3b8]
	mov rsi, qword ptr[rbp - 0x390]
	call add_ref
	mov qword ptr[rbp - 0x3c0], rax
	pop rsi
	pop rdi
	
	# %55 = 7
	mov byte ptr[rbp - 0x3c1], 7

	# %56 = 55
	mov qword ptr[rbp - 0x3d0], 55

	# call f14(%55, %53, %56)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x3c1]
	mov rsi, qword ptr[rbp - 0x3c0]
	mov rdx, qword ptr[rbp - 0x3d0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %57 = 6
	mov word ptr[rbp - 0x3d2], 6

	# [s6 + 0] = store %57
	mov r10w, word ptr[rbp - 0x3d2]
	mov word ptr[rbp - 0x92], r10w
	# [s7 + 0] = copy [s6 + 0]
	movzx r10, word ptr[rbp - 0x92]
	mov word ptr[rbp - 0x94], r10w
	
	# %58 = &[s7 + 0]
	lea r10, [rbp - 0x94 + 0x0]
	mov qword ptr[rbp - 0x3e0], r10
	# %60 = call f8(%58)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x3e0]
	call read_u16
	mov word ptr[rbp - 0x3e2], ax
	pop rsi
	pop rdi
	
	# %62 = cast %60
	mov r10w, word ptr[rbp - 0x3e2]
	mov r11, qword ptr[rbp - 0x3f0]
	movzx r11d, r10w
	mov qword ptr[rbp - 0x3f0], r11
	# %61 = 8
	mov byte ptr[rbp - 0x3f1], 8

	# %63 = 6
	mov qword ptr[rbp - 0x400], 6

	# call f14(%61, %62, %63)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x3f1]
	mov rsi, qword ptr[rbp - 0x3f0]
	mov rdx, qword ptr[rbp - 0x400]
	call assert_eq
	pop rsi
	pop rdi
	
	# %64 = 13
	mov qword ptr[rbp - 0x408], 13

	# %65 = move %64
	mov r10, qword ptr[rbp - 0x408]
	mov qword ptr[rbp - 0x410], r10
	# [s8 + 0] = store %65
	mov r10, qword ptr[rbp - 0x410]
	mov qword ptr[rbp - 0xb0], r10
	# %66 = 45
	mov qword ptr[rbp - 0x418], 45

	# %67 = move %66
	mov r10, qword ptr[rbp - 0x418]
	mov qword ptr[rbp - 0x420], r10
	# [s8 + 8] = store %67
	mov r10, qword ptr[rbp - 0x420]
	mov qword ptr[rbp - 0xa8], r10
	# [s11 + 0] = copy [s8 + 0]
	mov r10, qword ptr[rbp - 0xb0]
	mov qword ptr[rbp - 0xc0], r10
	mov r10, qword ptr[rbp - 0xa8]
	mov qword ptr[rbp - 0xb8], r10
	
	# [s12 + 0] = copy [s11 + 0]
	mov r10, qword ptr[rbp - 0xc0]
	mov qword ptr[rbp - 0xd0], r10
	mov r10, qword ptr[rbp - 0xb8]
	mov qword ptr[rbp - 0xc8], r10
	
	# %69 = load [s12 + 0]
	mov r10, qword ptr[rbp - 0xd0]
	mov qword ptr[rbp - 0x428], r10
	# %70 = load [s12 + 8]
	mov r10, qword ptr[rbp - 0xc8]
	mov qword ptr[rbp - 0x430], r10
	# %71 = %69 + %70
	mov r10, qword ptr[rbp - 0x428]
	mov r11, qword ptr[rbp - 0x430]
	add r10, r11
	mov qword ptr[rbp - 0x438], r10
	
	# %68 = move %71
	mov r10, qword ptr[rbp - 0x438]
	mov qword ptr[rbp - 0x440], r10
	# %72 = 9
	mov byte ptr[rbp - 0x441], 9

	# %73 = 58
	mov qword ptr[rbp - 0x450], 58

	# call f14(%72, %68, %73)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x441]
	mov rsi, qword ptr[rbp - 0x440]
	mov rdx, qword ptr[rbp - 0x450]
	call assert_eq
	pop rsi
	pop rdi
	
	# %74 = &[s11 + 0]
	lea r10, [rbp - 0xc0 + 0x0]
	mov qword ptr[rbp - 0x458], r10
	# [s13 + 0] = copy [*%74 + 0]
	mov r10, qword ptr[rbp - 0x458]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0xe0], r10
	mov r10, qword ptr[rbp - 0x458]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0xd8], r10
	
	# %77 = load [s13 + 0]
	mov r10, qword ptr[rbp - 0xe0]
	mov qword ptr[rbp - 0x460], r10
	# %78 = load [s13 + 8]
	mov r10, qword ptr[rbp - 0xd8]
	mov qword ptr[rbp - 0x468], r10
	# %79 = %77 + %78
	mov r10, qword ptr[rbp - 0x460]
	mov r11, qword ptr[rbp - 0x468]
	add r10, r11
	mov qword ptr[rbp - 0x470], r10
	
	# %76 = move %79
	mov r10, qword ptr[rbp - 0x470]
	mov qword ptr[rbp - 0x478], r10
	# %80 = 10
	mov byte ptr[rbp - 0x479], 10

	# %81 = 58
	mov qword ptr[rbp - 0x488], 58

	# call f14(%80, %76, %81)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x479]
	mov rsi, qword ptr[rbp - 0x478]
	mov rdx, qword ptr[rbp - 0x488]
	call assert_eq
	pop rsi
	pop rdi
	
	# %82 = &[s11 + 8]
	lea r10, [rbp - 0xc0 + 0x8]
	mov qword ptr[rbp - 0x490], r10
	# %85 = load [*%82 + 0]
	mov r10, qword ptr[rbp - 0x490]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x498], r10
	# %84 = move %85
	mov r10, qword ptr[rbp - 0x498]
	mov qword ptr[rbp - 0x4a0], r10
	# %86 = 11
	mov byte ptr[rbp - 0x4a1], 11

	# %87 = 45
	mov qword ptr[rbp - 0x4b0], 45

	# call f14(%86, %84, %87)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x4a1]
	mov rsi, qword ptr[rbp - 0x4a0]
	mov rdx, qword ptr[rbp - 0x4b0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %89 = &[s11 + 0]
	lea r10, [rbp - 0xc0 + 0x0]
	mov qword ptr[rbp - 0x4b8], r10
	# %88 = call f9(%89)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x4b8]
	call sum_position
	mov qword ptr[rbp - 0x4c0], rax
	pop rsi
	pop rdi
	
	# %90 = 12
	mov byte ptr[rbp - 0x4c1], 12

	# %91 = 58
	mov qword ptr[rbp - 0x4d0], 58

	# call f14(%90, %88, %91)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x4c1]
	mov rsi, qword ptr[rbp - 0x4c0]
	mov rdx, qword ptr[rbp - 0x4d0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %92 = "hello"
	lea r10, [rip + .Lstr_0]
	mov qword ptr[rbp - 0x4d8], r10
	# [s14 + 0] = store %92
	mov r10, qword ptr[rbp - 0x4d8]
	mov qword ptr[rbp - 0xf0], r10
	# %93 = 5
	mov qword ptr[rbp - 0x4e0], 5

	# [s14 + 8] = store %93
	mov r10, qword ptr[rbp - 0x4e0]
	mov qword ptr[rbp - 0xe8], r10
	# [s15 + 0] = copy [s14 + 0]
	mov r10, qword ptr[rbp - 0xf0]
	mov qword ptr[rbp - 0x100], r10
	mov r10, qword ptr[rbp - 0xe8]
	mov qword ptr[rbp - 0xf8], r10
	
	# %94 = &[s15 + 0]
	lea r10, [rbp - 0x100 + 0x0]
	mov qword ptr[rbp - 0x4e8], r10
	# call f15(%94)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x4e8]
	call print
	pop rsi
	pop rdi
	
	# %95 = " "
	lea r10, [rip + .Lstr_1]
	mov qword ptr[rbp - 0x4f0], r10
	# [s16 + 0] = store %95
	mov r10, qword ptr[rbp - 0x4f0]
	mov qword ptr[rbp - 0x110], r10
	# %96 = 1
	mov qword ptr[rbp - 0x4f8], 1

	# [s16 + 8] = store %96
	mov r10, qword ptr[rbp - 0x4f8]
	mov qword ptr[rbp - 0x108], r10
	# [s17 + 0] = copy [s16 + 0]
	mov r10, qword ptr[rbp - 0x110]
	mov qword ptr[rbp - 0x120], r10
	mov r10, qword ptr[rbp - 0x108]
	mov qword ptr[rbp - 0x118], r10
	
	# %97 = &[s17 + 0]
	lea r10, [rbp - 0x120 + 0x0]
	mov qword ptr[rbp - 0x500], r10
	# call f15(%97)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x500]
	call print
	pop rsi
	pop rdi
	
	# %98 = "world"
	lea r10, [rip + .Lstr_2]
	mov qword ptr[rbp - 0x508], r10
	# [s18 + 0] = store %98
	mov r10, qword ptr[rbp - 0x508]
	mov qword ptr[rbp - 0x130], r10
	# %99 = 5
	mov qword ptr[rbp - 0x510], 5

	# [s18 + 8] = store %99
	mov r10, qword ptr[rbp - 0x510]
	mov qword ptr[rbp - 0x128], r10
	# [s19 + 0] = copy [s18 + 0]
	mov r10, qword ptr[rbp - 0x130]
	mov qword ptr[rbp - 0x140], r10
	mov r10, qword ptr[rbp - 0x128]
	mov qword ptr[rbp - 0x138], r10
	
	# %100 = &[s19 + 0]
	lea r10, [rbp - 0x140 + 0x0]
	mov qword ptr[rbp - 0x518], r10
	# call f15(%100)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x518]
	call print
	pop rsi
	pop rdi
	
	# %101 = "\\n"
	lea r10, [rip + .Lstr_3]
	mov qword ptr[rbp - 0x520], r10
	# [s20 + 0] = store %101
	mov r10, qword ptr[rbp - 0x520]
	mov qword ptr[rbp - 0x150], r10
	# %102 = 1
	mov qword ptr[rbp - 0x528], 1

	# [s20 + 8] = store %102
	mov r10, qword ptr[rbp - 0x528]
	mov qword ptr[rbp - 0x148], r10
	# [s21 + 0] = copy [s20 + 0]
	mov r10, qword ptr[rbp - 0x150]
	mov qword ptr[rbp - 0x160], r10
	mov r10, qword ptr[rbp - 0x148]
	mov qword ptr[rbp - 0x158], r10
	
	# %103 = &[s21 + 0]
	lea r10, [rbp - 0x160 + 0x0]
	mov qword ptr[rbp - 0x530], r10
	# call f15(%103)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x530]
	call print
	pop rsi
	pop rdi
	
	# %104 = "first string\\n"
	lea r10, [rip + .Lstr_4]
	mov qword ptr[rbp - 0x538], r10
	# [s22 + 0] = store %104
	mov r10, qword ptr[rbp - 0x538]
	mov qword ptr[rbp - 0x170], r10
	# %105 = 13
	mov qword ptr[rbp - 0x540], 13

	# [s22 + 8] = store %105
	mov r10, qword ptr[rbp - 0x540]
	mov qword ptr[rbp - 0x168], r10
	# [s23 + 0] = copy [s22 + 0]
	mov r10, qword ptr[rbp - 0x170]
	mov qword ptr[rbp - 0x180], r10
	mov r10, qword ptr[rbp - 0x168]
	mov qword ptr[rbp - 0x178], r10
	
	# [s24 + 0] = copy [s23 + 0]
	mov r10, qword ptr[rbp - 0x180]
	mov qword ptr[rbp - 0x190], r10
	mov r10, qword ptr[rbp - 0x178]
	mov qword ptr[rbp - 0x188], r10
	
	# %106 = &[s24 + 0]
	lea r10, [rbp - 0x190 + 0x0]
	mov qword ptr[rbp - 0x548], r10
	# call f15(%106)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x548]
	call print
	pop rsi
	pop rdi
	
	# %107 = "through &str\\n"
	lea r10, [rip + .Lstr_5]
	mov qword ptr[rbp - 0x550], r10
	# [s25 + 0] = store %107
	mov r10, qword ptr[rbp - 0x550]
	mov qword ptr[rbp - 0x1a0], r10
	# %108 = 13
	mov qword ptr[rbp - 0x558], 13

	# [s25 + 8] = store %108
	mov r10, qword ptr[rbp - 0x558]
	mov qword ptr[rbp - 0x198], r10
	# [s26 + 0] = copy [s25 + 0]
	mov r10, qword ptr[rbp - 0x1a0]
	mov qword ptr[rbp - 0x1b0], r10
	mov r10, qword ptr[rbp - 0x198]
	mov qword ptr[rbp - 0x1a8], r10
	
	# %110 = &[s26 + 0]
	lea r10, [rbp - 0x1b0 + 0x0]
	mov qword ptr[rbp - 0x560], r10
	# %109 = move %110
	mov r10, qword ptr[rbp - 0x560]
	mov qword ptr[rbp - 0x568], r10
	# call f15(%109)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x568]
	call print
	pop rsi
	pop rdi
	
	# [s27 + 0] = copy [*%109 + 0]
	mov r10, qword ptr[rbp - 0x568]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x1c0], r10
	mov r10, qword ptr[rbp - 0x568]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0x1b8], r10
	
	# %111 = &[s27 + 0]
	lea r10, [rbp - 0x1c0 + 0x0]
	mov qword ptr[rbp - 0x570], r10
	# call f15(%111)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x570]
	call print
	pop rsi
	pop rdi
	
	# %112 = "sparrow"
	lea r10, [rip + .Lstr_6]
	mov qword ptr[rbp - 0x578], r10
	# [s28 + 0] = store %112
	mov r10, qword ptr[rbp - 0x578]
	mov qword ptr[rbp - 0x1d0], r10
	# %113 = 7
	mov qword ptr[rbp - 0x580], 7

	# [s28 + 8] = store %113
	mov r10, qword ptr[rbp - 0x580]
	mov qword ptr[rbp - 0x1c8], r10
	# [s29 + 0] = copy [s28 + 0]
	mov r10, qword ptr[rbp - 0x1d0]
	mov qword ptr[rbp - 0x1e0], r10
	mov r10, qword ptr[rbp - 0x1c8]
	mov qword ptr[rbp - 0x1d8], r10
	
	# %114 = 3
	mov word ptr[rbp - 0x582], 3

	# %115 = move %114
	mov r10w, word ptr[rbp - 0x582]
	mov word ptr[rbp - 0x584], r10w
	# [s30 + 0] = store %115
	mov r10w, word ptr[rbp - 0x584]
	mov word ptr[rbp - 0x230], r10w
	# %116 = 40
	mov qword ptr[rbp - 0x590], 40

	# %117 = move %116
	mov r10, qword ptr[rbp - 0x590]
	mov qword ptr[rbp - 0x598], r10
	# [s32 + 0] = store %117
	mov r10, qword ptr[rbp - 0x598]
	mov qword ptr[rbp - 0x240], r10
	# %118 = 2
	mov qword ptr[rbp - 0x5a0], 2

	# %119 = move %118
	mov r10, qword ptr[rbp - 0x5a0]
	mov qword ptr[rbp - 0x5a8], r10
	# [s32 + 8] = store %119
	mov r10, qword ptr[rbp - 0x5a8]
	mov qword ptr[rbp - 0x238], r10
	# [s30 + 16] = copy [s32 + 0]
	mov r10, qword ptr[rbp - 0x240]
	mov qword ptr[rbp - 0x220], r10
	mov r10, qword ptr[rbp - 0x238]
	mov qword ptr[rbp - 0x218], r10
	
	# [s30 + 32] = copy [s29 + 0]
	mov r10, qword ptr[rbp - 0x1e0]
	mov qword ptr[rbp - 0x210], r10
	mov r10, qword ptr[rbp - 0x1d8]
	mov qword ptr[rbp - 0x208], r10
	
	# [s35 + 0] = copy [s30 + 0]
	mov r10, qword ptr[rbp - 0x230]
	mov qword ptr[rbp - 0x270], r10
	mov r10, qword ptr[rbp - 0x228]
	mov qword ptr[rbp - 0x268], r10
	mov r10, qword ptr[rbp - 0x220]
	mov qword ptr[rbp - 0x260], r10
	mov r10, qword ptr[rbp - 0x218]
	mov qword ptr[rbp - 0x258], r10
	mov r10, qword ptr[rbp - 0x210]
	mov qword ptr[rbp - 0x250], r10
	mov r10, qword ptr[rbp - 0x208]
	mov qword ptr[rbp - 0x248], r10
	
	# %121 = load [s35 + 16]
	mov r10, qword ptr[rbp - 0x260]
	mov qword ptr[rbp - 0x5b0], r10
	# %122 = load [s35 + 24]
	mov r10, qword ptr[rbp - 0x258]
	mov qword ptr[rbp - 0x5b8], r10
	# %123 = %121 + %122
	mov r10, qword ptr[rbp - 0x5b0]
	mov r11, qword ptr[rbp - 0x5b8]
	add r10, r11
	mov qword ptr[rbp - 0x5c0], r10
	
	# %120 = move %123
	mov r10, qword ptr[rbp - 0x5c0]
	mov qword ptr[rbp - 0x5c8], r10
	# %124 = 13
	mov byte ptr[rbp - 0x5c9], 13

	# %125 = 42
	mov qword ptr[rbp - 0x5d8], 42

	# call f14(%124, %120, %125)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x5c9]
	mov rsi, qword ptr[rbp - 0x5c8]
	mov rdx, qword ptr[rbp - 0x5d8]
	call assert_eq
	pop rsi
	pop rdi
	
	# %126 = &[s35 + 16]
	lea r10, [rbp - 0x270 + 0x10]
	mov qword ptr[rbp - 0x5e0], r10
	# %129 = load [*%126 + 0]
	mov r10, qword ptr[rbp - 0x5e0]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x5e8], r10
	# %128 = move %129
	mov r10, qword ptr[rbp - 0x5e8]
	mov qword ptr[rbp - 0x5f0], r10
	# %130 = 14
	mov byte ptr[rbp - 0x5f1], 14

	# %131 = 40
	mov qword ptr[rbp - 0x600], 40

	# call f14(%130, %128, %131)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x5f1]
	mov rsi, qword ptr[rbp - 0x5f0]
	mov rdx, qword ptr[rbp - 0x600]
	call assert_eq
	pop rsi
	pop rdi
	
	# %132 = &[s35 + 16]
	lea r10, [rbp - 0x270 + 0x10]
	mov qword ptr[rbp - 0x608], r10
	# [s36 + 0] = copy [*%132 + 0]
	mov r10, qword ptr[rbp - 0x608]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x280], r10
	mov r10, qword ptr[rbp - 0x608]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0x278], r10
	
	# %135 = load [s36 + 0]
	mov r10, qword ptr[rbp - 0x280]
	mov qword ptr[rbp - 0x610], r10
	# %136 = load [s36 + 8]
	mov r10, qword ptr[rbp - 0x278]
	mov qword ptr[rbp - 0x618], r10
	# %137 = %135 + %136
	mov r10, qword ptr[rbp - 0x610]
	mov r11, qword ptr[rbp - 0x618]
	add r10, r11
	mov qword ptr[rbp - 0x620], r10
	
	# %134 = move %137
	mov r10, qword ptr[rbp - 0x620]
	mov qword ptr[rbp - 0x628], r10
	# %138 = 15
	mov byte ptr[rbp - 0x629], 15

	# %139 = 42
	mov qword ptr[rbp - 0x638], 42

	# call f14(%138, %134, %139)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x629]
	mov rsi, qword ptr[rbp - 0x628]
	mov rdx, qword ptr[rbp - 0x638]
	call assert_eq
	pop rsi
	pop rdi
	
	# %141 = &[s35 + 0]
	lea r10, [rbp - 0x270 + 0x0]
	mov qword ptr[rbp - 0x640], r10
	# %140 = move %141
	mov r10, qword ptr[rbp - 0x640]
	mov qword ptr[rbp - 0x648], r10
	# %142 = call f10(%140)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x648]
	call get_bird_age
	mov word ptr[rbp - 0x64a], ax
	pop rsi
	pop rdi
	
	# %144 = cast %142
	mov r10w, word ptr[rbp - 0x64a]
	mov r11, qword ptr[rbp - 0x658]
	movzx r11d, r10w
	mov qword ptr[rbp - 0x658], r11
	# %143 = 16
	mov byte ptr[rbp - 0x659], 16

	# %145 = 3
	mov qword ptr[rbp - 0x668], 3

	# call f14(%143, %144, %145)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x659]
	mov rsi, qword ptr[rbp - 0x658]
	mov rdx, qword ptr[rbp - 0x668]
	call assert_eq
	pop rsi
	pop rdi
	
	# %146 = call f11(%140)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x648]
	call get_bird_position_sum
	mov qword ptr[rbp - 0x670], rax
	pop rsi
	pop rdi
	
	# %147 = 17
	mov byte ptr[rbp - 0x671], 17

	# %148 = 42
	mov qword ptr[rbp - 0x680], 42

	# call f14(%147, %146, %148)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x671]
	mov rsi, qword ptr[rbp - 0x670]
	mov rdx, qword ptr[rbp - 0x680]
	call assert_eq
	pop rsi
	pop rdi
	
	# call f12(%140)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x648]
	call print_bird_name
	pop rsi
	pop rdi
	
	# %149 = "\\n"
	lea r10, [rip + .Lstr_7]
	mov qword ptr[rbp - 0x688], r10
	# [s37 + 0] = store %149
	mov r10, qword ptr[rbp - 0x688]
	mov qword ptr[rbp - 0x290], r10
	# %150 = 1
	mov qword ptr[rbp - 0x690], 1

	# [s37 + 8] = store %150
	mov r10, qword ptr[rbp - 0x690]
	mov qword ptr[rbp - 0x288], r10
	# [s38 + 0] = copy [s37 + 0]
	mov r10, qword ptr[rbp - 0x290]
	mov qword ptr[rbp - 0x2a0], r10
	mov r10, qword ptr[rbp - 0x288]
	mov qword ptr[rbp - 0x298], r10
	
	# %151 = &[s38 + 0]
	lea r10, [rbp - 0x2a0 + 0x0]
	mov qword ptr[rbp - 0x698], r10
	# call f15(%151)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x698]
	call print
	pop rsi
	pop rdi
	
	# %153 = 2
	mov qword ptr[rbp - 0x6a0], 2

	# %154 = 3
	mov qword ptr[rbp - 0x6a8], 3

	# %155 = call f5(%153, %154)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x6a0]
	mov rsi, qword ptr[rbp - 0x6a8]
	call add
	mov qword ptr[rbp - 0x6b0], rax
	pop rsi
	pop rdi
	
	# %156 = 4
	mov qword ptr[rbp - 0x6b8], 4

	# %157 = 5
	mov qword ptr[rbp - 0x6c0], 5

	# %158 = call f5(%156, %157)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x6b8]
	mov rsi, qword ptr[rbp - 0x6c0]
	call add
	mov qword ptr[rbp - 0x6c8], rax
	pop rsi
	pop rdi
	
	# %152 = call f6(%155, %158)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x6b0]
	mov rsi, qword ptr[rbp - 0x6c8]
	call multiply
	mov qword ptr[rbp - 0x6d0], rax
	pop rsi
	pop rdi
	
	# %159 = 18
	mov byte ptr[rbp - 0x6d1], 18

	# %160 = 45
	mov qword ptr[rbp - 0x6e0], 45

	# call f14(%159, %152, %160)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x6d1]
	mov rsi, qword ptr[rbp - 0x6d0]
	mov rdx, qword ptr[rbp - 0x6e0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %162 = 7
	mov qword ptr[rbp - 0x6e8], 7

	# %163 = 8
	mov qword ptr[rbp - 0x6f0], 8

	# %165 = %162 < %163
	mov r10, qword ptr[rbp - 0x6e8]
	mov r11, qword ptr[rbp - 0x6f0]
	cmp r10, r11
	setb byte ptr[rbp - 0x6f1]

	# Branch
	cmp byte ptr[rbp - 0x6f1], 0
	je .Lmain.if_else.11
	jmp .Lmain.if_then.10

.Lmain.if_then.10:
	# %166 = 5
	mov qword ptr[rbp - 0x700], 5

	# %167 = %162 + %163
	mov r10, qword ptr[rbp - 0x6e8]
	mov r11, qword ptr[rbp - 0x6f0]
	add r10, r11
	mov qword ptr[rbp - 0x708], r10
	
	# %168 = %167 + %166
	mov r10, qword ptr[rbp - 0x708]
	mov r11, qword ptr[rbp - 0x700]
	add r10, r11
	mov qword ptr[rbp - 0x710], r10
	
	# %164 = move %168
	mov r10, qword ptr[rbp - 0x710]
	mov qword ptr[rbp - 0x718], r10
	# Jump
	jmp .Lmain.if_merge.12

.Lmain.if_else.11:
	# %164 = 0
	mov qword ptr[rbp - 0x718], 0

	# Jump
	jmp .Lmain.if_merge.12

.Lmain.if_merge.12:
	# %161 = move %164
	mov r10, qword ptr[rbp - 0x718]
	mov qword ptr[rbp - 0x720], r10
	# %169 = 19
	mov byte ptr[rbp - 0x721], 19

	# %170 = 20
	mov qword ptr[rbp - 0x730], 20

	# call f14(%169, %161, %170)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x721]
	mov rsi, qword ptr[rbp - 0x720]
	mov rdx, qword ptr[rbp - 0x730]
	call assert_eq
	pop rsi
	pop rdi
	
	# %172 = %34 + %39
	mov r10, qword ptr[rbp - 0x330]
	mov r11, qword ptr[rbp - 0x368]
	add r10, r11
	mov qword ptr[rbp - 0x738], r10
	
	# %173 = %172 + %49
	mov r10, qword ptr[rbp - 0x738]
	mov r11, qword ptr[rbp - 0x3a0]
	add r10, r11
	mov qword ptr[rbp - 0x740], r10
	
	# %174 = %173 + %53
	mov r10, qword ptr[rbp - 0x740]
	mov r11, qword ptr[rbp - 0x3c0]
	add r10, r11
	mov qword ptr[rbp - 0x748], r10
	
	# %175 = %174 + %68
	mov r10, qword ptr[rbp - 0x748]
	mov r11, qword ptr[rbp - 0x440]
	add r10, r11
	mov qword ptr[rbp - 0x750], r10
	
	# %176 = %175 + %76
	mov r10, qword ptr[rbp - 0x750]
	mov r11, qword ptr[rbp - 0x478]
	add r10, r11
	mov qword ptr[rbp - 0x758], r10
	
	# %177 = %176 + %84
	mov r10, qword ptr[rbp - 0x758]
	mov r11, qword ptr[rbp - 0x4a0]
	add r10, r11
	mov qword ptr[rbp - 0x760], r10
	
	# %178 = %177 + %88
	mov r10, qword ptr[rbp - 0x760]
	mov r11, qword ptr[rbp - 0x4c0]
	add r10, r11
	mov qword ptr[rbp - 0x768], r10
	
	# %179 = %178 + %120
	mov r10, qword ptr[rbp - 0x768]
	mov r11, qword ptr[rbp - 0x5c8]
	add r10, r11
	mov qword ptr[rbp - 0x770], r10
	
	# %180 = %179 + %128
	mov r10, qword ptr[rbp - 0x770]
	mov r11, qword ptr[rbp - 0x5f0]
	add r10, r11
	mov qword ptr[rbp - 0x778], r10
	
	# %181 = %180 + %134
	mov r10, qword ptr[rbp - 0x778]
	mov r11, qword ptr[rbp - 0x628]
	add r10, r11
	mov qword ptr[rbp - 0x780], r10
	
	# %182 = %181 + %146
	mov r10, qword ptr[rbp - 0x780]
	mov r11, qword ptr[rbp - 0x670]
	add r10, r11
	mov qword ptr[rbp - 0x788], r10
	
	# %183 = %182 + %152
	mov r10, qword ptr[rbp - 0x788]
	mov r11, qword ptr[rbp - 0x6d0]
	add r10, r11
	mov qword ptr[rbp - 0x790], r10
	
	# %184 = %183 + %161
	mov r10, qword ptr[rbp - 0x790]
	mov r11, qword ptr[rbp - 0x720]
	add r10, r11
	mov qword ptr[rbp - 0x798], r10
	
	# %171 = move %184
	mov r10, qword ptr[rbp - 0x798]
	mov qword ptr[rbp - 0x7a0], r10
	# %185 = 20
	mov byte ptr[rbp - 0x7a1], 20

	# %186 = 1355
	mov qword ptr[rbp - 0x7b0], 1355

	# call f14(%185, %171, %186)
	push rdi
	push rsi
	mov dil, byte ptr[rbp - 0x7a1]
	mov rsi, qword ptr[rbp - 0x7a0]
	mov rdx, qword ptr[rbp - 0x7b0]
	call assert_eq
	pop rsi
	pop rdi
	
	# %187 = 0
	mov qword ptr[rbp - 0x7b8], 0

	# call f1(%187)
	push rdi
	push rsi
	mov rdi, qword ptr[rbp - 0x7b8]
	call crust_exit
	pop rsi
	pop rdi
	
.type add, @function
add:
.Ladd.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10

	mov rdi, rdi
	mov rsi, rsi
	# %2 = %0 + %1
	add rdi, rsi
	mov r9, rdi
	
	# Return
	mov rax, r9
	mov rsp, rbp
	pop rbp
	ret

.type multiply, @function
multiply:
.Lmultiply.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10

	mov rdi, rdi
	mov rsi, rsi
	# %2 = %0 * %1
	imul rdi, rsi
	mov r9, rdi
	
	# Return
	mov rax, r9
	mov rsp, rbp
	pop rbp
	ret

.type add_ref, @function
add_ref:
.Ladd_ref.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10

	mov rdi, rdi
	mov rsi, rsi
	# %2 = load [*%1 + 0]
	mov r9, [rsi + 0x0]
	# %3 = %0 + %2
	add rdi, r9
	mov r8, rdi
	
	# Return
	mov rax, r8
	mov rsp, rbp
	pop rbp
	ret

.type read_u16, @function
read_u16:
.Lread_u16.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x10

	mov rdi, rdi
	# %1 = load [*%0 + 0]
	mov r9, [rdi + 0x0]
	# Return
	mov ax, r9w
	mov rsp, rbp
	pop rbp
	ret

.type sum_position, @function
sum_position:
.Lsum_position.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x20

	mov rdi, rdi
	# [s0 + 0] = copy [*%0 + 0]
	mov r10, [rdi + 0x0]
	mov qword ptr[rbp - 0x10], r10
	mov r10, [rdi + 0x8]
	mov qword ptr[rbp - 0x8], r10
	
	# %1 = load [s0 + 0]
	mov r9, qword ptr[rbp - 0x10]
	# %2 = load [s0 + 8]
	mov r8, qword ptr[rbp - 0x8]
	# %3 = %1 + %2
	add r9, r8
	mov rsi, r9
	
	# Return
	mov rax, rsi
	mov rsp, rbp
	pop rbp
	ret

.type get_bird_age, @function
get_bird_age:
.Lget_bird_age.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x40

	mov rdi, rdi
	# [s0 + 0] = copy [*%0 + 0]
	mov r10, [rdi + 0x0]
	mov qword ptr[rbp - 0x30], r10
	mov r10, [rdi + 0x8]
	mov qword ptr[rbp - 0x28], r10
	mov r10, [rdi + 0x10]
	mov qword ptr[rbp - 0x20], r10
	mov r10, [rdi + 0x18]
	mov qword ptr[rbp - 0x18], r10
	mov r10, [rdi + 0x20]
	mov qword ptr[rbp - 0x10], r10
	mov r10, [rdi + 0x28]
	mov qword ptr[rbp - 0x8], r10
	
	# %1 = load [s0 + 0]
	movzx r9, word ptr[rbp - 0x30]
	# Return
	mov ax, r9w
	mov rsp, rbp
	pop rbp
	ret

.type get_bird_position_sum, @function
get_bird_position_sum:
.Lget_bird_position_sum.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x40

	mov rdi, rdi
	# [s0 + 0] = copy [*%0 + 0]
	mov r10, [rdi + 0x0]
	mov qword ptr[rbp - 0x30], r10
	mov r10, [rdi + 0x8]
	mov qword ptr[rbp - 0x28], r10
	mov r10, [rdi + 0x10]
	mov qword ptr[rbp - 0x20], r10
	mov r10, [rdi + 0x18]
	mov qword ptr[rbp - 0x18], r10
	mov r10, [rdi + 0x20]
	mov qword ptr[rbp - 0x10], r10
	mov r10, [rdi + 0x28]
	mov qword ptr[rbp - 0x8], r10
	
	# %1 = load [s0 + 16]
	mov r9, qword ptr[rbp - 0x20]
	# %2 = load [s0 + 24]
	mov r8, qword ptr[rbp - 0x18]
	# %3 = %1 + %2
	add r9, r8
	mov rsi, r9
	
	# Return
	mov rax, rsi
	mov rsp, rbp
	pop rbp
	ret

.type print_bird_name, @function
print_bird_name:
.Lprint_bird_name.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x40

	mov rdi, rdi
	# [s0 + 0] = copy [*%0 + 0]
	mov r10, [rdi + 0x0]
	mov qword ptr[rbp - 0x30], r10
	mov r10, [rdi + 0x8]
	mov qword ptr[rbp - 0x28], r10
	mov r10, [rdi + 0x10]
	mov qword ptr[rbp - 0x20], r10
	mov r10, [rdi + 0x18]
	mov qword ptr[rbp - 0x18], r10
	mov r10, [rdi + 0x20]
	mov qword ptr[rbp - 0x10], r10
	mov r10, [rdi + 0x28]
	mov qword ptr[rbp - 0x8], r10
	
	# %1 = &[s0 + 32]
	lea r9, [rbp - 0x30 + 0x20]
	# call f15(%1)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# Return
	mov rsp, rbp
	pop rbp
	ret

.type print_u64, @function
print_u64:
.Lprint_u64.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0xa0

	mov rdi, rdi
	# [s0 + 0] = for all 0
	push rax
	push rcx
	push rdi
	mov rax, 0
	mov rcx, 2
	lea rdi, [rbp - 0x14]
	rep stosq
	pop rdi
	pop rcx
	pop rax
	# %1 = 0
	mov r9b, 0

	# [s0 + 16] = store %1
	mov byte ptr[rbp - 0x4], r9b
	# %2 = 0
	mov r9b, 0

	# [s0 + 17] = store %2
	mov byte ptr[rbp - 0x3], r9b
	# %3 = 0
	mov r9b, 0

	# [s0 + 18] = store %3
	mov byte ptr[rbp - 0x2], r9b
	# %4 = 0
	mov r9b, 0

	# [s0 + 19] = store %4
	mov byte ptr[rbp - 0x1], r9b
	# [s1 + 0] = copy [s0 + 0]
	mov r10b, byte ptr[rbp - 0x14]
	mov byte ptr[rbp - 0x38], r10b
	mov r10b, byte ptr[rbp - 0x13]
	mov byte ptr[rbp - 0x37], r10b
	mov r10b, byte ptr[rbp - 0x12]
	mov byte ptr[rbp - 0x36], r10b
	mov r10b, byte ptr[rbp - 0x11]
	mov byte ptr[rbp - 0x35], r10b
	mov r10b, byte ptr[rbp - 0x10]
	mov byte ptr[rbp - 0x34], r10b
	mov r10b, byte ptr[rbp - 0xf]
	mov byte ptr[rbp - 0x33], r10b
	mov r10b, byte ptr[rbp - 0xe]
	mov byte ptr[rbp - 0x32], r10b
	mov r10b, byte ptr[rbp - 0xd]
	mov byte ptr[rbp - 0x31], r10b
	mov r10b, byte ptr[rbp - 0xc]
	mov byte ptr[rbp - 0x30], r10b
	mov r10b, byte ptr[rbp - 0xb]
	mov byte ptr[rbp - 0x2f], r10b
	mov r10b, byte ptr[rbp - 0xa]
	mov byte ptr[rbp - 0x2e], r10b
	mov r10b, byte ptr[rbp - 0x9]
	mov byte ptr[rbp - 0x2d], r10b
	mov r10b, byte ptr[rbp - 0x8]
	mov byte ptr[rbp - 0x2c], r10b
	mov r10b, byte ptr[rbp - 0x7]
	mov byte ptr[rbp - 0x2b], r10b
	mov r10b, byte ptr[rbp - 0x6]
	mov byte ptr[rbp - 0x2a], r10b
	mov r10b, byte ptr[rbp - 0x5]
	mov byte ptr[rbp - 0x29], r10b
	mov r10b, byte ptr[rbp - 0x4]
	mov byte ptr[rbp - 0x28], r10b
	mov r10b, byte ptr[rbp - 0x3]
	mov byte ptr[rbp - 0x27], r10b
	mov r10b, byte ptr[rbp - 0x2]
	mov byte ptr[rbp - 0x26], r10b
	mov r10b, byte ptr[rbp - 0x1]
	mov byte ptr[rbp - 0x25], r10b
	# %5 = 20
	mov r9, 20

	# [s2 + 0] = store %5
	mov qword ptr[rbp - 0x40], r9
	# [s3 + 0] = copy [s2 + 0]
	mov r10, qword ptr[rbp - 0x40]
	mov qword ptr[rbp - 0x48], r10
	
	# [s4 + 0] = store %0
	mov qword ptr[rbp - 0x50], rdi
	# [s5 + 0] = copy [s4 + 0]
	mov r10, qword ptr[rbp - 0x50]
	mov qword ptr[rbp - 0x58], r10
	
	# %6 = load [s5 + 0]
	mov r9, qword ptr[rbp - 0x58]
	# %7 = 0
	mov r8, 0

	# %8 = %6 == %7
	cmp r9, r8
	sete sil

	# Branch
	cmp sil, 0
	je .Lprint_u64.if_merge.3
	jmp .Lprint_u64.if_then.1

.Lprint_u64.if_then.1:
	# %9 = 48
	mov r9b, 48

	# [s6 + 0] = store %9
	mov byte ptr[rbp - 0x59], r9b
	# [s7 + 0] = copy [s6 + 0]
	movzx r10, byte ptr[rbp - 0x59]
	mov byte ptr[rbp - 0x5a], r10b
	
	# %10 = &[s7 + 0]
	lea r9, [rbp - 0x5a + 0x0]
	# %11 = 1
	mov rsi, 1

	# %12 = call f0(%10, %11)
	push rsi
	push rdi
	mov rdi, r9
	call crust_write
	mov r8, rax
	pop rdi
	pop rsi
	
	# Return
	mov rsp, rbp
	pop rbp
	ret

.Lprint_u64.if_merge.3:
	# %13 = load [s5 + 0]
	mov r9, qword ptr[rbp - 0x58]
	# %14 = 0
	mov rdx, 0

	# %15 = %13 > %14
	cmp r9, rdx
	seta cl

	# Branch
	cmp cl, 0
	je .Lprint_u64.while_merge.4
	jmp .Lprint_u64.while_loop.3

.Lprint_u64.while_loop.3:
	# %17 = load [s5 + 0]
	mov r9, qword ptr[rbp - 0x58]
	# %18 = 10
	mov rdx, 10

	# %19 = %17 % %18
	push rax
	push rdx
	mov r11, rdx
	xor rdx, rdx
	mov rax, r9
	div r11
	mov ecx, edx
	pop rdx
	pop rax
	
	# %20 = cast %19
	movzx r9d, cl
	# %16 = move %20
	mov dl, r9b
	# %21 = load [s3 + 0]
	mov r9, qword ptr[rbp - 0x48]
	# %22 = 1
	mov rcx, 1

	# %23 = %21 - %22
	sub r9, rcx
	mov rax, r9
	
	# [s3 + 0] = store %23
	mov qword ptr[rbp - 0x48], rax
	# %24 = 48
	mov r9b, 48

	# %25 = %16 + %24
	add dl, r9b
	mov cl, dl
	
	# %26 = load [s3 + 0]
	mov r9, qword ptr[rbp - 0x48]
	# [[s1 + 0] + %26] = store %25
	lea r11, [rbp - 0x38]
	mov [r11 + r9 * 1], cl
	# %27 = load [s5 + 0]
	mov rcx, qword ptr[rbp - 0x58]
	# %28 = 10
	mov rax, 10

	# %29 = %27 / %28
	mov r10, qword ptr[rbp - 0x68]
	push rax
	push rdx
	xor rdx, rdx
	mov rax, rcx
	div rax
	mov r10d, eax
	mov qword ptr[rbp - 0x68], r10
	pop rdx
	pop rax
	
	# [s5 + 0] = store %29
	mov r10, qword ptr[rbp - 0x68]
	mov qword ptr[rbp - 0x58], r10
	# %30 = load [s5 + 0]
	mov rcx, qword ptr[rbp - 0x58]
	# %31 = 0
	mov rax, 0

	# %32 = %30 > %31
	cmp rcx, rax
	seta byte ptr[rbp - 0x69]

	# Branch
	cmp byte ptr[rbp - 0x69], 0
	je .Lprint_u64.while_merge.4
	jmp .Lprint_u64.while_loop.3

.Lprint_u64.while_merge.4:
	# %33 = load [s3 + 0]
	mov rcx, qword ptr[rbp - 0x48]
	# %34 = 20
	mov rax, 20

	# %35 = %33 < %34
	cmp rcx, rax
	setb byte ptr[rbp - 0x6a]

	# Branch
	cmp byte ptr[rbp - 0x6a], 0
	je .Lprint_u64.while_merge.6
	jmp .Lprint_u64.while_loop.5

.Lprint_u64.while_loop.5:
	# %36 = load [s3 + 0]
	mov rcx, qword ptr[rbp - 0x48]
	# %37 = &[[s1 + 0] + %36]
	lea r11, [rbp - 0x38]
	lea rax, [r11 + rcx * 1]
	# %38 = 1
	mov qword ptr[rbp - 0x78], 1

	# %39 = call f0(%37, %38)
	push rsi
	push rdi
	mov rdi, rax
	mov rsi, qword ptr[rbp - 0x78]
	call crust_write
	mov qword ptr[rbp - 0x80], rax
	pop rdi
	pop rsi
	
	# %40 = load [s3 + 0]
	mov rax, qword ptr[rbp - 0x48]
	# %41 = 1
	mov qword ptr[rbp - 0x88], 1

	# %42 = %40 + %41
	mov r11, qword ptr[rbp - 0x88]
	add rax, r11
	mov qword ptr[rbp - 0x90], rax
	
	# [s3 + 0] = store %42
	mov r10, qword ptr[rbp - 0x90]
	mov qword ptr[rbp - 0x48], r10
	# %43 = load [s3 + 0]
	mov rax, qword ptr[rbp - 0x48]
	# %44 = 20
	mov qword ptr[rbp - 0x98], 20

	# %45 = %43 < %44
	mov r11, qword ptr[rbp - 0x98]
	cmp rax, r11
	setb byte ptr[rbp - 0x99]

	# Branch
	cmp byte ptr[rbp - 0x99], 0
	je .Lprint_u64.while_merge.6
	jmp .Lprint_u64.while_loop.5

.Lprint_u64.while_merge.6:
	# Return
	mov rsp, rbp
	pop rbp
	ret

.type assert_eq, @function
assert_eq:
.Lassert_eq.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0xe0

	mov byte ptr[rbp - 0xc1], dil
	mov qword ptr[rbp - 0xd0], rsi
	mov qword ptr[rbp - 0xd8], rdx
	# %3 = "test "
	lea r9, [rip + .Lstr_8]
	# [s0 + 0] = store %3
	mov qword ptr[rbp - 0x10], r9
	# %4 = 5
	mov r9, 5

	# [s0 + 8] = store %4
	mov qword ptr[rbp - 0x8], r9
	# [s1 + 0] = copy [s0 + 0]
	mov r10, qword ptr[rbp - 0x10]
	mov qword ptr[rbp - 0x20], r10
	mov r10, qword ptr[rbp - 0x8]
	mov qword ptr[rbp - 0x18], r10
	
	# %5 = " passed! "
	lea r9, [rip + .Lstr_9]
	# [s2 + 0] = store %5
	mov qword ptr[rbp - 0x30], r9
	# %6 = 9
	mov r9, 9

	# [s2 + 8] = store %6
	mov qword ptr[rbp - 0x28], r9
	# [s3 + 0] = copy [s2 + 0]
	mov r10, qword ptr[rbp - 0x30]
	mov qword ptr[rbp - 0x40], r10
	mov r10, qword ptr[rbp - 0x28]
	mov qword ptr[rbp - 0x38], r10
	
	# %7 = " failed! "
	lea r9, [rip + .Lstr_10]
	# [s4 + 0] = store %7
	mov qword ptr[rbp - 0x50], r9
	# %8 = 9
	mov r9, 9

	# [s4 + 8] = store %8
	mov qword ptr[rbp - 0x48], r9
	# [s5 + 0] = copy [s4 + 0]
	mov r10, qword ptr[rbp - 0x50]
	mov qword ptr[rbp - 0x60], r10
	mov r10, qword ptr[rbp - 0x48]
	mov qword ptr[rbp - 0x58], r10
	
	# %9 = " == "
	lea r9, [rip + .Lstr_11]
	# [s6 + 0] = store %9
	mov qword ptr[rbp - 0x70], r9
	# %10 = 4
	mov r9, 4

	# [s6 + 8] = store %10
	mov qword ptr[rbp - 0x68], r9
	# [s7 + 0] = copy [s6 + 0]
	mov r10, qword ptr[rbp - 0x70]
	mov qword ptr[rbp - 0x80], r10
	mov r10, qword ptr[rbp - 0x68]
	mov qword ptr[rbp - 0x78], r10
	
	# %11 = " != "
	lea r9, [rip + .Lstr_12]
	# [s8 + 0] = store %11
	mov qword ptr[rbp - 0x90], r9
	# %12 = 4
	mov r9, 4

	# [s8 + 8] = store %12
	mov qword ptr[rbp - 0x88], r9
	# [s9 + 0] = copy [s8 + 0]
	mov r10, qword ptr[rbp - 0x90]
	mov qword ptr[rbp - 0xa0], r10
	mov r10, qword ptr[rbp - 0x88]
	mov qword ptr[rbp - 0x98], r10
	
	# %13 = "\\n"
	lea r9, [rip + .Lstr_13]
	# [s10 + 0] = store %13
	mov qword ptr[rbp - 0xb0], r9
	# %14 = 1
	mov r9, 1

	# [s10 + 8] = store %14
	mov qword ptr[rbp - 0xa8], r9
	# [s11 + 0] = copy [s10 + 0]
	mov r10, qword ptr[rbp - 0xb0]
	mov qword ptr[rbp - 0xc0], r10
	mov r10, qword ptr[rbp - 0xa8]
	mov qword ptr[rbp - 0xb8], r10
	
	# %15 = %1 == %2
	mov r10, qword ptr[rbp - 0xd0]
	mov r11, qword ptr[rbp - 0xd8]
	cmp r10, r11
	sete r9b

	# Branch
	cmp r9b, 0
	je .Lassert_eq.if_else.2
	jmp .Lassert_eq.if_then.1

.Lassert_eq.if_then.1:
	# %16 = &[s1 + 0]
	lea rdi, [rbp - 0x20 + 0x0]
	# call f15(%16)
	push rdi
	call print
	pop rdi
	
	# %17 = cast %0
	mov r10b, byte ptr[rbp - 0xc1]
	movzx r9d, r10b
	# call f13(%17)
	push rdi
	mov rdi, r9
	call print_u64
	pop rdi
	
	# %18 = &[s3 + 0]
	lea r9, [rbp - 0x40 + 0x0]
	# call f15(%18)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# call f13(%1)
	push rdi
	mov rdi, qword ptr[rbp - 0xd0]
	call print_u64
	pop rdi
	
	# %19 = &[s7 + 0]
	lea r9, [rbp - 0x80 + 0x0]
	# call f15(%19)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# call f13(%2)
	push rdi
	mov rdi, qword ptr[rbp - 0xd8]
	call print_u64
	pop rdi
	
	# %20 = &[s11 + 0]
	lea r9, [rbp - 0xc0 + 0x0]
	# call f15(%20)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# Jump
	jmp .Lassert_eq.if_merge.3

.Lassert_eq.if_else.2:
	# %21 = &[s1 + 0]
	lea r9, [rbp - 0x20 + 0x0]
	# call f15(%21)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# %22 = cast %0
	mov r10b, byte ptr[rbp - 0xc1]
	movzx r9d, r10b
	# call f13(%22)
	push rdi
	mov rdi, r9
	call print_u64
	pop rdi
	
	# %23 = &[s5 + 0]
	lea r9, [rbp - 0x60 + 0x0]
	# call f15(%23)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# call f13(%1)
	push rdi
	mov rdi, qword ptr[rbp - 0xd0]
	call print_u64
	pop rdi
	
	# %24 = &[s9 + 0]
	lea r9, [rbp - 0xa0 + 0x0]
	# call f15(%24)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# call f13(%2)
	push rdi
	mov rdi, qword ptr[rbp - 0xd8]
	call print_u64
	pop rdi
	
	# %25 = &[s11 + 0]
	lea r9, [rbp - 0xc0 + 0x0]
	# call f15(%25)
	push rdi
	mov rdi, r9
	call print
	pop rdi
	
	# Jump
	jmp .Lassert_eq.if_merge.3

.Lassert_eq.if_merge.3:
	# Return
	mov rsp, rbp
	pop rbp
	ret

.type print, @function
print:
.Lprint.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x20

	mov rdi, rdi
	# [s0 + 0] = copy [*%0 + 0]
	mov r10, [rdi + 0x0]
	mov qword ptr[rbp - 0x10], r10
	mov r10, [rdi + 0x8]
	mov qword ptr[rbp - 0x8], r10
	
	# %1 = load [s0 + 0]
	mov r9, qword ptr[rbp - 0x10]
	# %2 = load [s0 + 8]
	mov rsi, qword ptr[rbp - 0x8]
	# %3 = call f0(%1, %2)
	push rsi
	push rdi
	mov rdi, r9
	call crust_write
	mov r8, rax
	pop rdi
	pop rsi
	
	# Return
	mov rsp, rbp
	pop rbp
	ret

.type allocator_new, @function
allocator_new:
.Lallocator_new.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x50

	mov rdi, rdi
	# %1 = 65536
	mov qword ptr[rbp - 0x48], 65536

	# %3 = call f2(%1)
	push rdi
	mov rdi, qword ptr[rbp - 0x48]
	call crust_mmap
	mov r9, rax
	pop rdi
	
	# %4 = cast %3
	mov r8d, r9d
	# %2 = move %4
	mov r9, r8
	# [s1 + 0] = store %2
	mov qword ptr[rbp - 0x20], r9
	# %5 = load [s1 + 0]
	mov r8, qword ptr[rbp - 0x20]
	# [s0 + 0] = store %5
	mov qword ptr[rbp - 0x18], r8
	# [s2 + 0] = store %2
	mov qword ptr[rbp - 0x28], r9
	# %6 = load [s2 + 0]
	mov r8, qword ptr[rbp - 0x28]
	# [s0 + 8] = store %6
	mov qword ptr[rbp - 0x10], r8
	# %7 = %2 + %1
	mov r11, qword ptr[rbp - 0x48]
	add r9, r11
	mov r8, r9
	
	# %8 = move %7
	mov rsi, r8
	# [s0 + 16] = store %8
	mov qword ptr[rbp - 0x8], rsi
	# [s4 + 0] = copy [s0 + 0]
	mov r10, qword ptr[rbp - 0x18]
	mov qword ptr[rbp - 0x40], r10
	mov r10, qword ptr[rbp - 0x10]
	mov qword ptr[rbp - 0x38], r10
	mov r10, qword ptr[rbp - 0x8]
	mov qword ptr[rbp - 0x30], r10
	
	# [*%0 + 0] = copy [s4 + 0]
	mov r10, qword ptr[rbp - 0x40]
	mov [rdi + 0x0], r10
	mov r10, qword ptr[rbp - 0x38]
	mov [rdi + 0x8], r10
	mov r10, qword ptr[rbp - 0x30]
	mov [rdi + 0x10], r10
	
	# Return
	mov rsp, rbp
	pop rbp
	ret


.section .rodata
.Lstr_0:
	.ascii "hello"
.Lstr_1:
	.ascii " "
.Lstr_2:
	.ascii "world"
.Lstr_3:
	.ascii "\n"
.Lstr_4:
	.ascii "first string\n"
.Lstr_5:
	.ascii "through &str\n"
.Lstr_6:
	.ascii "sparrow"
.Lstr_7:
	.ascii "\n"
.Lstr_8:
	.ascii "test "
.Lstr_9:
	.ascii " passed! "
.Lstr_10:
	.ascii " failed! "
.Lstr_11:
	.ascii " == "
.Lstr_12:
	.ascii " != "
.Lstr_13:
	.ascii "\n"
