.intel_syntax noprefix
.section .text
.global main
.type main, @function
main:
.Lmain.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x750

	# %1 = 4096
	mov rdi, 4096

	# %0 = call f2(%1)
	call crust_mmap
	mov qword ptr[rbp - 0x268], rax
	
	# %2 = 4096
	mov rsi, 4096

	# call f3(%0, %2)
	mov rdi, qword ptr[rbp - 0x268]
	call crust_munmap
	
	# %3 = load [*%0 + 0]
	mov r10, qword ptr[rbp - 0x268]
	mov r9, [r10 + 0x0]
	# %4 = cast %3
	movzx r8d, r9b
	# call f13(%4)
	mov rdi, r8
	call print_u64
	
	# %5 = 10
	mov r9, 10

	# %7 = 1
	mov r8, 1

	# [s0 + 0] = store %7
	mov qword ptr[rbp - 0x20], r8
	# %8 = 2
	mov r8, 2

	# [s0 + 8] = store %8
	mov qword ptr[rbp - 0x18], r8
	# %9 = 3
	mov r8, 3

	# [s0 + 16] = store %9
	mov qword ptr[rbp - 0x10], r8
	# %10 = 4
	mov r8, 4

	# [s0 + 24] = store %10
	mov qword ptr[rbp - 0x8], r8
	# [s1 + 0] = copy [s0 + 0]
	mov r10, qword ptr[rbp - 0x20]
	mov qword ptr[rbp - 0x40], r10
	mov r10, qword ptr[rbp - 0x18]
	mov qword ptr[rbp - 0x38], r10
	mov r10, qword ptr[rbp - 0x10]
	mov qword ptr[rbp - 0x30], r10
	mov r10, qword ptr[rbp - 0x8]
	mov qword ptr[rbp - 0x28], r10
	# %12 = &[s1 + 0]
	lea r8, [rbp - 0x40 + 0x0]
	# %13 = 2
	mov rdi, 2

	# %14 = [%12 + %13]
	mov rsi, [r8 + rdi * 0x8]
	# %11 = move %14
	mov r8, rsi
	# %16 = cast %11
	mov edi, r8d
	# %17 = %16 + %5
	add rdi, r9
	mov rsi, rdi
	
	# %6 = 20
	mov rdi, 20

	# %15 = call f5(%17, %6)
	push rsi
	push rdi
	pop r10
	mov rsi, r10
	pop r10
	mov rdi, r10
	call add
	mov qword ptr[rbp - 0x270], rax
	
	# %18 = 1
	mov sil, 1

	# %19 = 33
	mov rdx, 33

	# call f14(%18, %15, %19)
	push rsi
	mov rsi, qword ptr[rbp - 0x270]
	pop r10
	mov dil, r10b
	call assert_eq
	
	# %21 = 5
	mov rsi, 5

	# %22 = 7
	mov rdx, 7

	# %23 = %21 + %22
	add rsi, rdx
	mov rcx, rsi
	
	# %20 = move %23
	mov qword ptr[rbp - 0x278], rcx
	# %24 = 2
	mov cl, 2

	# %25 = 12
	mov rax, 12

	# call f14(%24, %20, %25)
	mov dil, cl
	mov rsi, qword ptr[rbp - 0x278]
	mov rdx, rax
	call assert_eq
	
	# %26 = 2
	mov rcx, 2

	# %29 = 3
	mov rax, 3

	# %30 = %26 + %29
	add rcx, rax
	mov qword ptr[rbp - 0x280], rcx
	
	# %28 = move %30
	mov r10, qword ptr[rbp - 0x280]
	mov rax, r10
	# %31 = 2
	mov qword ptr[rbp - 0x288], 2

	# %32 = %28 * %31
	mov r11, qword ptr[rbp - 0x288]
	imul rax, r11
	mov qword ptr[rbp - 0x290], rax
	
	# %27 = move %32
	mov r10, qword ptr[rbp - 0x290]
	mov qword ptr[rbp - 0x298], r10
	# %33 = 3
	mov byte ptr[rbp - 0x299], 3

	# %34 = 10
	mov qword ptr[rbp - 0x2a8], 10

	# call f14(%33, %27, %34)
	mov dil, byte ptr[rbp - 0x299]
	mov rsi, qword ptr[rbp - 0x298]
	mov rdx, qword ptr[rbp - 0x2a8]
	call assert_eq
	
	# %36 = 20
	mov qword ptr[rbp - 0x2b0], 20

	# %37 = %15 > %36
	mov r10, qword ptr[rbp - 0x270]
	mov r11, qword ptr[rbp - 0x2b0]
	cmp r10, r11
	seta byte ptr[rbp - 0x2b1]

	# Branch
	cmp byte ptr[rbp - 0x2b1], 0
	je .Lmain.if_else.2
	jmp .Lmain.if_then.1

.Lmain.if_then.1:
	# %35 = 8
	mov qword ptr[rbp - 0x2c0], 8

	# Jump
	jmp .Lmain.if_merge.3

.Lmain.if_else.2:
	# %35 = 4
	mov qword ptr[rbp - 0x2c0], 4

	# Jump
	jmp .Lmain.if_merge.3

.Lmain.if_merge.3:
	# %38 = 4
	mov byte ptr[rbp - 0x2c1], 4

	# %39 = 8
	mov qword ptr[rbp - 0x2d0], 8

	# call f14(%38, %35, %39)
	mov dil, byte ptr[rbp - 0x2c1]
	mov rsi, qword ptr[rbp - 0x2c0]
	mov rdx, qword ptr[rbp - 0x2d0]
	call assert_eq
	
	# %41 = 8
	mov qword ptr[rbp - 0x2d8], 8

	# %42 = %35 == %41
	mov r10, qword ptr[rbp - 0x2c0]
	mov r11, qword ptr[rbp - 0x2d8]
	cmp r10, r11
	sete byte ptr[rbp - 0x2d9]

	# Branch
	cmp byte ptr[rbp - 0x2d9], 0
	je .Lmain.if_else.5
	jmp .Lmain.if_then.4

.Lmain.if_then.4:
	# %43 = 12
	mov qword ptr[rbp - 0x2e8], 12

	# %44 = %20 == %43
	mov r10, qword ptr[rbp - 0x278]
	mov r11, qword ptr[rbp - 0x2e8]
	cmp r10, r11
	sete byte ptr[rbp - 0x2e9]

	# Branch
	cmp byte ptr[rbp - 0x2e9], 0
	je .Lmain.if_else.6
	jmp .Lmain.if_then.5

.Lmain.if_then.5:
	# %40 = 11
	mov qword ptr[rbp - 0x2f8], 11

	# Jump
	jmp .Lmain.if_merge.7

.Lmain.if_else.6:
	# %40 = 100
	mov qword ptr[rbp - 0x2f8], 100

	# Jump
	jmp .Lmain.if_merge.7

.Lmain.if_merge.7:
	# Jump
	jmp .Lmain.if_merge.6

.Lmain.if_else.5:
	# %40 = 200
	mov qword ptr[rbp - 0x2f8], 200

	# Jump
	jmp .Lmain.if_merge.6

.Lmain.if_merge.6:
	# %45 = 5
	mov byte ptr[rbp - 0x2f9], 5

	# %46 = 11
	mov qword ptr[rbp - 0x308], 11

	# call f14(%45, %40, %46)
	mov dil, byte ptr[rbp - 0x2f9]
	mov rsi, qword ptr[rbp - 0x2f8]
	mov rdx, qword ptr[rbp - 0x308]
	call assert_eq
	
	# %47 = 50
	mov qword ptr[rbp - 0x310], 50

	# [s2 + 0] = store %47
	mov r10, qword ptr[rbp - 0x310]
	mov qword ptr[rbp - 0x48], r10
	# [s3 + 0] = copy [s2 + 0]
	mov r10, qword ptr[rbp - 0x48]
	mov qword ptr[rbp - 0x50], r10
	
	# %49 = &[s3 + 0]
	lea r10, [rbp - 0x50 + 0x0]
	mov qword ptr[rbp - 0x318], r10
	# %48 = move %49
	mov r10, qword ptr[rbp - 0x318]
	mov qword ptr[rbp - 0x320], r10
	# %51 = load [*%48 + 0]
	mov r10, qword ptr[rbp - 0x320]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x328], r10
	# %50 = move %51
	mov r10, qword ptr[rbp - 0x328]
	mov qword ptr[rbp - 0x330], r10
	# %52 = 6
	mov byte ptr[rbp - 0x331], 6

	# %53 = 50
	mov qword ptr[rbp - 0x340], 50

	# call f14(%52, %50, %53)
	mov dil, byte ptr[rbp - 0x331]
	mov rsi, qword ptr[rbp - 0x330]
	mov rdx, qword ptr[rbp - 0x340]
	call assert_eq
	
	# %55 = 5
	mov qword ptr[rbp - 0x348], 5

	# %54 = call f7(%55, %48)
	mov rdi, qword ptr[rbp - 0x348]
	mov rsi, qword ptr[rbp - 0x320]
	call add_ref
	mov qword ptr[rbp - 0x350], rax
	
	# %56 = 7
	mov byte ptr[rbp - 0x351], 7

	# %57 = 55
	mov qword ptr[rbp - 0x360], 55

	# call f14(%56, %54, %57)
	mov dil, byte ptr[rbp - 0x351]
	mov rsi, qword ptr[rbp - 0x350]
	mov rdx, qword ptr[rbp - 0x360]
	call assert_eq
	
	# %58 = 6
	mov word ptr[rbp - 0x362], 6

	# [s4 + 0] = store %58
	mov r10w, word ptr[rbp - 0x362]
	mov word ptr[rbp - 0x52], r10w
	# [s5 + 0] = copy [s4 + 0]
	movzx r10, word ptr[rbp - 0x52]
	mov word ptr[rbp - 0x54], r10w
	
	# %59 = &[s5 + 0]
	lea r10, [rbp - 0x54 + 0x0]
	mov qword ptr[rbp - 0x370], r10
	# %61 = call f8(%59)
	mov rdi, qword ptr[rbp - 0x370]
	call read_u16
	mov word ptr[rbp - 0x372], ax
	
	# %63 = cast %61
	mov r10w, word ptr[rbp - 0x372]
	mov r11, qword ptr[rbp - 0x380]
	movzx r11d, r10w
	mov qword ptr[rbp - 0x380], r11
	# %62 = 8
	mov byte ptr[rbp - 0x381], 8

	# %64 = 6
	mov qword ptr[rbp - 0x390], 6

	# call f14(%62, %63, %64)
	mov dil, byte ptr[rbp - 0x381]
	mov rsi, qword ptr[rbp - 0x380]
	mov rdx, qword ptr[rbp - 0x390]
	call assert_eq
	
	# %65 = 13
	mov qword ptr[rbp - 0x398], 13

	# %66 = move %65
	mov r10, qword ptr[rbp - 0x398]
	mov qword ptr[rbp - 0x3a0], r10
	# [s6 + 0] = store %66
	mov r10, qword ptr[rbp - 0x3a0]
	mov qword ptr[rbp - 0x70], r10
	# %67 = 45
	mov qword ptr[rbp - 0x3a8], 45

	# %68 = move %67
	mov r10, qword ptr[rbp - 0x3a8]
	mov qword ptr[rbp - 0x3b0], r10
	# [s6 + 8] = store %68
	mov r10, qword ptr[rbp - 0x3b0]
	mov qword ptr[rbp - 0x68], r10
	# [s9 + 0] = copy [s6 + 0]
	mov r10, qword ptr[rbp - 0x70]
	mov qword ptr[rbp - 0x80], r10
	mov r10, qword ptr[rbp - 0x68]
	mov qword ptr[rbp - 0x78], r10
	
	# [s10 + 0] = copy [s9 + 0]
	mov r10, qword ptr[rbp - 0x80]
	mov qword ptr[rbp - 0x90], r10
	mov r10, qword ptr[rbp - 0x78]
	mov qword ptr[rbp - 0x88], r10
	
	# %70 = load [s10 + 0]
	mov r10, qword ptr[rbp - 0x90]
	mov qword ptr[rbp - 0x3b8], r10
	# %71 = load [s10 + 8]
	mov r10, qword ptr[rbp - 0x88]
	mov qword ptr[rbp - 0x3c0], r10
	# %72 = %70 + %71
	mov r10, qword ptr[rbp - 0x3b8]
	mov r11, qword ptr[rbp - 0x3c0]
	add r10, r11
	mov qword ptr[rbp - 0x3c8], r10
	
	# %69 = move %72
	mov r10, qword ptr[rbp - 0x3c8]
	mov qword ptr[rbp - 0x3d0], r10
	# %73 = 9
	mov byte ptr[rbp - 0x3d1], 9

	# %74 = 58
	mov qword ptr[rbp - 0x3e0], 58

	# call f14(%73, %69, %74)
	mov dil, byte ptr[rbp - 0x3d1]
	mov rsi, qword ptr[rbp - 0x3d0]
	mov rdx, qword ptr[rbp - 0x3e0]
	call assert_eq
	
	# %75 = &[s9 + 0]
	lea r10, [rbp - 0x80 + 0x0]
	mov qword ptr[rbp - 0x3e8], r10
	# [s11 + 0] = copy [*%75 + 0]
	mov r10, qword ptr[rbp - 0x3e8]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0xa0], r10
	mov r10, qword ptr[rbp - 0x3e8]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0x98], r10
	
	# %78 = load [s11 + 0]
	mov r10, qword ptr[rbp - 0xa0]
	mov qword ptr[rbp - 0x3f0], r10
	# %79 = load [s11 + 8]
	mov r10, qword ptr[rbp - 0x98]
	mov qword ptr[rbp - 0x3f8], r10
	# %80 = %78 + %79
	mov r10, qword ptr[rbp - 0x3f0]
	mov r11, qword ptr[rbp - 0x3f8]
	add r10, r11
	mov qword ptr[rbp - 0x400], r10
	
	# %77 = move %80
	mov r10, qword ptr[rbp - 0x400]
	mov qword ptr[rbp - 0x408], r10
	# %81 = 10
	mov byte ptr[rbp - 0x409], 10

	# %82 = 58
	mov qword ptr[rbp - 0x418], 58

	# call f14(%81, %77, %82)
	mov dil, byte ptr[rbp - 0x409]
	mov rsi, qword ptr[rbp - 0x408]
	mov rdx, qword ptr[rbp - 0x418]
	call assert_eq
	
	# %83 = &[s9 + 8]
	lea r10, [rbp - 0x80 + 0x8]
	mov qword ptr[rbp - 0x420], r10
	# %86 = load [*%83 + 0]
	mov r10, qword ptr[rbp - 0x420]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x428], r10
	# %85 = move %86
	mov r10, qword ptr[rbp - 0x428]
	mov qword ptr[rbp - 0x430], r10
	# %87 = 11
	mov byte ptr[rbp - 0x431], 11

	# %88 = 45
	mov qword ptr[rbp - 0x440], 45

	# call f14(%87, %85, %88)
	mov dil, byte ptr[rbp - 0x431]
	mov rsi, qword ptr[rbp - 0x430]
	mov rdx, qword ptr[rbp - 0x440]
	call assert_eq
	
	# %90 = &[s9 + 0]
	lea r10, [rbp - 0x80 + 0x0]
	mov qword ptr[rbp - 0x448], r10
	# %89 = call f9(%90)
	mov rdi, qword ptr[rbp - 0x448]
	call sum_position
	mov qword ptr[rbp - 0x450], rax
	
	# %91 = 12
	mov byte ptr[rbp - 0x451], 12

	# %92 = 58
	mov qword ptr[rbp - 0x460], 58

	# call f14(%91, %89, %92)
	mov dil, byte ptr[rbp - 0x451]
	mov rsi, qword ptr[rbp - 0x450]
	mov rdx, qword ptr[rbp - 0x460]
	call assert_eq
	
	# %93 = "hello"
	lea r10, [rip + .Lstr_0]
	mov qword ptr[rbp - 0x468], r10
	# [s12 + 0] = store %93
	mov r10, qword ptr[rbp - 0x468]
	mov qword ptr[rbp - 0xb0], r10
	# %94 = 5
	mov qword ptr[rbp - 0x470], 5

	# [s12 + 8] = store %94
	mov r10, qword ptr[rbp - 0x470]
	mov qword ptr[rbp - 0xa8], r10
	# [s13 + 0] = copy [s12 + 0]
	mov r10, qword ptr[rbp - 0xb0]
	mov qword ptr[rbp - 0xc0], r10
	mov r10, qword ptr[rbp - 0xa8]
	mov qword ptr[rbp - 0xb8], r10
	
	# %95 = &[s13 + 0]
	lea r10, [rbp - 0xc0 + 0x0]
	mov qword ptr[rbp - 0x478], r10
	# call f15(%95)
	mov rdi, qword ptr[rbp - 0x478]
	call print
	
	# %96 = " "
	lea r10, [rip + .Lstr_1]
	mov qword ptr[rbp - 0x480], r10
	# [s14 + 0] = store %96
	mov r10, qword ptr[rbp - 0x480]
	mov qword ptr[rbp - 0xd0], r10
	# %97 = 1
	mov qword ptr[rbp - 0x488], 1

	# [s14 + 8] = store %97
	mov r10, qword ptr[rbp - 0x488]
	mov qword ptr[rbp - 0xc8], r10
	# [s15 + 0] = copy [s14 + 0]
	mov r10, qword ptr[rbp - 0xd0]
	mov qword ptr[rbp - 0xe0], r10
	mov r10, qword ptr[rbp - 0xc8]
	mov qword ptr[rbp - 0xd8], r10
	
	# %98 = &[s15 + 0]
	lea r10, [rbp - 0xe0 + 0x0]
	mov qword ptr[rbp - 0x490], r10
	# call f15(%98)
	mov rdi, qword ptr[rbp - 0x490]
	call print
	
	# %99 = "world"
	lea r10, [rip + .Lstr_2]
	mov qword ptr[rbp - 0x498], r10
	# [s16 + 0] = store %99
	mov r10, qword ptr[rbp - 0x498]
	mov qword ptr[rbp - 0xf0], r10
	# %100 = 5
	mov qword ptr[rbp - 0x4a0], 5

	# [s16 + 8] = store %100
	mov r10, qword ptr[rbp - 0x4a0]
	mov qword ptr[rbp - 0xe8], r10
	# [s17 + 0] = copy [s16 + 0]
	mov r10, qword ptr[rbp - 0xf0]
	mov qword ptr[rbp - 0x100], r10
	mov r10, qword ptr[rbp - 0xe8]
	mov qword ptr[rbp - 0xf8], r10
	
	# %101 = &[s17 + 0]
	lea r10, [rbp - 0x100 + 0x0]
	mov qword ptr[rbp - 0x4a8], r10
	# call f15(%101)
	mov rdi, qword ptr[rbp - 0x4a8]
	call print
	
	# %102 = "\\n"
	lea r10, [rip + .Lstr_3]
	mov qword ptr[rbp - 0x4b0], r10
	# [s18 + 0] = store %102
	mov r10, qword ptr[rbp - 0x4b0]
	mov qword ptr[rbp - 0x110], r10
	# %103 = 1
	mov qword ptr[rbp - 0x4b8], 1

	# [s18 + 8] = store %103
	mov r10, qword ptr[rbp - 0x4b8]
	mov qword ptr[rbp - 0x108], r10
	# [s19 + 0] = copy [s18 + 0]
	mov r10, qword ptr[rbp - 0x110]
	mov qword ptr[rbp - 0x120], r10
	mov r10, qword ptr[rbp - 0x108]
	mov qword ptr[rbp - 0x118], r10
	
	# %104 = &[s19 + 0]
	lea r10, [rbp - 0x120 + 0x0]
	mov qword ptr[rbp - 0x4c0], r10
	# call f15(%104)
	mov rdi, qword ptr[rbp - 0x4c0]
	call print
	
	# %105 = "first string\\n"
	lea r10, [rip + .Lstr_4]
	mov qword ptr[rbp - 0x4c8], r10
	# [s20 + 0] = store %105
	mov r10, qword ptr[rbp - 0x4c8]
	mov qword ptr[rbp - 0x130], r10
	# %106 = 13
	mov qword ptr[rbp - 0x4d0], 13

	# [s20 + 8] = store %106
	mov r10, qword ptr[rbp - 0x4d0]
	mov qword ptr[rbp - 0x128], r10
	# [s21 + 0] = copy [s20 + 0]
	mov r10, qword ptr[rbp - 0x130]
	mov qword ptr[rbp - 0x140], r10
	mov r10, qword ptr[rbp - 0x128]
	mov qword ptr[rbp - 0x138], r10
	
	# [s22 + 0] = copy [s21 + 0]
	mov r10, qword ptr[rbp - 0x140]
	mov qword ptr[rbp - 0x150], r10
	mov r10, qword ptr[rbp - 0x138]
	mov qword ptr[rbp - 0x148], r10
	
	# %107 = &[s22 + 0]
	lea r10, [rbp - 0x150 + 0x0]
	mov qword ptr[rbp - 0x4d8], r10
	# call f15(%107)
	mov rdi, qword ptr[rbp - 0x4d8]
	call print
	
	# %108 = "through &str\\n"
	lea r10, [rip + .Lstr_5]
	mov qword ptr[rbp - 0x4e0], r10
	# [s23 + 0] = store %108
	mov r10, qword ptr[rbp - 0x4e0]
	mov qword ptr[rbp - 0x160], r10
	# %109 = 13
	mov qword ptr[rbp - 0x4e8], 13

	# [s23 + 8] = store %109
	mov r10, qword ptr[rbp - 0x4e8]
	mov qword ptr[rbp - 0x158], r10
	# [s24 + 0] = copy [s23 + 0]
	mov r10, qword ptr[rbp - 0x160]
	mov qword ptr[rbp - 0x170], r10
	mov r10, qword ptr[rbp - 0x158]
	mov qword ptr[rbp - 0x168], r10
	
	# %111 = &[s24 + 0]
	lea r10, [rbp - 0x170 + 0x0]
	mov qword ptr[rbp - 0x4f0], r10
	# %110 = move %111
	mov r10, qword ptr[rbp - 0x4f0]
	mov qword ptr[rbp - 0x4f8], r10
	# call f15(%110)
	mov rdi, qword ptr[rbp - 0x4f8]
	call print
	
	# [s25 + 0] = copy [*%110 + 0]
	mov r10, qword ptr[rbp - 0x4f8]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x180], r10
	mov r10, qword ptr[rbp - 0x4f8]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0x178], r10
	
	# %112 = &[s25 + 0]
	lea r10, [rbp - 0x180 + 0x0]
	mov qword ptr[rbp - 0x500], r10
	# call f15(%112)
	mov rdi, qword ptr[rbp - 0x500]
	call print
	
	# %113 = "sparrow"
	lea r10, [rip + .Lstr_6]
	mov qword ptr[rbp - 0x508], r10
	# [s26 + 0] = store %113
	mov r10, qword ptr[rbp - 0x508]
	mov qword ptr[rbp - 0x190], r10
	# %114 = 7
	mov qword ptr[rbp - 0x510], 7

	# [s26 + 8] = store %114
	mov r10, qword ptr[rbp - 0x510]
	mov qword ptr[rbp - 0x188], r10
	# [s27 + 0] = copy [s26 + 0]
	mov r10, qword ptr[rbp - 0x190]
	mov qword ptr[rbp - 0x1a0], r10
	mov r10, qword ptr[rbp - 0x188]
	mov qword ptr[rbp - 0x198], r10
	
	# %115 = 3
	mov word ptr[rbp - 0x512], 3

	# %116 = move %115
	mov r10w, word ptr[rbp - 0x512]
	mov word ptr[rbp - 0x514], r10w
	# [s28 + 0] = store %116
	mov r10w, word ptr[rbp - 0x514]
	mov word ptr[rbp - 0x1f0], r10w
	# %117 = 40
	mov qword ptr[rbp - 0x520], 40

	# %118 = move %117
	mov r10, qword ptr[rbp - 0x520]
	mov qword ptr[rbp - 0x528], r10
	# [s30 + 0] = store %118
	mov r10, qword ptr[rbp - 0x528]
	mov qword ptr[rbp - 0x200], r10
	# %119 = 2
	mov qword ptr[rbp - 0x530], 2

	# %120 = move %119
	mov r10, qword ptr[rbp - 0x530]
	mov qword ptr[rbp - 0x538], r10
	# [s30 + 8] = store %120
	mov r10, qword ptr[rbp - 0x538]
	mov qword ptr[rbp - 0x1f8], r10
	# [s28 + 16] = copy [s30 + 0]
	mov r10, qword ptr[rbp - 0x200]
	mov qword ptr[rbp - 0x1e0], r10
	mov r10, qword ptr[rbp - 0x1f8]
	mov qword ptr[rbp - 0x1d8], r10
	
	# [s28 + 32] = copy [s27 + 0]
	mov r10, qword ptr[rbp - 0x1a0]
	mov qword ptr[rbp - 0x1d0], r10
	mov r10, qword ptr[rbp - 0x198]
	mov qword ptr[rbp - 0x1c8], r10
	
	# [s33 + 0] = copy [s28 + 0]
	mov r10, qword ptr[rbp - 0x1f0]
	mov qword ptr[rbp - 0x230], r10
	mov r10, qword ptr[rbp - 0x1e8]
	mov qword ptr[rbp - 0x228], r10
	mov r10, qword ptr[rbp - 0x1e0]
	mov qword ptr[rbp - 0x220], r10
	mov r10, qword ptr[rbp - 0x1d8]
	mov qword ptr[rbp - 0x218], r10
	mov r10, qword ptr[rbp - 0x1d0]
	mov qword ptr[rbp - 0x210], r10
	mov r10, qword ptr[rbp - 0x1c8]
	mov qword ptr[rbp - 0x208], r10
	
	# %122 = load [s33 + 16]
	mov r10, qword ptr[rbp - 0x220]
	mov qword ptr[rbp - 0x540], r10
	# %123 = load [s33 + 24]
	mov r10, qword ptr[rbp - 0x218]
	mov qword ptr[rbp - 0x548], r10
	# %124 = %122 + %123
	mov r10, qword ptr[rbp - 0x540]
	mov r11, qword ptr[rbp - 0x548]
	add r10, r11
	mov qword ptr[rbp - 0x550], r10
	
	# %121 = move %124
	mov r10, qword ptr[rbp - 0x550]
	mov qword ptr[rbp - 0x558], r10
	# %125 = 13
	mov byte ptr[rbp - 0x559], 13

	# %126 = 42
	mov qword ptr[rbp - 0x568], 42

	# call f14(%125, %121, %126)
	mov dil, byte ptr[rbp - 0x559]
	mov rsi, qword ptr[rbp - 0x558]
	mov rdx, qword ptr[rbp - 0x568]
	call assert_eq
	
	# %127 = &[s33 + 16]
	lea r10, [rbp - 0x230 + 0x10]
	mov qword ptr[rbp - 0x570], r10
	# %130 = load [*%127 + 0]
	mov r10, qword ptr[rbp - 0x570]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x578], r10
	# %129 = move %130
	mov r10, qword ptr[rbp - 0x578]
	mov qword ptr[rbp - 0x580], r10
	# %131 = 14
	mov byte ptr[rbp - 0x581], 14

	# %132 = 40
	mov qword ptr[rbp - 0x590], 40

	# call f14(%131, %129, %132)
	mov dil, byte ptr[rbp - 0x581]
	mov rsi, qword ptr[rbp - 0x580]
	mov rdx, qword ptr[rbp - 0x590]
	call assert_eq
	
	# %133 = &[s33 + 16]
	lea r10, [rbp - 0x230 + 0x10]
	mov qword ptr[rbp - 0x598], r10
	# [s34 + 0] = copy [*%133 + 0]
	mov r10, qword ptr[rbp - 0x598]
	mov r10, [r10 + 0x0]
	mov qword ptr[rbp - 0x240], r10
	mov r10, qword ptr[rbp - 0x598]
	mov r10, [r10 + 0x8]
	mov qword ptr[rbp - 0x238], r10
	
	# %136 = load [s34 + 0]
	mov r10, qword ptr[rbp - 0x240]
	mov qword ptr[rbp - 0x5a0], r10
	# %137 = load [s34 + 8]
	mov r10, qword ptr[rbp - 0x238]
	mov qword ptr[rbp - 0x5a8], r10
	# %138 = %136 + %137
	mov r10, qword ptr[rbp - 0x5a0]
	mov r11, qword ptr[rbp - 0x5a8]
	add r10, r11
	mov qword ptr[rbp - 0x5b0], r10
	
	# %135 = move %138
	mov r10, qword ptr[rbp - 0x5b0]
	mov qword ptr[rbp - 0x5b8], r10
	# %139 = 15
	mov byte ptr[rbp - 0x5b9], 15

	# %140 = 42
	mov qword ptr[rbp - 0x5c8], 42

	# call f14(%139, %135, %140)
	mov dil, byte ptr[rbp - 0x5b9]
	mov rsi, qword ptr[rbp - 0x5b8]
	mov rdx, qword ptr[rbp - 0x5c8]
	call assert_eq
	
	# %142 = &[s33 + 0]
	lea r10, [rbp - 0x230 + 0x0]
	mov qword ptr[rbp - 0x5d0], r10
	# %141 = move %142
	mov r10, qword ptr[rbp - 0x5d0]
	mov qword ptr[rbp - 0x5d8], r10
	# %143 = call f10(%141)
	mov rdi, qword ptr[rbp - 0x5d8]
	call get_bird_age
	mov word ptr[rbp - 0x5da], ax
	
	# %145 = cast %143
	mov r10w, word ptr[rbp - 0x5da]
	mov r11, qword ptr[rbp - 0x5e8]
	movzx r11d, r10w
	mov qword ptr[rbp - 0x5e8], r11
	# %144 = 16
	mov byte ptr[rbp - 0x5e9], 16

	# %146 = 3
	mov qword ptr[rbp - 0x5f8], 3

	# call f14(%144, %145, %146)
	mov dil, byte ptr[rbp - 0x5e9]
	mov rsi, qword ptr[rbp - 0x5e8]
	mov rdx, qword ptr[rbp - 0x5f8]
	call assert_eq
	
	# %147 = call f11(%141)
	mov rdi, qword ptr[rbp - 0x5d8]
	call get_bird_position_sum
	mov qword ptr[rbp - 0x600], rax
	
	# %148 = 17
	mov byte ptr[rbp - 0x601], 17

	# %149 = 42
	mov qword ptr[rbp - 0x610], 42

	# call f14(%148, %147, %149)
	mov dil, byte ptr[rbp - 0x601]
	mov rsi, qword ptr[rbp - 0x600]
	mov rdx, qword ptr[rbp - 0x610]
	call assert_eq
	
	# call f12(%141)
	mov rdi, qword ptr[rbp - 0x5d8]
	call print_bird_name
	
	# %150 = "\\n"
	lea r10, [rip + .Lstr_7]
	mov qword ptr[rbp - 0x618], r10
	# [s35 + 0] = store %150
	mov r10, qword ptr[rbp - 0x618]
	mov qword ptr[rbp - 0x250], r10
	# %151 = 1
	mov qword ptr[rbp - 0x620], 1

	# [s35 + 8] = store %151
	mov r10, qword ptr[rbp - 0x620]
	mov qword ptr[rbp - 0x248], r10
	# [s36 + 0] = copy [s35 + 0]
	mov r10, qword ptr[rbp - 0x250]
	mov qword ptr[rbp - 0x260], r10
	mov r10, qword ptr[rbp - 0x248]
	mov qword ptr[rbp - 0x258], r10
	
	# %152 = &[s36 + 0]
	lea r10, [rbp - 0x260 + 0x0]
	mov qword ptr[rbp - 0x628], r10
	# call f15(%152)
	mov rdi, qword ptr[rbp - 0x628]
	call print
	
	# %154 = 2
	mov qword ptr[rbp - 0x630], 2

	# %155 = 3
	mov qword ptr[rbp - 0x638], 3

	# %156 = call f5(%154, %155)
	mov rdi, qword ptr[rbp - 0x630]
	mov rsi, qword ptr[rbp - 0x638]
	call add
	mov qword ptr[rbp - 0x640], rax
	
	# %157 = 4
	mov qword ptr[rbp - 0x648], 4

	# %158 = 5
	mov qword ptr[rbp - 0x650], 5

	# %159 = call f5(%157, %158)
	mov rdi, qword ptr[rbp - 0x648]
	mov rsi, qword ptr[rbp - 0x650]
	call add
	mov qword ptr[rbp - 0x658], rax
	
	# %153 = call f6(%156, %159)
	mov rdi, qword ptr[rbp - 0x640]
	mov rsi, qword ptr[rbp - 0x658]
	call multiply
	mov qword ptr[rbp - 0x660], rax
	
	# %160 = 18
	mov byte ptr[rbp - 0x661], 18

	# %161 = 45
	mov qword ptr[rbp - 0x670], 45

	# call f14(%160, %153, %161)
	mov dil, byte ptr[rbp - 0x661]
	mov rsi, qword ptr[rbp - 0x660]
	mov rdx, qword ptr[rbp - 0x670]
	call assert_eq
	
	# %163 = 7
	mov qword ptr[rbp - 0x678], 7

	# %164 = 8
	mov qword ptr[rbp - 0x680], 8

	# %166 = %163 < %164
	mov r10, qword ptr[rbp - 0x678]
	mov r11, qword ptr[rbp - 0x680]
	cmp r10, r11
	setb byte ptr[rbp - 0x681]

	# Branch
	cmp byte ptr[rbp - 0x681], 0
	je .Lmain.if_else.11
	jmp .Lmain.if_then.10

.Lmain.if_then.10:
	# %167 = 5
	mov qword ptr[rbp - 0x690], 5

	# %168 = %163 + %164
	mov r10, qword ptr[rbp - 0x678]
	mov r11, qword ptr[rbp - 0x680]
	add r10, r11
	mov qword ptr[rbp - 0x698], r10
	
	# %169 = %168 + %167
	mov r10, qword ptr[rbp - 0x698]
	mov r11, qword ptr[rbp - 0x690]
	add r10, r11
	mov qword ptr[rbp - 0x6a0], r10
	
	# %165 = move %169
	mov r10, qword ptr[rbp - 0x6a0]
	mov qword ptr[rbp - 0x6a8], r10
	# Jump
	jmp .Lmain.if_merge.12

.Lmain.if_else.11:
	# %165 = 0
	mov qword ptr[rbp - 0x6a8], 0

	# Jump
	jmp .Lmain.if_merge.12

.Lmain.if_merge.12:
	# %162 = move %165
	mov r10, qword ptr[rbp - 0x6a8]
	mov qword ptr[rbp - 0x6b0], r10
	# %170 = 19
	mov byte ptr[rbp - 0x6b1], 19

	# %171 = 20
	mov qword ptr[rbp - 0x6c0], 20

	# call f14(%170, %162, %171)
	mov dil, byte ptr[rbp - 0x6b1]
	mov rsi, qword ptr[rbp - 0x6b0]
	mov rdx, qword ptr[rbp - 0x6c0]
	call assert_eq
	
	# %173 = %35 + %40
	mov r10, qword ptr[rbp - 0x2c0]
	mov r11, qword ptr[rbp - 0x2f8]
	add r10, r11
	mov qword ptr[rbp - 0x6c8], r10
	
	# %174 = %173 + %50
	mov r10, qword ptr[rbp - 0x6c8]
	mov r11, qword ptr[rbp - 0x330]
	add r10, r11
	mov qword ptr[rbp - 0x6d0], r10
	
	# %175 = %174 + %54
	mov r10, qword ptr[rbp - 0x6d0]
	mov r11, qword ptr[rbp - 0x350]
	add r10, r11
	mov qword ptr[rbp - 0x6d8], r10
	
	# %176 = %175 + %69
	mov r10, qword ptr[rbp - 0x6d8]
	mov r11, qword ptr[rbp - 0x3d0]
	add r10, r11
	mov qword ptr[rbp - 0x6e0], r10
	
	# %177 = %176 + %77
	mov r10, qword ptr[rbp - 0x6e0]
	mov r11, qword ptr[rbp - 0x408]
	add r10, r11
	mov qword ptr[rbp - 0x6e8], r10
	
	# %178 = %177 + %85
	mov r10, qword ptr[rbp - 0x6e8]
	mov r11, qword ptr[rbp - 0x430]
	add r10, r11
	mov qword ptr[rbp - 0x6f0], r10
	
	# %179 = %178 + %89
	mov r10, qword ptr[rbp - 0x6f0]
	mov r11, qword ptr[rbp - 0x450]
	add r10, r11
	mov qword ptr[rbp - 0x6f8], r10
	
	# %180 = %179 + %121
	mov r10, qword ptr[rbp - 0x6f8]
	mov r11, qword ptr[rbp - 0x558]
	add r10, r11
	mov qword ptr[rbp - 0x700], r10
	
	# %181 = %180 + %129
	mov r10, qword ptr[rbp - 0x700]
	mov r11, qword ptr[rbp - 0x580]
	add r10, r11
	mov qword ptr[rbp - 0x708], r10
	
	# %182 = %181 + %135
	mov r10, qword ptr[rbp - 0x708]
	mov r11, qword ptr[rbp - 0x5b8]
	add r10, r11
	mov qword ptr[rbp - 0x710], r10
	
	# %183 = %182 + %147
	mov r10, qword ptr[rbp - 0x710]
	mov r11, qword ptr[rbp - 0x600]
	add r10, r11
	mov qword ptr[rbp - 0x718], r10
	
	# %184 = %183 + %153
	mov r10, qword ptr[rbp - 0x718]
	mov r11, qword ptr[rbp - 0x660]
	add r10, r11
	mov qword ptr[rbp - 0x720], r10
	
	# %185 = %184 + %162
	mov r10, qword ptr[rbp - 0x720]
	mov r11, qword ptr[rbp - 0x6b0]
	add r10, r11
	mov qword ptr[rbp - 0x728], r10
	
	# %172 = move %185
	mov r10, qword ptr[rbp - 0x728]
	mov qword ptr[rbp - 0x730], r10
	# %186 = 20
	mov byte ptr[rbp - 0x731], 20

	# %187 = 1355
	mov qword ptr[rbp - 0x740], 1355

	# call f14(%186, %172, %187)
	mov dil, byte ptr[rbp - 0x731]
	mov rsi, qword ptr[rbp - 0x730]
	mov rdx, qword ptr[rbp - 0x740]
	call assert_eq
	
	# %188 = 0
	mov qword ptr[rbp - 0x748], 0

	# call f1(%188)
	mov rdi, qword ptr[rbp - 0x748]
	call crust_exit
	
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
	mov rdi, r9
	
	# Return
	mov rax, rdi
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
	mov rdi, r9
	
	# Return
	mov rax, rdi
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
	mov rdi, r9
	call print
	
	# Return
	mov rsp, rbp
	pop rbp
	ret

.type print_u64, @function
print_u64:
.Lprint_u64.entry.0:
	push rbp
	mov rbp, rsp
	sub rsp, 0x70

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
	sete dil

	# Branch
	cmp dil, 0
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
	mov rdi, r9
	call crust_write
	mov r8, rax
	
	# Return
	mov rsp, rbp
	pop rbp
	ret

.Lprint_u64.if_merge.3:
	# %13 = load [s5 + 0]
	mov r9, qword ptr[rbp - 0x58]
	# %14 = 0
	mov rdi, 0

	# %15 = %13 > %14
	cmp r9, rdi
	seta sil

	# Branch
	cmp sil, 0
	je .Lprint_u64.while_merge.4
	jmp .Lprint_u64.while_loop.3

.Lprint_u64.while_loop.3:
	# %17 = load [s5 + 0]
	mov r9, qword ptr[rbp - 0x58]
	# %18 = 10
	mov rdi, 10

	# %19 = %17 % %18
	push rax
	push rdx
	xor rdx, rdx
	mov rax, r9
	div rdi
	mov esi, edx
	pop rdx
	pop rax
	
	# %20 = cast %19
	movzx r9d, sil
	# %16 = move %20
	mov dil, r9b
	# %21 = load [s3 + 0]
	mov r9, qword ptr[rbp - 0x48]
	# %22 = 1
	mov rsi, 1

	# %23 = %21 - %22
	sub r9, rsi
	mov rdx, r9
	
	# [s3 + 0] = store %23
	mov qword ptr[rbp - 0x48], rdx
	# %24 = 48
	mov r9b, 48

	# %25 = %16 + %24
	add dil, r9b
	mov sil, dil
	
	# %26 = load [s3 + 0]
	mov r9, qword ptr[rbp - 0x48]
	# [[s1 + 0] + %26] = store %25
	lea r11, [rbp - 0x38]
	mov [r11 + r9 * 1], sil
	# %27 = load [s5 + 0]
	mov rsi, qword ptr[rbp - 0x58]
	# %28 = 10
	mov rdx, 10

	# %29 = %27 / %28
	push rax
	push rdx
	mov r11, rdx
	xor rdx, rdx
	mov rax, rsi
	div r11
	mov ecx, eax
	pop rdx
	pop rax
	
	# [s5 + 0] = store %29
	mov qword ptr[rbp - 0x58], rcx
	# %30 = load [s5 + 0]
	mov rsi, qword ptr[rbp - 0x58]
	# %31 = 0
	mov rdx, 0

	# %32 = %30 > %31
	cmp rsi, rdx
	seta cl

	# Branch
	cmp cl, 0
	je .Lprint_u64.while_merge.4
	jmp .Lprint_u64.while_loop.3

.Lprint_u64.while_merge.4:
	# %33 = load [s3 + 0]
	mov rsi, qword ptr[rbp - 0x48]
	# %34 = 20
	mov rdx, 20

	# %35 = %33 < %34
	cmp rsi, rdx
	setb cl

	# Branch
	cmp cl, 0
	je .Lprint_u64.while_merge.6
	jmp .Lprint_u64.while_loop.5

.Lprint_u64.while_loop.5:
	# %36 = load [s3 + 0]
	mov rsi, qword ptr[rbp - 0x48]
	# %37 = &[[s1 + 0] + %36]
	lea r11, [rbp - 0x38]
	lea rdx, [r11 + rsi * 1]
	# %38 = 1
	mov rcx, 1

	# %39 = call f0(%37, %38)
	mov rdi, rdx
	mov rsi, rcx
	call crust_write
	mov rax, rax
	
	# %40 = load [s3 + 0]
	mov rdx, qword ptr[rbp - 0x48]
	# %41 = 1
	mov rcx, 1

	# %42 = %40 + %41
	add rdx, rcx
	mov qword ptr[rbp - 0x68], rdx
	
	# [s3 + 0] = store %42
	mov r10, qword ptr[rbp - 0x68]
	mov qword ptr[rbp - 0x48], r10
	# %43 = load [s3 + 0]
	mov rdx, qword ptr[rbp - 0x48]
	# %44 = 20
	mov rcx, 20

	# %45 = %43 < %44
	cmp rdx, rcx
	setb byte ptr[rbp - 0x69]

	# Branch
	cmp byte ptr[rbp - 0x69], 0
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
	call print
	
	# %17 = cast %0
	mov r10b, byte ptr[rbp - 0xc1]
	movzx r9d, r10b
	# call f13(%17)
	mov rdi, r9
	call print_u64
	
	# %18 = &[s3 + 0]
	lea r9, [rbp - 0x40 + 0x0]
	# call f15(%18)
	mov rdi, r9
	call print
	
	# call f13(%1)
	mov rdi, qword ptr[rbp - 0xd0]
	call print_u64
	
	# %19 = &[s7 + 0]
	lea r9, [rbp - 0x80 + 0x0]
	# call f15(%19)
	mov rdi, r9
	call print
	
	# call f13(%2)
	mov rdi, qword ptr[rbp - 0xd8]
	call print_u64
	
	# %20 = &[s11 + 0]
	lea r9, [rbp - 0xc0 + 0x0]
	# call f15(%20)
	mov rdi, r9
	call print
	
	# Jump
	jmp .Lassert_eq.if_merge.3

.Lassert_eq.if_else.2:
	# %21 = &[s1 + 0]
	lea r9, [rbp - 0x20 + 0x0]
	# call f15(%21)
	mov rdi, r9
	call print
	
	# %22 = cast %0
	mov r10b, byte ptr[rbp - 0xc1]
	movzx r9d, r10b
	# call f13(%22)
	mov rdi, r9
	call print_u64
	
	# %23 = &[s5 + 0]
	lea r9, [rbp - 0x60 + 0x0]
	# call f15(%23)
	mov rdi, r9
	call print
	
	# call f13(%1)
	mov rdi, qword ptr[rbp - 0xd0]
	call print_u64
	
	# %24 = &[s9 + 0]
	lea r9, [rbp - 0xa0 + 0x0]
	# call f15(%24)
	mov rdi, r9
	call print
	
	# call f13(%2)
	mov rdi, qword ptr[rbp - 0xd8]
	call print_u64
	
	# %25 = &[s11 + 0]
	lea r9, [rbp - 0xc0 + 0x0]
	# call f15(%25)
	mov rdi, r9
	call print
	
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
	mov rdi, r9
	call crust_write
	mov r8, rax
	
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
