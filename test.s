	.text
	.file	"GMAIL"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$8, %edi
	callq	malloc@PLT
	movq	$0, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	main@GOTPCREL(%rip), %rcx
	movq	%rcx, (%rax)
	movq	$0, 8(%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	xorl	%edi, %edi
	callq	malloc@PLT
	movq	f0@GOTPCREL(%rip), %rcx
	movq	%rax, 8(%rbx)
	movq	%rcx, (%rbx)
	movl	$16, %edi
	callq	malloc@PLT
	movq	(%rbx), %rcx
	movq	8(%rbx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rcx, (%rax)
	movq	8(%rax), %rdi
	movl	$3, %esi
	movl	$4, %edx
	callq	*(%rax)
	movl	%eax, %ecx
	leaq	.Lfmt(%rip), %rdi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf@PLT
	xorl	%eax, %eax
	popq	%rbx
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	f0                      # -- Begin function f0
	.p2align	4, 0x90
	.type	f0,@function
f0:                                     # @f0
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%rbx
	.cfi_def_cfa_offset 40
	pushq	%rax
	.cfi_def_cfa_offset 48
	.cfi_offset %rbx, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movl	%edx, %r14d
	movl	%esi, %ebp
	movq	%rdi, %r15
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %rbx
	movl	%ebp, (%rbx)
	movl	$4, %edi
	callq	malloc@PLT
	movq	%rax, %rbp
	movl	%r14d, (%rbp)
	movl	$8, %edi
	callq	malloc@PLT
	movq	%r15, (%rax)
	movl	$16, %edi
	callq	malloc@PLT
	movq	f0@GOTPCREL(%rip), %rcx
	movq	%r15, 8(%rax)
	movq	%rcx, (%rax)
	movl	(%rbx), %eax
	addl	(%rbp), %eax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Lfunc_end1:
	.size	f0, .Lfunc_end1-f0
	.cfi_endproc
                                        # -- End function
	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%s\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%g\n"
	.size	.Lfmt.2, 4

	.type	.Lfmt.3,@object         # @fmt.3
.Lfmt.3:
	.asciz	"%d\n"
	.size	.Lfmt.3, 4

	.type	.Lfmt.4,@object         # @fmt.4
.Lfmt.4:
	.asciz	"%s\n"
	.size	.Lfmt.4, 4

	.type	.Lfmt.5,@object         # @fmt.5
.Lfmt.5:
	.asciz	"%g\n"
	.size	.Lfmt.5, 4


	.section	".note.GNU-stack","",@progbits
