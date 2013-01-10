
.globl prepareCollection

prepareCollection:
	push %rbp
	mov %rsp, %rbp
	sub sizeof_RootFinder, %rsp

	# get callee-saved regs
	mov %rbx, 0x0(%rsp)
	mov %r12, 0x8(%rsp)
	mov %r13, 0x10(%rsp)
	mov %r14, 0x18(%rsp)
	mov %r15, 0x20(%rsp)

	# get frame pointer
	mov sizeof_RootFinder(%rsp), %rax
	mov %rax, 0x28(%rsp)

	# get return address
	mov sizeof_RootFinder+8(%rsp), %rax
	mov %rax, 0x30(%rsp)

	# Prepare for args and call into the real gc function
	mov %rdi, %rax
	mov %rsp, %rdi
	call *%rax
	
	mov %rbp, %rsp
	pop %rbp
	ret

