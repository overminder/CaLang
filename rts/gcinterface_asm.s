
.globl CaLang_prepareCollection

# struct CalleeRegs {
#     long regVal[5];
# }

# struct RootFinder {
#     long **regLocs[5];
#     void **rbp;
# }

# intptr_t CaLang_prepareCollection(GcFunction gcFunc, void *userData)
# where typedef intptr_t (*GcFunction) (RootFinder *, void *userData)

CaLang_prepareCollection:
	push %rbp
	mov %rsp, %rbp

	# Make place for struct CalleeRegs (temporary saved by this function)
	sub $0x28, %rsp

	# get callee-saved regs and put them into a temporary place
	mov %rbx, 0x0(%rsp)
	mov %r12, 0x8(%rsp)
	mov %r13, 0x10(%rsp)
	mov %r14, 0x18(%rsp)
	mov %r15, 0x20(%rsp)

	# Make place for struct RootFinder
	mov %rsp, %rbx
	sub $0x30, %rsp

	# move callee-saved register address to RootFinder
	mov %rbx, 0x0(%rsp)
	lea 0x8(%rbx), %rax
	mov %rax, 0x8(%rsp)
	lea 0x10(%rbx), %rax
	mov %rax, 0x10(%rsp)
	lea 0x18(%rbx), %rax
	mov %rax, 0x18(%rsp)
	lea 0x20(%rbx), %rax
	mov %rax, 0x20(%rsp)

	# move frame pointer
	mov %rbp, 0x28(%rsp)

	# Prepare for args and call into the real gc function
	mov %rdi, %rax
	mov %rsp, %rdi
	call *%rax

	# Restore (probably modified) callee-saved registers
	add $0x30, %rsp
	mov 0x0(%rsp) ,%rbx 
	mov 0x8(%rsp) ,%r12 
	mov 0x10(%rsp),%r13 
	mov 0x18(%rsp),%r14 
	mov 0x20(%rsp),%r15 
	
	mov %rbp, %rsp
	pop %rbp
	ret

