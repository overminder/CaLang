
/* Stack layouts
   -------------
   
   ### cafunc calls cafunc:

   The design goal is to support proper tailcall and gdb.
   We save the frame pointer at [&ret_addr - 8] because in this way
   the gdb can walk the stack easily.
   We save callee-saved registers far away because... Well, no reason :P

   Just after entering a cafunc:

   [arg1..argN, empty slot, ret addr, prev func's locals]
                            ^ sp                    ^ fp

   Standard prologue:
     push %rbp
     mov %rsp, %rbp
     mov %rbx, someOffset(%rbp)  ;; Save callee-saved reg on stack
     mov %r12, someOffset2(%rbp) ;; Another callee-saved reg
     sub numLocals, %rsp         ;; Allocate local stack space

   And the stack becomes:

   [ saved r12, saved rbx, uninitialized func locals, arg7..argN
     ^ sp
   , prev frame ptr, ret addr, prev func's locals]
     ^ fp

   Prepare call to cafunc:
     mov %rdi, someOffset3(%rbp) ;; Save caller-saved reg on stack
     ...
     mov var_1(%rbp), %rdi       ;; Pass arg by reg
     mov var_2(%rbp), %rsi       ;; Another reg arg
     ...
     mov var_8(%rbp), %rax       ;; Pass arg by memory. Note that we reserved
                                 ;; -0x8(%rsp) and -0x10(%rsp) for the
                                 ;; return address pushed
                                 ;; by x86 `call' instruction.
     mov %rax, -0x18(%rsp)
     mov var_7(%rbp), %rax
     mov %rax, -0x20(%rsp)

   At this point, the stack looks like:
   [arg7..argN, empty slot, empty slot2, prev func's locals]
                                         ^ sp          ^ fp


   Return from cafunc:
     mov %rax, var_retVal(%rbp)  ;; Store return value
     mov someOffset3(%rbp), %rdi ;; Restore caller-saved reg

   Standard epilogue:
     mov %rbp, %rsp              ;; restore stack ptr
     mov someOffset2(%rbp), %r12 ;; restore callee-saved reg
     mov someOffset(%rbp), %rbx  ;; restore another
     pop %rbp
     ret

   For tailcall, args passed on stack and callee-saved regs need to be
   xchg'd when there are conflicts. This can be done in O(n) time and O(1)
   space.

   #### cafunc calls "C" func

   Just entered "C" func:

   [ret addr, arg7..argN, prev func's locals]
    ^ sp                  ^ fp

   And after standard prologue:

   [reserved space for args, locals..., prev frame ptr,
    ^ sp                                ^ fp
    ret addr, arg7..argN, prev func's locals]

   As we define the collector function to be an extern "C" func,
   it can retrive saved frame ptr and saved return address
   by i64[fp] and i64[fp + 8].

   Using the return address we can lookup the corresponding GcMap
   in the gcmap table.

 */

typedef struct {
    long prologueSavedRegs[5];  /* Offset of callee-save regs saved
                                   in the prologue of this function,
                                   or 0 if not saved */
    long ptrContainingRegs;     /* Bitmap 0bXXX????? of callee-save
                                   regs used by the callee and
                                   escaped out of this callsite */
    long framePtrOffset;
    long numPtrs;
    long ptrOffsets[];
} GcMap;

typedef struct {
    void *label;
    GcMap gcmap;
} LabeledGcMap;

