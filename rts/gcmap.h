
/* Stack layout when a cafunc is just entered:

   [arg1..argN, empty slot, ret addr, prev func's locals]
                            ^ sp                    ^ fp
   Standard prologue (such that we can use gdb to walk the stack):
     push %rbp
     mov %rsp, %rbp
     # we use direct move instead of push
     mov %rbx, someOffset(%rbp)
     mov %r12, someOffset2(%rbp)

   And the stack becomes:

   [ saved r12, saved rbx, uninitialized func locals, arg7..argN
   , prev frame ptr, ret addr, prev func's locals]
     ^ sp/fp

   Standard epilogue:
     mov %rbp, %rsp
     pop %rbp
     ret

   The other case is that when a cafunc calls an extern "C" func:

   [ret addr, arg7..argN, prev func's locals]
    ^ sp

   And after standard prologue:

   [prev frame ptr, ret addr, arg7..argN, prev func's locals]
    ^ sp/fp

   As we define the collector function to be an extern "C" func,
   it can retrive saved frame ptr and saved return address
   by i64[sp] and i64[sp + 8].

   Using the return address we can lookup the corresponding GcMap
   in the gcmap table.

 */

typedef struct {
    long prologueSavedRegs[5];  /* Offset of callee-save regs saved
                                   in the prologue of this function,
                                   or 0 if not saved */
    uint8_t ptrContainingRegs;  /* Bitmap 0bXXX????? of callee-save
                                   regs living across this callsite */
    long numPtrs;
    long ptrOffsets[];
} GcMap;

typedef struct {
    void *label;
    GcMap gcmap;
} LabeledGcMap;

