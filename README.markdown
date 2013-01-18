CaLang
======

An C-alternative language.

Mobai Lunba
-----------

### Description
Inspired by SPJ's C-- I started to think about writing something
similar.

### Feature
- A working x86\_64 code generator
  * Uses GAS for assembling and linking.

- Has runtime support for GC root stack scanning.
  * Implemented by closely following Andrew W. Appel's Modern Compiler
    Implementation in ML (AKA the tiger book).
  * That is, it has zero runtime cost when gc is not triggered.

- Supports TCO
  * Even when there are too many arguments. E.g., CaLang has its own
    (not compatible with C when passing argument by stack) calling convention.

- C interoperability
  * In SysV, function that has no greater than 6 arguments is compatible
    with C.

### Feature that should be implemented in the future
- Should apply some optimizations.
  * Welp..

- Should support runtime interface for debugging
  * GDB/DWARF?

- Should support more platforms
  * Adding a code generator for MIPS should be pretty straightforward. In
    this way, students studying COMP180 can have a better time...

