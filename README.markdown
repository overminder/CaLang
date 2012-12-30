CaLang
======

An C-alternative language.

Mobai Lunba
-----------

### Description
Inspired by SPJ's C-- I started to think about writing something
similar.

### Feature
- Should have runtime interface for GC root stack scanning.
  * Maybe just allocate all the *gcptrs* in caller-saved registers?
- Should support TCO
  * Even when there are too many arguments.
  * The caller should not be a vararg function.
- Should apply some optimizations.
  * SSA
- Should support runtime interface for debugging (_how?_)
  * GDB/DWARF?

