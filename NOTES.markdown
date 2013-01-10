
诶嘿嘿嘿嘿
----------

### Runtime Interface for GC

根据Appel的Modern Compiler Impl in ML里所述，GC的runtime interface可以这么实现：

- 在每个call site的接下来的地方放一个label
- 对于每个label，生成一个gc map，描述对于这个return address以及stack或者frame
pointer而言，有哪些地方有live的pointer，以及前一个stack或者frame pointer在哪，
有哪几个pointer在callee-saved register里面，我作为一个callee，save了哪些
register，等等等等。。
- 这样，当需要collect的时候，collector就可以通过找stack上存着的return address
来知道当前的register里和stack上有哪些是pointer了。

### Calling Convention

System V x86\_64的spec（下称sysV）里说，1st-6th的int arg通过reg来传递，
不知道多少float arg通过xmm来传递，其他的通过stack来传递，可是通过stack来
传递的arg居然属于caller！另外，return address是最后push上去的。
这样一来，如果想要实现proper tailcall，那么只要caller和callee的signature不同，
我们都必须让caller在jump之前调整一下return address。

如果我们自己来一种calling convention的话，也许可以更方便一些。比如我们可以让
argument属于callee，return address属于caller，这样就不用调整return addr了。

C--也是有一套自己的calling convention...

_See rts/gcmap.h for further discussions_

### Dataflow analysis: fixpoint?

apply optimization的时候，最好能够保证optimize之后的code依然能满足各种dataflow
invariant。这样的好处是optimization的时候不用每走一次就重新analyze一次。
那么这个怎么做到呢(或者说，能做到吗)？

我们从最简单的block-level开始。假设我们只有movi, add和ret这3个指令.
首先是做value numbering，然后做一系列的analysis。
fwd analysis有avail expr(特点是由于VN了，所以所有的AE都是immutable的),
bwd有liveness(这个就比较单疼：DCE掉某个instr之后，需要把use从这个instr的in开始
一直kill到前一个use了的instr的out上。

。。。好像很麻烦，而且这已经是非常简单的情况了。看来这个不太可能吧
