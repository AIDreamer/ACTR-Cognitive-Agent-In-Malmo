This directory contains files which provide functions for performing
computations in parallel using multiple threads in Lisps for which
the ACT-R uni-files code contains thread support (any Lisp which
can use the ACT-R Environment has that capability).  

The parallel-computation.lisp file provides general "mapcar like"
parallel execution functions that may be used and parallel-chunks.lisp
uses the functions from parallel-computation.lisp to replace some
of the internal ACT-R chunk operations (computing activations and
finding matching chunks) with parallel versions.

For details on how to use those capabilities see the comments in
the files themselves.  In general, this probably won't improve 
performance for most models because of the overhead involved (none
of the tutorial tasks showed any improvement), but for models
which deal with very large sets of chunks it might be of some
benefit.

The parallel-model-running.lisp file provides the option of running
the same model (and experiment code) in separate instances of ACT-R
within the same Lisp instance in separate threads.  This is more
likely to improve performance because it doesn't require "big" models,
but does use a lot of memory which could be an issue for some Lisps.



