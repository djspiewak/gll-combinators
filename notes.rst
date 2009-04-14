====================================
Implementation Notes and Experiences
====================================

Computation of true PREDICT sets is impossible because no ``Parser`` instance
actually knows what its successor is.  Thus, we cannot compute FOLLOW sets
without "stepping out" into the parent parser.  To avoid this, we say that
whenever FIRST(a) = { }, PREDICT(a) = \Sigma.  Less-formally, if a parser goes
to \epsilon, then its (uncomputed) PREDICT set is satisfied by *any* input.

Our GSS seems to be somewhat less effective than that of GLL due to the fact that
parallel sequential parsers with shared suffixes do not actually share state.
Thus, we could easily get the following situation in our GSS::
    
           C -- D -- F
          /
    A -- B
          \
           E -- D -- F
           
Notice that the ``D -- F`` suffix is shared, but because it is in separate parsers,
it will not be merged.  Note however that if these two branches *reduce* to the
same value, that result will be merged.  Alternatively, these branches may reduce
to differing values but eventually go to the same parser.  When this happens, it
will be considered as a common prefix and merged accordingly (*not sure of this is sound sound*).
  
Greedy vs lazy matching seems to be a problem.  Consider the following grammar::
    
    A ::= 'a' A
        |
    
This grammar is actually quite ambiguous.  The input string "``aaa``" may parse
as ``Success("", Stream('a', 'a', 'a'))``, ``Success("a", Stream('a', 'a'))``,
``Success("aa", Stream('a'))`` or ``Success("aaa", Stream())``.  Obviously, this
is a problem.  Or rather, this is a problem if we want to maintain PEG semantics.
