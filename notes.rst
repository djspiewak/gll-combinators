====================================
Implementation Notes and Experiences
====================================

The original GLL algorithm is quite dependent upon an unrestricted ``goto``
statement.  In fact, the form of ``goto`` required by GLL is even unavailable in
C, forcing the original authors to implement a workaround of their own by using
a "big ``switch``" within the ``L0`` branch.  Obviously, this algorithm is not
immediately ammenable to implementation in a functional language, much less a
cleanly-separated implementation using combinators.

The critical observation which allows ``goto``-less implementation of the algorithm
is in regards to the nature of the ``L0`` branch.  Upon close examination of the
algorithm, it becomes apparent that ``L0`` can be viewed as a *trampoline*, a
concept which is quite common in functional programming as a way of implementing
stackless mutual tail-recursion.  In the case of GLL, this trampoline function
must not only dispatch the various alternate productions (also represented as
functions) but also have some knowledge of the GSS and the dispatch queue itself.
In short, ``L0`` is a trampoline function with some additional smarts to deal
with divergent and convergent branches.

Once this observation is made, the rest of the implementation just falls into
place.  Continuations (wrapped up in anonymous functions) can be used to satisfy
the functionality of an unrestricted ``goto``, assuming a trampoline function
as described above.  Surprisingly, this scheme divides itself quite cleanly into
combinator-like constructs, further reinforcing the claim that GLL is just another
incarnation of recursive-descent.


Bumps in the Road
=================

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
In order to solve this problem, we need to define ``apply(...)`` for ``NonTerminalParser``
so that any ``Success`` with a ``tail != Stream()`` becomes a ``Failure("Expected end of stream", tail)``.

Parser equality is a very serious issue.  Consider the following parser
declaration::
    
    def p: Parser[Any] = p | "a"
    
While it would be nice to say that ``p == p'``, where ``p'`` is the "inner ``p``",
the recursive case.  Unfortunately, these are actually two distinct instance of
``DisjunctiveParser``.  This means that we cannot simply check equality to avoid
infinite recursion.

To solve this, we need to get direct access to the ``p`` thunk and check its
*class* rather than its *instance*.  To do this, we will use Java reflection to
access the field value without allowing the Scala compiler to transparently
invoke the thunk.  Once we have this value, we can invoke ``getClass`` and quickly
perform the comparison.  The only problem with this solution is it forces all of
the thunk-uses to be logical constants.  Thus, we cannot define a parser in the
following way::
    
    def p = make() | make()
    
    def make() = literal(Math.random.toString)
    
The ``DisjunctiveParser`` contained by ``p`` will consider both the left ``make()``
and the right ``make()`` to be exactly identical.  Fortunately, we can safely
assume that grammars are constructed in a declarative fashion.  The downside is
when people *do* try something like this, the result will be fairly bizzare from
a user's standpoint.

Another interesting issue is one which arises in conjunction with left-recursion.
Consider the following grammar::
    
    def p: Parser[Any] = p ~ "a" | "a"

This grammar is quite unambiguous (so long as the parse is greedy), but it will
still lead to non-terminating execution for an input of ``Stream('a')``.  This is
because the parser will handle the single character using the second production
while simultaneously queueing up the first production rule against the untouched
stream (``Stream('a')``).  This rule will in turn queue up two more parsers: the
first and second rules again.  The second rule will immediately match, produce a
duplicate result and be discarded.  However, the *first* rule will behave exactly
as it did before, queueing up two more parsers without consuming any of the stream.
Needless to say, this is a slight issue.

The solution here is that the second queueing of the first rule must lead to a
memoization of the relevant parse.  The second pass over the second rule should
return that result through the second queueing, saving that result in ``popped``
and avoiding the divergence.  Thus, left-recursive rules will go *one* extra
queueing, but this extra step will be pruned as the successful parse will avoid
any additional repetition.  Unfortunately, this solution is made more difficult
to implement due to the fact that disjunctive parsers are never themselves pushed
onto the dispatch queue.  ``Trampoline`` does not know of any connection between
the first and second productions of a disjunction.  It only knows that the two
separate productions have been pushed.

To solve this problem in a practical way, we need to introduce another ``Parser``
subtype: ``ThunkParser``.  This parser just delegates everything to its wrapper
parser with the exception of ``queue``, which it leaves abstract.  This parser
is instantiated using an anonymous inner-class within ``DisjunctiveParser`` to
handle the details of queueing up the separate productions without "losing" the
disjunction itself.

Another problem encountered while attempting to implement the trampoline is that
Scala's ``Stream`` implementation isn't quite what one would expect.  In particular,
equality is defined on a reference basis, rather than logical value.  Thus,
two streams which have the same contents may not necessarily be equivalent according
to ``equals(...)``.  This isn't normally an issue, but it does cause problems
with the ``Seq#toStream`` method::
    
    "".toStream == "".toStream        // => false!!
    
For non-left-recursive grammars, this will lead to duplicate results from the
parse.  However, for left-recursive grammars, this could actually lead to
divergence.  This isn't really a problem with GLL or the combinator implementation.
Rather, it is an issue with the Scala ``Stream`` implementation.  To avoid this,
we must ensure that all input streams are created using ``Stream()``, ``Stream.cons``
and ``Stream.empty``.
