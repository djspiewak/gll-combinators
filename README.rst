======
README
======

**GLL Combinators** is a framework designed to implement the `GLL parsing algorithm`_
(Scott and Johnstone, LDTA 2009) in a functional manner.  More specifically, the
framework makes use of atomic parser combinators to compose grammars which are
then evaluated using the GLL algorithm.  The framework provides a syntax for this
task which is almost identical to that of the parser combinator framework built
into Scala.  For example, we can render the classic "parentheses grammar" using
GLL combinators::
    
    lazy val expr: Parser[Any] = (
        "(" ~ expr ~ ")"
      | ""
    )

As the type annotation implies, ``expr`` will reference an instance of type
``Parser[Any]``.  That is, an atomic parser which consumes some input and returns
a value of type ``Any``.  We can invoke this parser against a ``String`` input
in the following way::
    
    expr("((()))")
    
This will return a value of type ``Stream[Result[Any]]``.  The ``Result[A]`` ADT is
defined as one of the following (for some type ``A``):

* ``Success[A]`` -- Represents a successful parse and contains the resulting value.
* ``Failure`` -- Represents a failed parse and contains the relevant error message
  as well as the remainder of the parse stream (the characters which were not
  consumed).

If any result is successful (i.e. an instance of ``Success[A]``), then no failures
will be returned.  Thus, the ``Stream`` returned will be completely homogeneous,
containing *either* ``Success`` or ``Failure``, but not both.  A ``Stream`` is
returned rather than a single value to allow for ambiguity in the grammar (see
below).

It's worth mentioning that GLL is a form of `recursive-descent parsing`_.  It has
all of the advantages of conventional recursive-descent, including an intuitive
control flow, arbitrary positioning of semantic actions and superior error
messages.  In fact, it is the fact that GLL is recursive-descent which allows it
to be implemented using atomic combinators.  Other algorithms which share the
same capabilities as GLL (such as GLR, Earley Parsing, etc) are fundamentally
incompatible with the combinator model due to their highly-unintuitive control
flow.  In GLL parsing, the control flow follows that of the grammar, as it does
in traditional parser combinators or any other form of recursive-descent.

.. _`GLL parsing algorithm`: http://ldta.info/2009/ldta2009proceedings.pdf
.. _recursive-descent parsing: http://en.wikipedia.org/wiki/Recursive_descent_parser


Getting Started
===============

Simply install the GLL Combinators framework into your local
repository (by default, ``~/.ivy2/cache/``). This is easily
accomplished by using the following command::

    sbt publish-local

Once the JAR is on your CLASSPATH, you should have the following packages available
for use:

* ``com.codecommit.gll`` -- The complete public interface for the framework.
* ``com.codecommit.util`` -- A number of useful utility classes used internally.  Should
  not be considered as part of the stable API.

Fundamentals
------------

**TODO**


Examples
--------

**TODO**


Advantages
==========
    
Despite superficial syntactic similarities, the GLL combinators framework
does improve somewhat upon the syntax provided by the default Scala framework.
For example, we can modify the above grammar with semantic actions using the
``^^`` combinator::
    
    lazy val expr: Parser[Any] = (
        "(" ~ expr ~ ")" ^^ { _ + _ + _ }
      | ""
    )
    
The key difference here is that GLL combinators does not require semantic actions
to be defined as functions of arity-1 (although this is supported).  Instead, the
semantic action is expected (and type-checked) to be of the same arity as the
number of tokens returned by the relevant production.  Unlike the default Scala
framework, this is made possible by an extremely intricate series of implicit
conversions, allowing Scala to automatically infer the argument types of the
function.  This is what enables use of the "underscore shorthand" as shown above.

GLL combinators are also capable of parsing grammars with certain useful properties,
such as left-recursion and even ambiguity.  In fact, the GLL algorithm is capable
of parsing *any* context-free grammar, no matter how arcane.  As an example,
consider this very natural grammar for arithmetic expressions::
    
    lazy val expr: Parser[Int] = (
        expr ~ "*" ~ expr     ^^ { (e1, _, e2) => e1 * e2 }
      | expr ~ "/" ~ expr     ^^ { (e1, _, e2) => e1 / e2 }
      | expr ~ "+" ~ expr     ^^ { (e1, _, e2) => e1 + e2 }
      | expr ~ "-" ~ expr     ^^ { (e1, _, e2) => e1 - e2 }
      | "(" ~> expr <~ ")"
      | "-" ~> expr           ^^ { -_ }
      | """\d+""".r           ^^ { _.toInt }
    )
    
Unfortunately, while this grammar may be very natural, it is also well beyond
the capabilities of a traditional parser combinator framework.  Specifically,
this grammar exhibits *both* left-recursion and a rather pesky form of ambiguity.
For the uninitiated, left-recursion is when a non-terminal -- in this case, 
``expr`` -- refers to itself as the first token in any one of its productions
-- in this case, the productions for multiplication, division, addition and
subtraction.  Ambiguity is when it is possible for a parser to produce two
different values from the same input while still following the rules of the
grammar.  The ``expr`` parser is ambiguous in two ways: first, it doesn't dictate
operator associativity ("``1 + 2 + 3``" could parse as either "``(1 + 2) + 3``"
or "``1 + (2 + 3)``"); second, it doesn't dictate operator precedence ("``1 + 2 * 3``"
could parse as either "``(1 + 2) * 3``" or "``1 + (2 * 3)``").

Now, the updated parser combinator framework in Scala 2.8.0 will be able to handle
the left-recursion aspect of this grammar (through the use of a modified form of
the Packrat algorithm), but not the ambiguity.  This is where the GLL algorithm
really begins to shine.  Let's imagine that we ran our parser in the following
way::
    
    val results = expr("-1 + 2 * 3")
    
The ``results`` value will contain the following ``Stream`` [#]_::
    
    Stream(Success(-7,), Success(5,), Success(-9,), Success(3,))
    
These results represent all of the different values which can be produced by
following the grammar while parsing the input string "``1 + 2 * -3 + 4``".  The
different interpretations are as follows:

 ========== ================
  Value      Interpretation 
 ========== ================
 **5**      (-1) + (2 * 3)  
 ---------- ----------------
 **-9**     -(1 + 2) * 3    
 ---------- ----------------
 **3**      ((-1) + 2) * 3  
 ---------- ----------------
 **-9**     -((1 + 2) * 3)  
 ---------- ----------------
 **-7**     -(1 + (2 * 3))  
 ========== ================

If we were to feed this grammar into the 2.7.4 (or earlier) version of the Scala
parser combinator framework, the result would be an immediate infinite loop as
the ``expr`` parser attempted to consume an ``expr`` as the first step in
consuming an ``expr`` (a well-known problem inherent to recursive-descent_).  As
mentioned earlier, the Scala 2.8.0 version of the framework would do better,
parsing the input completely and producing a result.  However, this would produce
only one of the four possible results (shown above).  In other words, even Packrat
parser combinators (as are used in Scala 2.8.0) must select a single unambiguous
line to follow at the expense of the other possibilities.  While this sounds like
a good thing, it ultimately imposes some severe limits on the grammars which can
be handled.

Ambiguity is *essential* in fields like natural-language processing, where the
language to be parsed may even be inherantly ambiguous.  However, it is also
extremely useful in other, less escoteric applications.  While it is always possible
to create an unambiguous grammar for a language which does not have any inherant
ambiguity, it is often *easier* to simply allow for local ambiguity which is
resolved later on in the parse.
    
    **TODO:** I suppose I should come up with an example here.  Maybe Haskell?
    
Critically, GLL does not impose a significant cost when dealing with ambiguous
grammars.  One would expect that following all possible parse trees in a highly-ambiguous
grammar would lead to exponentially long runtimes.  However, GLL is able to
effectively exploit the same data structure which allows generalized bottom-up
parsing algorithms (such as GLR) to function efficiently: the `graph-structured stack`_.
Describing this data structure is beyond the scope of this README.  Instead, I
would refer you to `this paper by Masaru Tomita`_, original creator of GLR and
inventor of the graph-structured stack.  Suffice it to say that the GSS makes it
possible for the GLL combinators framework to parse *any* grammar in *O(n^3)*
time.  This is even better than GLR, which is *O(n^4)* in the worst case.
    
Note that ``Stream`` is used as a result type (rather than ``List``) to allow
users to retrieve only the results which they actually need.  Technically, generalized
*parsing* has an exponential lower-bound due to the fact that a parser may need
to return an exponential number of results.  The *O(n^3)* performance guarantee
offered by GLL is only valid when GLL is being used as a recognizer with a single
result value for all parse trails.  To get around this problem, the parse process
will run *only* until it reaches the first successful value (or runs out of
possible parse trails to attempt).  Once it hits this first success, it bundles
up the ``Result[A]`` along with a thunk_ representing the remainder of the parse.
If you only require a single result, then the remainder of the parse can
be discarded, resulting in truly *O(n^3)* performance in the worst case (likely
much faster).  If you need *all* possible results, then you are free to enumerate
the entire result ``Stream``, forcing the parse to return all possible values.

Please note that Scala's ``Stream`` implementation is highly prone to memory
leaks.  For example, even if you have already traversed the entire ``Stream``
(and thus completed the parse), the data structure will continue to maintain a
reference to the transient data structures used during the GLL parse process.
It is recommended that you allow the result ``Stream`` to go out of scope as
quickly as possible.  If you need to retain a list of results for any amount of
time, you should use the ``toList`` method to copy the ``Stream`` into a ``List``,
rather than simply saving a reference to the ``Stream``.

.. [#] The "extra" comma in the ``Success`` constructors is not a typo, it
       indicates that the entire stream was consumed by the parse.  Without some
       serious conniptions, this is the default.  Any ``Success`` which does not
       consume the entire stream is converted into a ``Failure`` prior to return.
       This is to enforce greedy matching in repetitions (the default for PEGs_).

.. _recursive-descent: http://en.wikipedia.org/wiki/Recursive_descent_parser
.. _PEGs: http://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _graph-structured stack: http://en.wikipedia.org/wiki/Graph-structured_stack
.. _this paper by Masaru Tomita: http://acl.ldc.upenn.edu/P/P88/P88-1031.pdf
.. _thunk: http://en.wikipedia.org/wiki/Thunk#Thunk_as_delayed_computation


Performance
===========

At the moment, performance is basically non-existent.  The GLL algorithm itself
is *O(n^3)* even in the worst case, but there is a high constant factor which is
introduced by the framework which makes this quite a bit slower than it sounds.
This is significantly better than traditional parser combinators, which are *O(k^n)*
in the worst case (where *k* is a constant representing the ambiguity of the
grammar), but the constant overhead imposed by the framework does make parsing
according to the average grammar a somewhat longer affair than the traditional
parser combinators or even mainstream bottom-up parsers such as Bison.
A good example of poor performance is the **MiniML** example in the ``examples/``
directory.  Another, somewhat pathological example is the following highly-ambiguous
grammar::
    
    lazy val s: Parser[String] = (
        "b"
      | s ~ s       ^^ { _ + _ }
      | s ~ s ~ s   ^^ { _ + _ + _ }
    )
    
It takes roughly 18 seconds to run this grammar against an input consisting of
the letter ``b`` repeated 100 times.  If we increase that number to 300, the
parser will actually exhaust the available heap space in the default JVM
configuration.

The actual performance on the ``s`` grammar is demonstrated by the following
graph (plotted on a cubic scale).  The gray line is *y = kx^3* (for some constant
*k*).  The blue line was determined emperically from progressively longer runs
(starting at strings of length 10 and increasing to length 100) on the ``s``
parser shown above.  The *y* axis represents time in milliseconds.

.. image:: performance.jpg

With all this said, there are very few grammar/input combinations which push the
framework to its limit.  In fact, for grammars which are LL(1)_, the GLL Combinators
framework should actually be *faster* than traditional parser combinators.  For
example::
    
    val num = ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9") ^^ { _.toInt }
    
In order to parse this grammar, traditional parser combinators would require time
proportional to the number of alternates (in this case, 10).  GLL Combinators are
capable of parsing this grammar in linear time (*O(n)*), which is equivalent to
the best LL(k) parsers.  This is because the GLL algorithm degrades gracefully
to predictive recursive-descent when the grammar (or sub-grammar) is LL(1).
Note that GLL also lacks any form of conventional backtracking, which is how it
is able to avoid the exponential cases which make naive recursive-descent so
problematic.

It is also worth noting that the GLL algorithm is inherantly parallelizable.
This means that, given enough processors, GLL should be quite a bit faster (in
terms of total parse time) than any conventional bottom-up *or* top-down parser.
The framework does not currently exploit this design property, but the plan is
to eventually do so.  Essentially, the parse would seamlessly distribute across
all available cores.  The more ambiguous the grammar, the better the algorithm
could parallelize the parse.

.. _LL(1): http://en.wikipedia.org/wiki/LL(1)


Theory
======

The theoretical underpinnings for GLL are quite interesting, but also beyond the
scope of this readme.  I would refer you to `the original paper`_ by doctors
Elizabeth Scott and Adrian Johnstone of Royal Holloway, University of London.

In a nutshell, the algorithm is almost identical to conventional single-token predictive
recursive-descent parsing with no backtracking.  This technique (recursive-descent)
is only capable of handling grammars which are LL(1), meaning no left-recursion,
no ambiguity, and no alternates which begin with the same token.  The key difference
is that GLL uses a *trampoline* function to dispatch ambiguous alternates.  The
idea of using a trampoline function to implement mutual tail-recursion in
constant stack space is a well-known technique in functional programming (it's
at the heart of Scheme's dispatch system).  However, GLL is the first (to my
knowledge) to apply this idea to text parsing.

The trampoline contains a queue (or stack) of pending alternate productions and
their corresponding position in the input stream.  Any number of alternates may
be pending at any given point in time.  These alternates are considered individually
and parsed using conventional recursive-descent.  That is, until the parsing
process hits another ambiguity, at which point the possible alternates are added
to the trampoline and control flow is returned to the main loop.  This process
continues until no further alternates are available.

The entire proceding is saved from exponentially-long runtimes by the 
graph-structured stack (GSS), a well-known device used in many generalized parsing
algorithms.  GLL expands slightly upon the original concept of the GSS by allowing
for full-blown cycles in the graph structure, symbolizing direct or indirect
left-recursion.  These cycles effectively take the place of the ``GOTO`` operation
used by LR parser automata on grammars with *non-hidden* left-recursion (hidden
left-recursion, where the left-recursive production has a nullable non-terminal
(one which goes to the empty string) as its first token, is not supported by any
of the mainstream LR variants, including the ever-popular LALR).

.. _the original paper: http://ldta.info/2009/ldta2009proceedings.pdf
