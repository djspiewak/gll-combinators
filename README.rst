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
    
This will return a value of type ``List[Result[Any]]``.  The ``Result[A]`` ADT is
defined as one of the following (for some type ``A``):

* ``Success[A]`` -- Represents a successful parse and contains the resulting value.
* ``Failure`` -- Represents a failed parse and contains the relevant error message
  as well as the remainder of the parse stream (the characters which were not
  consumed).

If any result is successful (i.e. an instance of ``Success[A]``), then no failures
will be returned.  Thus, the ``List`` returned will be completely homogeneous,
containing *either* ``Success`` or ``Failure``, but not both.  A ``List`` is
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

* Usage_
    * Fundamentals_
    * Examples_
* Advantages_
* Performance_
* Theory_

.. _`GLL parsing algorithm`: http://ldta.info/ldta2009proceedings.pdf
.. _recursive-descent parsing: http://en.wikipedia.org/wiki/Recursive_descent_parser


.. _Usage:

Usage
=====

Thanks to `Apache Buildr`_, it is quite simple to build the GLL Combinators
library into a fully-realized JAR.  Note that you will need Buildr 1.3.4 (or later)
installed, otherwise the build will fail while attempting to run the test suite.
You can create the library JAR by running the following command::
    
    buildr package
    
This will produce a file of the form ``gll-combinators-0.3.0.jar`` (where "``0.3.0``"
is the current version number) in the ``target/`` directory.  Note that this
command will also invoke the test suite, failing if any of the tests are
unsuccessful.  You can by-pass the test suite by appending the ``test=no`` option
to the command given above.

If you are using Buildr or Maven2, it may be easier to simply install the GLL
Combinators framework into your local repository (by default, ``~/.m2/repository/``).
This is easily accomplished by using the following command::
    
    buildr install
    
Once this installation is complete, you should be able to reference the library
using a ``groupId`` of "``edu.uwm.cs``", an ``artifactId`` of "``gll-combinators``"
and the relevant version number (can be found in ``buildfile`` at the root of
the project).  If you are using Buildr, the full artifact descriptor would be
"``edu.uwm.cs:gll-combinators:jar:0.3.0``" (for version **0.3.0**).  If you are
using Maven2, you will want to add the following snippet to your POM file::
    
    <dependency>
      <groupId>edu.uwm.cs</groupId>
      <artifactId>gll-combinators</artifactId>
      <version>0.3.0</version>
    </dependency>
    
Once the JAR is on your CLASSPATH, you should have the following packages available
for use:

* ``edu.uwm.cs.gll`` -- The complete public interface for the framework.
* ``edu.uwm.cs.util`` -- A number of useful utility classes used internally.  Should
  not be considered as part of the stable API.

.. _`Apache Buildr`: http://buildr.apache.org


.. _Fundamentals:

Fundamentals
------------

**TODO**


.. _Examples:

Examples
--------

**TODO**


.. _Advantages:

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
    
The ``results`` value will contain the following ``List`` [#]_::
    
    List(Success(-7,), Success(5,), Success(-9,), Success(3,))
    
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

.. [#] The "extra" comma in the ``Success`` constructors is not a typo, it
       indicates that the entire stream was consumed by the parse.  Without some
       serious conniptions, this is the default.  Any ``Success`` which does not
       consume the entire stream is converted into a ``Failure`` prior to return.
       This is to enforce greedy matching in repetitions (the default for PEGs_).

.. _recursive-descent: http://en.wikipedia.org/wiki/Recursive_descent_parser
.. _PEGs: http://en.wikipedia.org/wiki/Parsing_expression_grammar
.. _graph-structured stack: http://en.wikipedia.org/wiki/Graph-structured_stack
.. _this paper by Masaru Tomita: http://acl.ldc.upenn.edu/P/P88/P88-1031.pdf


Performance
===========

**TODO**


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
used by LR parser automata on grammars with *direct* left-recursion (indirect
left-recursion is not supported by any of the mainstream LR variants, including
the ever-popular LALR).

.. _the original paper: http://ldta.info/ldta2009proceedings.pdf
