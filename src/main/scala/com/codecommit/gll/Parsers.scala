/*
 * Copyright (c) 2021, Daniel Spiewak
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.codecommit.gll

import scala.collection.mutable
import mutable.{Buffer, ListBuffer}

import com.codecommit.util._
import scala.collection.compat.immutable.LazyList

import Global._

// I hate the way this file is organized, but I don't have a choice
trait Parsers {
  import SetSyntax._

  // private val TAIL_ERROR_PATTERN = "Unexpected trailing characters: '%s'"

  implicit def literal(str: String) = new LiteralParser(str)

  def opt[A](p: Parser[A]) = p?

  def rep[A](p: Parser[A]) = p*

  def rep1[A](p: Parser[A]) = p+

  private def processTail(tail: LineStream) = {
    val newTail = handleWhitespace(tail)
    if (newTail.isEmpty) Some(newTail) else None
  }

  protected def handleWhitespace(s: LineStream) = s

  private def canonicalize(str: String) = str.foldLeft("") { (back, c) =>
    val tack = c match {
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\f' => "\\f"
      case c => c.toString
    }

    back + tack
  }

  // implicit conversions

  implicit def disjunctiveSyntax[A](left: =>Parser[A]) = new RichParser(left)
  implicit def disjunctiveLiterals(left: String) = new RichParser(literal(left))

  implicit def funSyntax1[A](p: Parser[A]) = new RichSyntax1(p)
  implicit def funLitSyntax(p: String) = new RichSyntax1(literal(p))
  implicit def funSyntax2[A, B](p: Parser[A ~ B]) = new RichSyntax2(p)

  implicit def funSyntax3l[A, B, C](p: Parser[A ~ B ~ C]) = new RichSyntax3l(p)
  implicit def funSyntax3r[A, B, C](p: Parser[~[A, B ~ C]]) = new RichSyntax3r(p)

  implicit def funSyntax4ll[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) = new RichSyntax4ll(p)
  implicit def funSyntax4lr[A, B, C, D](p: Parser[~[A, B ~ C] ~ D]) = new RichSyntax4lr(p)
  implicit def funSyntax4rl[A, B, C, D](p: Parser[~[A, B ~ C ~ D]]) = new RichSyntax4rl(p)
  implicit def funSyntax4rr[A, B, C, D](p: Parser[~[A, ~[B, C ~ D]]]) = new RichSyntax4rr(p)

  implicit def funSyntax5lll[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]) = new RichSyntax5lll(p)
  implicit def funSyntax5llr[A, B, C, D, E](p: Parser[~[A, B ~ C] ~ D ~ E]) = new RichSyntax5llr(p)
  implicit def funSyntax5lrl[A, B, C, D, E](p: Parser[~[A, B ~ C ~ D] ~ E]) = new RichSyntax5lrl(p)
  implicit def funSyntax5lrr[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D]] ~ E]) = new RichSyntax5lrr(p)
  implicit def funSyntax5rll[A, B, C, D, E](p: Parser[~[A ~ B, C ~ D ~ E]]) = new RichSyntax5rll(p)
  implicit def funSyntax5rlr[A, B, C, D, E](p: Parser[~[A ~ B, ~[C, D ~ E]]]) = new RichSyntax5rlr(p)
  implicit def funSyntax5rrl[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D ~ E]]]) = new RichSyntax5rrl(p)
  implicit def funSyntax5rrr[A, B, C, D, E](p: Parser[~[A, ~[B, ~[C, D ~ E]]]]) = new RichSyntax5rrr(p)

  implicit def funSyntax6[A, B, C, D, E, F](p: Parser[A ~ B ~ C ~ D ~ E ~ F]) = new RichSyntax6(p)
  implicit def funSyntax7[A, B, C, D, E, F, G](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G]) = new RichSyntax7(p)
  implicit def funSyntax8[A, B, C, D, E, F, G, H](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G ~ H]) = new RichSyntax8(p)

  class RichParser[A](left: =>Parser[A]) {
    def |[B >: A](right: =>Parser[B]): Parser[B] = new DisjunctiveParser(left, right)
  }

  // map syntax

  class RichSyntax1[A](p: Parser[A]) {
    def ^^[R](f: A => R) = ^# { (_, r) => f(r) }

    def ^#[R](f: (LineStream, A) => R) = p mapWithTail f
  }

  class RichSyntax2[A, B](p: Parser[A ~ B]) {
    def ^^[R](f: (A, B) => R) = ^# { (_, r1, r2) => f(r1, r2) }

    def ^#[R](fun: (LineStream, A, B) => R) = p mapWithTail { case (in, a ~ b) => fun(in, a, b) }
  }

  class RichSyntax3l[A, B, C](p: Parser[A ~ B ~ C]) {
    def ^^[R](f: (A, B, C) => R) = ^# { (_, r1, r2, r3) => f(r1, r2, r3) }

    def ^#[R](fun: (LineStream, A, B, C) => R) = p mapWithTail { case (in, a ~ b ~ c) => fun(in, a, b, c) }
  }

  class RichSyntax3r[A, B, C](p: Parser[~[A, B ~ C]]) {
    def ^^[R](f: (A, B, C) => R) = ^# { (_, r1, r2, r3) => f(r1, r2, r3) }

    def ^#[R](fun: (LineStream, A, B, C) => R) = p mapWithTail { case (in, a ~ (b ~ c)) => fun(in, a, b, c) }
  }

  class RichSyntax4ll[A, B, C, D](p: Parser[A ~ B ~ C ~ D]) {
    def ^^[R](f: (A, B, C, D) => R) = ^# { (_, r1, r2, r3, r4) => f(r1, r2, r3, r4) }

    def ^#[R](fun: (LineStream, A, B, C, D) => R) = p mapWithTail { case (in, a ~ b ~ c ~ d) => fun(in, a, b, c, d) }
  }

  class RichSyntax4lr[A, B, C, D](p: Parser[~[A, B ~ C] ~ D]) {
    def ^^[R](f: (A, B, C, D) => R) = ^# { (_, r1, r2, r3, r4) => f(r1, r2, r3, r4) }

    def ^#[R](fun: (LineStream, A, B, C, D) => R) = p mapWithTail { case (in, a ~ (b ~ c) ~ d) => fun(in, a, b, c, d) }
  }

  class RichSyntax4rl[A, B, C, D](p: Parser[~[A, B ~ C ~ D]]) {
    def ^^[R](f: (A, B, C, D) => R) = ^# { (_, r1, r2, r3, r4) => f(r1, r2, r3, r4) }

    def ^#[R](fun: (LineStream, A, B, C, D) => R) = p mapWithTail { case (in, a ~ ((b ~ c) ~ d)) => fun(in, a, b, c, d) }
  }

  class RichSyntax4rr[A, B, C, D](p: Parser[~[A, ~[B, C ~ D]]]) {
    def ^^[R](f: (A, B, C, D) => R) = ^# { (_, r1, r2, r3, r4) => f(r1, r2, r3, r4) }

    def ^#[R](fun: (LineStream, A, B, C, D) => R) = p mapWithTail { case (in, a ~ (b ~ (c ~ d))) => fun(in, a, b, c, d) }
  }

  class RichSyntax5lll[A, B, C, D, E](p: Parser[A ~ B ~ C ~ D ~ E]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, a ~ b ~ c ~ d ~ e) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5llr[A, B, C, D, E](p: Parser[~[A, B ~ C] ~ D ~ E]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, (a ~ (b ~ c)) ~ d ~ e) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5lrl[A, B, C, D, E](p: Parser[~[A, B ~ C ~ D] ~ E]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, (a ~ (b ~ c ~ d)) ~ e) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5lrr[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D]] ~ E]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, (a ~ (b ~ (c ~ d))) ~ e) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5rll[A, B, C, D, E](p: Parser[~[A ~ B, C ~ D ~ E]]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, (a ~ b) ~ (c ~ d ~ e)) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5rlr[A, B, C, D, E](p: Parser[~[A ~ B, ~[C, D ~ E]]]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, (a ~ b) ~ (c ~ (d ~ e))) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5rrl[A, B, C, D, E](p: Parser[~[A, ~[B, C ~ D ~ E]]]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, a ~ (b ~ (c ~ d ~ e))) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax5rrr[A, B, C, D, E](p: Parser[~[A, ~[B, ~[C, D ~ E]]]]) {
    def ^^[R](f: (A, B, C, D, E) => R) = ^# { (_, r1, r2, r3, r4, r5) => f(r1, r2, r3, r4, r5) }

    def ^#[R](fun: (LineStream, A, B, C, D, E) => R) = p mapWithTail { case (in, a ~ (b ~ (c ~ (d ~ e)))) => fun(in, a, b, c, d, e) }
  }

  class RichSyntax6[A, B, C, D, E, F](p: Parser[A ~ B ~ C ~ D ~ E ~ F]) {
    def ^^[R](f: (A, B, C, D, E, F) => R) = ^# { (_, r1, r2, r3, r4, r5, r6) => f(r1, r2, r3, r4, r5, r6) }

    def ^#[R](fun: (LineStream, A, B, C, D, E, F) => R) = p mapWithTail { case (in, a ~ b ~ c ~ d ~ e ~ f) => fun(in, a, b, c, d, e, f) }
  }

  class RichSyntax7[A, B, C, D, E, F, G](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G]) {
    def ^^[R](f: (A, B, C, D, E, F, G) => R) = ^# { (_, r1, r2, r3, r4, r5, r6, r7) => f(r1, r2, r3, r4, r5, r6, r7) }

    def ^#[R](fun: (LineStream, A, B, C, D, E, F, G) => R) = p mapWithTail { case (in, a ~ b ~ c ~ d ~ e ~ f ~ g) => fun(in, a, b, c, d, e, f, g) }
  }

  class RichSyntax8[A, B, C, D, E, F, G, H](p: Parser[A ~ B ~ C ~ D ~ E ~ F ~ G ~ H]) {
    def ^^[R](f: (A, B, C, D, E, F, G, H) => R) = ^# { (_, r1, r2, r3, r4, r5, r6, r7, r8) => f(r1, r2, r3, r4, r5, r6, r7, r8) }

    def ^#[R](fun: (LineStream, A, B, C, D, E, F, G, H) => R) = p mapWithTail { case (in, a ~ b ~ c ~ d ~ e ~ f ~ g ~ h) => fun(in, a, b, c, d, e, f, g, h) }
  }

  //////////////////////////////////////////////////////////////////////////////

  sealed trait Parser[+R] extends (LineStream => LazyList[Result[R]]) { self =>
    val terminal: Boolean

    lazy val first = {
      val set = computeFirst(Set()) getOrElse Set()

      if (set contains None)
        UniversalCharSet         // if \epsilon \in FIRST
      else
        set flatMap { x => x }
    }

    def isPreferred = false

    /**
     * @return The FIRST set for this parser, or the empty set
     *         if the production goes to \epsilon.
     */
    def computeFirst(seen: Set[Parser[Any]]): Option[Set[Option[Char]]]

    def chain(t: Trampoline, in: LineStream)(f: Result[R] => Unit): Unit

    // syntax

    def apply(str: String): LazyList[Result[R]] = apply(LineStream(str))

    def map[R2](f: R => R2) = mapWithTail { (_, r) => f(r) }

    def mapWithTail[R2](f: (LineStream, R) => R2): Parser[R2]

    def flatMap[R2](f1: R => Parser[R2]): Parser[R2] = new NonTerminalParser[R2] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)

      def chain(t: Trampoline, in: LineStream)(f2: Result[R2] => Unit) = {
        self.chain(t, in) {
          case Success(res1, tail) => f1(res1).chain(t, tail)(f2)
          case f: Failure => f2(f)
        }
      }
    }

    def filter(f: R => Boolean): Parser[R] = new NonTerminalParser[R] {
      override def isPreferred = self.isPreferred

      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)

      def chain(t: Trampoline, in: LineStream)(f2: Result[R] => Unit) = {
        self.chain(t, in) {
          case s @ Success(res, _) => {
            if (f(res))
              f2(s)
            else
              f2(Failure(SyntaxError, in))
          }

          case f: Failure => f2(f)
        }
      }
    }

    def orElse[R2 >: R](alternate: =>Parser[R2]): Parser[R2] = new DisjunctiveParser(this, alternate)

    // operators
    def ~[R2](that: Parser[R2]): Parser[R ~ R2] = new SequentialParser(this, that)

    def <~[R2](that: Parser[R2]) = this ~ that map { case a ~ _ => a }

    def ~>[R2](that: Parser[R2]) = this ~ that map { case _ ~ b => b }

    def * : Parser[List[R]] = (this+?) map { _ getOrElse Nil }

    def **(sep: Parser[_]): Parser[List[R]] = (this ++ sep).? ^^ { _ getOrElse Nil }

    def + : Parser[List[R]] = new NonTerminalParser[List[R]] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen + this)

      def chain(t: Trampoline, in: LineStream)(f: Result[List[R]] => Unit) = {
        t.add(self, in) {
          case Success(res1, tail) => {
            f(Success(res1 :: Nil, tail))

            if ((!tail.isEmpty || first.size == 0) && (tail.isEmpty || (first.contains(in.head) || first.size == 0))) {      // lookahead}
              t.add(this, tail) {
                case Success(res2, tail) => f(Success(res1 :: res2, tail))
                case res: Failure => f(res)
              }
            }
          }

          case res: Failure => f(res)
        }
      }

      override def toString = self.toString + "+"
    }

    def ++(sep: Parser[_]) = this ~ (sep ~> this).* ^^ { _ :: _ }

    def ? : Parser[Option[R]] = new NonTerminalParser[Option[R]] {
      def computeFirst(seen: Set[Parser[Any]]) =
        Some(self.computeFirst(seen) map { _ + None } getOrElse Set(None))

      def chain(t: Trampoline, in: LineStream)(f: Result[Option[R]] => Unit) = {
        f(Success(None, in))

        t.add(self, in) {
          case Success(res, tail) => f(Success(Some(res), tail))
          case res: Failure => f(res)
        }
      }

      override def toString = self.toString + "?"
    }

    def +? = (this+)?

    def \(not: TerminalParser[Any]): Parser[R] = new NonTerminalParser[R] {
      def computeFirst(seen: Set[Parser[Any]]) = self.computeFirst(seen)

      def chain(t: Trampoline, in: LineStream)(f: Result[R] => Unit) = {
        lazy val sub = not.parse(in)

        self.chain(t, in) {
          case s @ Success(_, tail) => {
            if (sub match { case Success(_, `tail`) => true case _ => false })
              f(Failure(SyntaxError, in))
            else
              f(s)
          }

          case r: Failure => f(r)
        }
      }

      override def toString = "(%s \\ %s)".format(self, not)
    }

    def ^^^[R2](v: =>R2) = map { _ => v }
  }

  trait TerminalParser[+R] extends Parser[R] { self =>
    final val terminal = true

    final def apply(in: LineStream) = LazyList(parse(handleWhitespace(in)) match {
      case Success(res, tail) => processTail(tail) match {
        case Some(tail) => Success(res, tail)
        case None => Failure(UnexpectedTrailingChars(canonicalize(tail.mkString)), tail)
      }

      case x => x
    })

    /**
     * For terminal parsing, this just delegates back to apply()
     */
    def chain(t: Trampoline, in: LineStream)(f: Result[R] => Unit) = {
      f(parse(handleWhitespace(in)))
    }

    protected[gll] def parse(in: LineStream): Result[R]

    override def ~[R2](other: Parser[R2]) = other match {
      case other: TerminalParser[R2] => {
        new TerminalParser[R ~ R2] {
          def computeFirst(s: Set[Parser[Any]]) = {
            val sub = self.computeFirst(s)

            sub map { set =>
              if (set.size == 0 || set.contains(None))
                other.computeFirst(s) match {
                  case Some(set2) => {
                    if (set.isComplement)
                      (set - None) ++ set2
                    else
                      set2 ++ (set - None)
                  }

                  case None => set
                }
              else
                set
            }
          }

          def parse(in: LineStream) = self.parse(handleWhitespace(in)) match {
            case Success(res1, tail) => other.parse(handleWhitespace(tail)) match {
              case Success(res2, tail) => Success(new ~(res1, res2), tail)
              case f: Failure => f
            }

            case f: Failure => f
          }
        }
      }

      case other => super.~(other)
    }

    override def \(not: TerminalParser[Any]) = new TerminalParser[R] {
      override def isPreferred = self.isPreferred

      def computeFirst(s: Set[Parser[Any]]) = self.computeFirst(s)

      def parse(in: LineStream) = self.parse(in) match {
        case s @ Success(_, tail) => {
          val sub = not.parse(in)

          if (sub match { case Success(_, `tail`) => true case _ => false })
            Failure(SyntaxError, in)
          else
            s
        }

        case f: Failure => f
      }
    }

    def mapWithTail[R2](f: (LineStream, R) => R2): Parser[R2] = new MappedParser[R, R2](self, f) with TerminalParser[R2] {
      override def isPreferred = self.isPreferred

      def parse(in: LineStream) = {
        val newTail = handleWhitespace(in)
        self.parse(newTail) match {
          case Success(res, tail) => Success(f(newTail, res), tail)
          case x: Failure => x
        }
      }
    }

    def preferred: TerminalParser[R] = PreferredParser(this)
  }

  trait NonTerminalParser[+R] extends Parser[R] { self =>
    final val terminal = false

    /**
     * This method takes care of kicking off a <i>new</i>
     * parse process.  We will never call this method to
     * handle a sub-parse.  In such situations, we will use
     * the trampoline to queue results.
     *
     * Note: to ensure greedy matching (for PEG compatibility)
     * we define any Success with a non-empty tail to be a
     * Failure
     */
    final def apply(in: LineStream) = {
      val t = new Trampoline

      val successes = mutable.Set[Success[R]]()
      val failures = mutable.Set[Failure]()

      var recognized = false

      def parse(): LazyList[Result[R]] = {
        if (t.hasNext) {
          t.step()

          if (successes.isEmpty) {
            parse()
          } else {
            val results = successes.toList
            successes.clear()
            LazyList(results: _*) ++ parse()
          }
        } else {
          val results = if (recognized) successes else failures
          LazyList(results.toList: _*)
        }
      }

      chain(t, in) {
        case s @ Success(res, tail) => {
          tracef("Top-Level Success: %s%n", s)
          processTail(tail) match {
            case Some(tail) => {
              tracef("Tail Accepted: %s%n", s)
              recognized = true
              successes += Success(res, tail)
            }

            case None => {
              tracef("Tail Rejected: %s%n", s)
              failures += Failure(UnexpectedTrailingChars(canonicalize(tail.mkString)), tail)
            }
          }
        }

        case f: Failure => {
          tracef("Top-Level Failure: %s%n", f)
          failures += f
        }
      }

      parse()
    }

    def mapWithTail[R2](f1: (LineStream, R) => R2): Parser[R2] = new MappedParser[R, R2](self, f1) with NonTerminalParser[R2] {
      def chain(t: Trampoline, in: LineStream)(f2: Result[R2] => Unit) = {
        self.chain(t, in) {
          case Success(res, tail) => f2(Success(f1(in, res), tail))
          case f: Failure => f2(f)
        }
      }
    }
  }

  abstract class MappedParser[A, +B](private val p: Parser[A], private val f1: (LineStream, A) => B) extends Parser[B] {
    def computeFirst(s: Set[Parser[Any]]) = p.computeFirst(s + this)

    override def toString = p.toString
  }

  /**
   * Used for setting up a trampoline wrapper for disjunction
   * alternatives (for the sake of left-recursion).
   */
  private[gll] abstract class ThunkParser[+A](private val self: Parser[A]) extends NonTerminalParser[A] {
    override def isPreferred = self.isPreferred

    def computeFirst(s: Set[Parser[Any]]) = self.computeFirst(s)

    override def toString = self.toString

    override def equals(other: Any) = other match {
      case that: ThunkParser[A] => this.self == that.self
      case _ => false
    }

    override def hashCode = self.hashCode
  }

  private[gll] case class PreferredParser[+R](delegate: TerminalParser[R]) extends TerminalParser[R] {
    override def isPreferred = true

    def computeFirst(s: Set[Parser[Any]]) = delegate.computeFirst(s)

    protected[gll] def parse(in: LineStream) = delegate.parse(in)
  }

  //////////////////////////////////////////////////////////////////////////////

  case class LiteralParser(str: String) extends TerminalParser[String] {
    def computeFirst(s: Set[Parser[Any]]) = {
      Some(if (str.length > 0) Set(Some(str charAt 0)) else Set(None))
    }

    def parse(in: LineStream) = {
      val trunc = in take str.length

      if (trunc.lengthCompare(str.length) != 0) {
        Failure(UnexpectedEndOfStream(Some(str)), in)
      } else {
        val succ = trunc.zipWithIndex forall {
          case (c, i) => c == str.charAt(i)
        }

        if (succ)
          Success(str, in drop str.length)
        else
          Failure(ExpectedLiteral(str, canonicalize(trunc.mkString)), in)
      }
    }

    override def equals(other: Any) = other match {
      case that: LiteralParser => this.str == that.str
      case _ => false
    }

    override def hashCode = str.hashCode

    override def toString = "'%s'".format(str)
  }

  class SequentialParser[+A, +B](private val left: Parser[A], private val right: Parser[B]) extends NonTerminalParser[A ~ B] {
    def computeFirst(seen: Set[Parser[Any]]) = {
      if (seen contains this) None    // left-recursion detected!
      else {
        val newSeen = seen + this
        val sub = left.computeFirst(newSeen)

        sub map { set =>
          if (set.size == 0 || set.contains(None))
            right.computeFirst(newSeen) match {
              case Some(set2) => {
                if (set.isComplement)
                  (set - None) ++ set2
                else
                  set2 ++ (set - None)
              }

              case None => set
            }
          else
            set
        }
      }
    }

    def chain(t: Trampoline, in: LineStream)(f: Result[A ~ B] => Unit) = {
      left.chain(t, in) {
        case Success(res1, tail) => {
          right.chain(t, tail) {
            case Success(res2, tail) => f(Success(new ~(res1, res2), tail))

            case res: Failure => f(res)
          }
        }

        case res: Failure => f(res)
      }
    }

    override def equals(other: Any) = other match {
      case that: SequentialParser[A, B] => {
        this.left == that.left && this.right == that.right
      }

      case _ => false
    }

    override def hashCode = left.hashCode + right.hashCode

    override def toString = "(%s ~ %s)".format(left, right)
  }

  class DisjunctiveParser[A](l: =>Parser[A], r: =>Parser[A]) extends NonTerminalParser[A] with Thunkable {
    private lazy val left = l
    private lazy val right = r

    // private lazy val leftClass = thunk[Parser[A]]('l).getClass
    // private lazy val rightClass = thunk[Parser[A]]('r).getClass

    lazy val gather: List[Parser[A]] = gatherImpl(Set()).toList

    /**
     * The PREDICT table for this disjunction.  Please note that
     * this is a very different concept from the standard LL(k)
     * PREDICT set.  Specifically, the PREDICT table allows for
     * ambiguity in the prediction while still retaining <i>O(1)</i>
     * dispatch on disjunctions which are LL(1) and <i>near-O(1)</i>
     * for disjunctions which are not.
     *
     * Note that this is not actually sufficient to handle all
     * CFGs allowed by GLL.  Specifically, parsers with an empty
     * FIRST set must be handled specially.
     */
    lazy val predict: Map[Char, Parser[A]] = {
      gather.foldLeft(Map[Char, Parser[A]]()) { (map, p) =>
        p.first.foldLeft(map) { _.updated(_, p) }
      }
    }

    /**
     * Checks if all FIRST sets are disjoint and none
     * are empty.  This is convergent even for
     * left-recursive parsers.
     */
    lazy val isLL1: Boolean = {
      val sets = gather map { _.first }
      val areFinite = sets forall { !_.isComplement }

      if (areFinite) {
        val totalSize = sets.foldLeft(0) { _ + _.size }
        val union = sets.reduceLeft[Set[Char]] { _ ++ _ }
        (totalSize == union.size) && (sets forall { _.size > 0 })
      } else false
    }

    def computeFirst(seen: Set[Parser[Any]]) = {
      if (seen contains this)
        None          // left-recursion detected!
      else {
        val newSeen = seen + this

        val firstSets = gather map { _ computeFirst newSeen getOrElse Set[Option[Char]]() } toList
        val back = firstSets sortWith { (a, b) => a.isComplement || !b.isComplement } reduceLeft { _ ++ _ }

        Some(back)
      }
    }

    def chain(t: Trampoline, in: LineStream)(f: Result[A] => Unit) = {
      if (isLL1) {        // graceful degrade to LL(1)
        trace("Detected LL(1): " + this)

        if (in.isEmpty) {
          f(Failure(UnexpectedEndOfStream(None), in))
        } else {
          predict get in.head match {
            case Some(p) => p.chain(t, in)(f)

            case None => f(Failure(UnexpectedChars(in.head.toString), in))
          }
        }
      } else {
        val thunk = new ThunkParser(this) {
          def chain(t: Trampoline, in: LineStream)(f: Result[A] => Unit) = {
            var predicted = false
            val results = mutable.Set[Result[A]]()    // merge results

            val preferred = gather filter { _.isPreferred }
            val prefResults = preferred flatMap { _(in) }

            val prefSuccess = prefResults exists {
              case _: Success[_] => true
              case _ => false
            }

            if (prefSuccess) {
              prefResults foreach { res =>
                if (!results.contains(res)) {
                  tracef("Reduced preferred: %s *=> %s%n", this, res)

                  f(res)
                  results += res
                }
              }
            } else {
              for {
                p <- gather

                // [(S = {}) -> (FIRST = U)] /\ [~(S = {}) -> (S[0] \in FIRST)]
                if !in.isEmpty || p.first == UniversalCharSet
                if in.isEmpty || p.first.contains(in.head)      // lookahead
              } {
                predicted = true
                t.add(p, in) { res =>
                  if (!results.contains(res)) {
                    tracef("Reduced: %s *=> %s%n", this, res)

                    f(res)
                    results += res
                  }
                }
              }

              if (!predicted) {
                if (in.isEmpty)
                  f(Failure(UnexpectedEndOfStream(None), in))
                else
                  f(Failure(UnexpectedChars(in.head.toString), in))
              }
            }
          }
        }

        t.add(thunk, handleWhitespace(in))(f)
      }
    }

    private def gatherImpl(seen: Set[DisjunctiveParser[A]]): Buffer[Parser[A]] = {
      val newSeen = seen + this

      def process(p: Parser[A]) = p match {
        case d: DisjunctiveParser[A] => {
          if (!seen.contains(d))
            d.gatherImpl(newSeen)
          else
            new ListBuffer[Parser[A]]
        }

        case p => p +: new ListBuffer[Parser[A]]
      }

      process(left) ++ process(right)
    }

    override def toString = "(%d | %d)".format(left.hashCode, right.hashCode)
  }

  //////////////////////////////////////////////////////////////////////////////

  class Trampoline {
    // private type RSet[A] = mutable.Set[Result[A]]
    private type SSet[A] = mutable.Set[Success[A]]
    private type FSet[A] = mutable.Set[Result[A] => Unit]

    // R
    private var queue: List[(Parser[Any], LineStream)] = Nil

    // U_j
    private val done = mutable.Map[LineStream, mutable.Set[Parser[Any]]]()

    // P
    private val popped = mutable.Map[LineStream, HOMap[Parser, SSet]]()

    // GSS back edges
    private val backlinks = mutable.Map[LineStream, HOMap[Parser, FSet]]()

    // prevents divergence in cyclic GSS traversal
    private val saved = HOMap[Result, FSet]()

    // L_0
    def run() = {
      while (hasNext) {
        step()
      }
    }

    def hasNext = !queue.isEmpty

    def step() = {
      val (p, s) = remove()

      p.chain(this, s) { res =>
        popped.get(s) match {
          case Some(parsers) =>
            if (!(parsers contains p))
              popped(s) += (p -> new mutable.HashSet[Success[Any]])
          case None =>
            popped += (s -> HOMap[Parser, SSet]())
            popped(s) += (p -> new mutable.HashSet[Success[Any]])
        }

        res match {
          case succ: Success[Any] => {
            popped(s)(p) += succ
            tracef("Saved: %s *=> %s%n", (p, s), succ)
          }

          case _: Failure => ()
        }

        saved.get(res) match {
          case Some(set) =>
            for (f <- backlinks(s)(p)) {
              if (!set.contains(f)) {
                set += f
                f(res)
              }
            }

          case None =>
            val set = new mutable.HashSet[Result[Any] => Unit]
            saved += (res -> set)

            for (f <- backlinks(s)(p)) {
              set += f
              f(res)
            }
        }
      }
    }

    def add[A](p: Parser[A], s: LineStream)(f: Result[A] => Unit) = {
      val tuple = (p, s)

      backlinks.get(s) match {
        case Some(parsers) =>
          if (!(parsers contains p))
            backlinks(s) += (p -> new mutable.HashSet[Result[Any] => Unit])
        case None =>
          backlinks += (s -> HOMap[Parser, FSet]())
          backlinks(s) += (p -> new mutable.HashSet[Result[Any] => Unit])
      }

      backlinks(s)(p) += f


      popped.get(s) match {
        case Some(parsers) if (parsers contains p) =>
            for (res <- parsers(p)) {           // if we've already done that, use the result
              tracef("Revisited: %s *=> %s%n", tuple, res)
              f(res)
            }
        case _ =>
          done.get(s) match {
            case Some(parsers) =>
              if (!(parsers contains p))
                addTuple(parsers)
            case None =>
              val parsers = new mutable.HashSet[Parser[Any]]
              done += (s -> parsers)
              addTuple(parsers)
          }

          def addTuple(parsers: mutable.Set[Parser[Any]]) = {
            queue ::= tuple
            parsers += p

            trace("Added: " + tuple)
          }
      }
    }

    private def remove() = {
      val tuple = queue.head
      queue = queue.tail

      trace("Removed: " + tuple)

      tuple
    }
  }
}

// trivial companion object
object Parsers extends Parsers
