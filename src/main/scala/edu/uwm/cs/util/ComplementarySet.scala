package edu.uwm.cs.util

import collection.SetLike
import collection.generic.CanBuildFrom

class ComplementarySet[A](private val without: Set[A]) extends Set[A] with SetLike[A, ComplementarySet[A]] {
  override val size = Math.MAX_INT     // should be infinite
  
  def this() = this(Set())
  
  override def empty = new ComplementarySet[A](Set())
  
  def contains(e: A) = !without.contains(e)
  
  def iterator = throw new AssertionError("Cannot iterate over a set complement")
  
  override def exists(f: A=>Boolean) = !without.exists(f)
  
  override def forall(f: A=>Boolean) = false
  
  def &(that: Set[A]): ComplementarySet[A] = this ** that
  
  def **(that: Set[A]): ComplementarySet[A] = new ComplementarySet(that -- without)
  
  def +(e: A) = {
    if (without contains e)
      new ComplementarySet(without - e)
    else
      this
  }
  
  def -(e: A) = new ComplementarySet(without + e)
  
  override def ++(other: Iterator[A]) = new ComplementarySet(without -- other)
  
  override def ++(other: Traversable[A]) = other match {
    case that: ComplementarySet[A] => new ComplementarySet(this.without ** that.without)
    
    case _ => new ComplementarySet(without -- other)
  }
  
  override def --(other: Iterator[A]) = new ComplementarySet(without ++ other)
  
  override def --(other: Traversable[A]) = other match {
    case that: ComplementarySet[A] => new ComplementarySet(this.without ++ that.without)
    
    case _ => new ComplementarySet(without ++ other)
  }
  
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[ComplementarySet[A], B, That]): That = 
    new ComplementarySet(without map f).asInstanceOf[That]
  
  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[ComplementarySet[A], B, That]): That =
    new ComplementarySet(without flatMap f).asInstanceOf[That]
  
  def subsetOf(other: Set[A]) = other match {
    case that: ComplementarySet[A] => that.without subsetOf this.without
    case _ => false
  }
  
  def empty[A] = Set[A]()
  
  override def elements = throw new AssertionError("Cannot enumerate the complementary set")
  
  override def toString = "ComplementarySet(%s)".format(without)
  
  override def equals(other: Any) = other match {
    case that: ComplementarySet[A] => this.without == that.without
    case _ => false
  }
  
  override def hashCode = ~(without.hashCode)
}

case object UniversalCharSet extends ComplementarySet[Char]

case object UniversalOptCharSet extends ComplementarySet[Option[Char]]
