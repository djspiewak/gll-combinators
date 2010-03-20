package edu.uwm.cs.util

class ComplementarySet[A](private val without: Set[A]) extends collection.immutable.Set[A] {
  override val size = Math.MAX_INT     // should be infinite
  
  def this() = this(Set())
  
  def contains(e: A) = !without.contains(e)
  
  def iterator = throw new AssertionError("Cannot iterate over a set complement")
  
  override def exists(f: A=>Boolean) = !without.exists(f)
  
  override def forall(f: A=>Boolean) = false
  
  def **(that: Set[A]) = new ComplementarySet(that -- without)
  
  def +(e: A) = {
    if (without contains e)
      new ComplementarySet(without - e)
    else
      this
  }
  
  def -(e: A) = new ComplementarySet(without + e)
  
  def ++(other: Iterable[A]) = other match {
    case that: ComplementarySet[A] => new ComplementarySet(this.without ** that.without)
    
    case _ => without -- other
  }
  
  def --(other: Iterable[A]) = other match {
    case that: ComplementarySet[A] => new ComplementarySet(this.without ++ that.without)
    
    case _ => without ++ other
  }
  
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
