package edu.uwm.cs.util

case object UniversalCharSet extends Set[Char] {
  val size = Math.MAX_INT     // should be infinite
  
  def contains(e: Char) = true
  
  override def exists(f: Char=>Boolean) = true
  
  override def forall(f: Char=>Boolean) = false
  
  def **(that: Set[Char]) = that
  
  def +(e: Char) = UniversalCharSet
  
  def -(e: Char) = throw new AssertionError("Cannot remove anything from the universal set")
  
  override def ++(that: Iterable[Char]) = UniversalCharSet
  
  def subsetOf(that: Set[Char]) = false
  
  def empty[A] = Set[A]()
  
  def elements = throw new AssertionError("Cannot enumerate the universal set")
  
  override def toString = "UniversalCharSet"
  
  override def equals(other: Any) = other match {
    case that: AnyRef => this eq that
    case _ => false
  }
  
  override def hashCode = 0   // why not?
}
