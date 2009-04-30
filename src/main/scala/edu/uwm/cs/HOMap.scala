package edu.uwm.cs

import scala.collection._

/**
 * @author Jorge Ortiz
 */
private[cs] class HOMap[K[_], V[_]] {
  private val underlying: mutable.Map[K[_], V[_]] = mutable.Map.empty
  
  def add[T](key: K[T], value: V[T]) {
    underlying(key) = value
  }
  
  def remove[T](key: K[T]) {
    underlying -= key
  }
  
  def get[T](key: K[T]): Option[V[T]] = underlying.get(key).asInstanceOf[Option[V[T]]]
  
  def apply[T](key: K[T]) = get(key) getOrElse { throw new IllegalArgumentException("No value for specified key") }
}

