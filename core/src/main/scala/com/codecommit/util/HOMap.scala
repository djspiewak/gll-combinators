package com.codecommit.util

import collection.mutable

/**
 * @author Jorge Ortiz
 */
private[codecommit] class HOMap[K[_], V[_]] {
  private val underlying: mutable.Map[K[_], V[_]] = mutable.Map.empty
  
  def add[T](key: K[T], value: V[T]) {
    underlying(key) = value
  }
  
  def remove[T](key: K[T]) {
    underlying -= key
  }
  
  def get[T](key: K[T]): Option[V[T]] = underlying.get(key).asInstanceOf[Option[V[T]]]
  
  def apply[T](key: K[T]) = get(key) getOrElse { throw new IllegalArgumentException("No value for specified key") }
  
  def update[T](key: K[T], value: V[T]) {
    underlying(key) = value
  }
  
  def +=[T](tuple: (K[T], V[T])) {
    underlying += tuple
  }
  
  
  def contains(key: K[_]) = underlying.contains(key)
}

private[codecommit] object HOMap {
  def apply[K[_], V[_]](tuples: (K[A], V[A]) forSome { type A } *) = {
    val back = new HOMap[K, V]
    tuples foreach { case (k, v) => back.add(k, v) } 
    back
  }
}