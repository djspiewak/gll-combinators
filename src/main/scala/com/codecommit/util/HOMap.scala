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

package com.codecommit.util

import collection.mutable

/**
 * @author Jorge Ortiz
 */
private[codecommit] class HOMap[K[_], V[_]] {
  private val underlying: mutable.Map[K[_], V[_]] = mutable.Map.empty

  def add[T](key: K[T], value: V[T]): Unit =
    underlying(key) = value

  def remove[T](key: K[T]): Unit =
    underlying -= key

  def get[T](key: K[T]): Option[V[T]] = underlying.get(key).asInstanceOf[Option[V[T]]]

  def apply[T](key: K[T]) = get(key) getOrElse { throw new IllegalArgumentException("No value for specified key") }

  def update[T](key: K[T], value: V[T]): Unit =
    underlying(key) = value

  def +=[T](tuple: (K[T], V[T])): Unit =
    underlying += tuple

  def contains(key: K[_]) = underlying.contains(key)
}

private[codecommit] object HOMap {
  def apply[K[_], V[_]](tuples: (K[A], V[A]) forSome { type A } *) = {
    val back = new HOMap[K, V]
    tuples foreach { case (k, v) => back.add(k, v) }
    back
  }
}
