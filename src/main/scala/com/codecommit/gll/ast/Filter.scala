package com.codecommit.gll.ast

trait Filter[-A] extends (A => Boolean) { self =>
  def unary_!(): Filter[A] = new Filter[A] {
    def apply(n: A) = !self(n)
  }
  
  def &[B <: A](other: Filter[B]): Filter[B] = new Filter[B] {
    def apply(n: B) = self(n) && other(n)
  }
  
  def |[B <: A](other: Filter[B]): Filter[B] = new Filter[B] {
    def apply(n: B) = self(n) || other(n)
  }
}
