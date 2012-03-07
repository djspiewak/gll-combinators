package com.codecommit.util

trait Thunkable {
  protected def thunk[A](field: Symbol) = {
    val ref = getClass.getDeclaredField(field.toString substring 1)
    ref.setAccessible(true)     // safe, because it's just us
    ref.get(this).asInstanceOf[() => A]
  }
}
