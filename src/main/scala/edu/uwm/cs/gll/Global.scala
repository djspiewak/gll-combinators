package edu.uwm.cs.gll

private[gll] object Global {
  val debug = true

  def trace(msg: String) {
    if (debug) println(msg)
  }

  def tracef(msg: String, args: Any*) {
    if (debug) printf(msg, args:_*)
  }
}
