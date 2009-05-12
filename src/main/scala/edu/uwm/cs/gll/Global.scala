package edu.uwm.cs.gll

private[gll] object Global {
  val debug = false

  def trace(msg: =>String) {
    if (debug) println(msg)
  }

  def tracef(msg: =>String, args: Any*) {
    if (debug) printf(msg, args:_*)
  }
}
