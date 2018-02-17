package com.codecommit.gll.ast

trait Node {
  def form: FormSpec
  
  def children: List[Node]
  
  protected implicit def symToFormSpec(sym: Symbol): FormSpec =
    FormSpec.NamePart(sym)
  
  protected implicit def nodeToFormSpec(node: Node): FormSpec =
    FormSpec.HolePart(node)
}

trait UnaryNode extends Node {
  def isPrefix: Boolean
  def child: Node
  
  def sym: Symbol
  
  def form = if (isPrefix) sym ~ child else child ~ sym
  def children = child :: Nil
}

trait BinaryNode extends Node {
  def assocLeft: Boolean
  def assocRight: Boolean = !assocLeft
  
  def left: Node
  def right: Node
  
  def sym: Symbol
  
  def form = left ~ sym ~ right
  
  def children = {
    if (assocLeft && assocRight)
      Nil
    else if (assocLeft)
      left :: right :: Nil
    else
      right :: left :: Nil
  }
}

trait LeafNode extends Node {
  def form = 'leaf
  def children = Nil
}

sealed trait FormSpec {
  def ~(that: FormSpec): FormSpec = FormSpec.Sequence(this, that)
  
  def isSimple: Boolean
  def linearize: Vector[FormSpec]
}

object FormSpec {
  case class NamePart(sym: Symbol) extends FormSpec {
    val isSimple = true
    def linearize = Vector(this)
  }
  
  case class HolePart(child: Node) extends FormSpec {
    val isSimple = false
    def linearize = Vector(this)
  }
  
  case class Sequence(left: FormSpec, right: FormSpec) extends FormSpec {
    def isSimple = left.isSimple && right.isSimple
    def linearize = left.linearize ++ right.linearize
  }
}
