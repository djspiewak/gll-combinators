package com.codecommit.gll.ast

trait Node {
  val label: Symbol
  
  def children: List[Node]
}

trait BinaryNode extends Node {
  def left: Node
  def right: Node
  
  def children = List(left, right)
}

trait UnaryNode extends Node {
  val isPrefix: Boolean
  
  def child: Node
  
  def children = List(child)
}

trait LeafNode extends Node {
  val children = Nil
}
