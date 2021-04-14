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
  def form = Symbol("leaf")
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
