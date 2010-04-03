package edu.uwm.cs.util

import scala.collection.generic.CanBuildFrom

package object util {
  import edu.uwm.cs.util.ComplementarySet
  
  implicit def createComplementarySetCanBuildFrom[A, B]: CanBuildFrom[ComplementarySet[A], B, ComplementarySet[B]] = new CanBuildFrom[ComplementarySet[A], B, ComplementarySet[B]] {
    def apply = null
    
    def apply(from: ComplementarySet[A]) = null
  }
}
