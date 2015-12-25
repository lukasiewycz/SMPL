package io.github.smpl

import io.github.smpl.E._
import io.github.smpl.z3.Z3Solver
import java.util.ArrayList
import scala.collection.mutable.MutableList

object BooleanTest extends App {

  val b0 = new VBool("b0")
  val b1 = new VBool("b1")
  val b2 = new VBool("b2")
  val b3 = new VBool("b3")

  val or = b0 || b1 || b2 || b3
  val and = b0 && b1 && b2
  val mix = b0 || b1 && b2
  
  println(or)
  println(and)
  println(mix)
  
  println(IOr.flattenOr(or))
  
  val solver = new Z3Solver
  val constraints : List[EBool] = List(
      b0 || b1 || b2 || b3,
      b0 && b1
  )
  solver.solve(constraints)
      
}