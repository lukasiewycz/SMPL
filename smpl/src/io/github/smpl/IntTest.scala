package io.github.smpl

import io.github.smpl.E._
import io.github.smpl.z3.Z3Solver
import java.util.ArrayList
import scala.collection.mutable.MutableList

object IntTest extends App {

  val i0 = new VInt("i0")
  val i1 = new VInt("i1")
  val i2 = new VInt("i2")
  val i3 = new VInt("i3")
  val b0 = new VBool("b0")
  val b1 = new VBool("b1")


  println(i0 * i1 * i2 * i3)
  println(i0 + i1 * i2 + i3)
  
  val solver = new Z3Solver
  val constraints : List[EBool] = List(
      i1 * i2 * b1 === i3 + i0,
      i0 !== i3,
      i1 > 2,
      i2 > 3,
      i3 > 0,
      i0 > 0,
      b0 + b1 > 0
  )
  solver.solve(constraints)
      
}