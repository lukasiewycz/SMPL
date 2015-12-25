package io.github.smpl

import io.github.smpl.E._
import io.github.smpl.z3.Z3Solver

object MyMain extends App {
  println("hello")
  

  val i0 = new VInt("i0")
  val i1 = new VInt("i1")
  val i2 = new VInt("i2")
  val i3 = new CInt(444)
  val b0 = new VBool("b0")
  val b1 = new VBool("b1")
  val b2 = new VBool("b2")
  val d0 = new VDouble("d0")
  val d1 = new VDouble("d1")
  val d2 = new VDouble("d2")
  
  val sum = i0 + i1 + i0 + 555 + 688 + i2 + 0.3
  println(sum)
  
  println ((i0 + i1).getClass+" "+(i0 + i1))
  println ((i0 + d1).getClass+" "+(i0 + d1))
  println ((d1 + d1).getClass+" "+(d1 + d1))
  println ((i0 / d1).getClass+" "+(i0 / d1))
  println ((i0 / i1).getClass+" "+(i0 / i1))
  
  println (i0 + i1 + i3 +i2)
  println (i0 + (i1 + i2) + 444)
  println ((i0 + i1 + d0 + d1 + i0 + i1))
  println ((i3 + i0 + (i1 * i0 % i0) % i1 + (i0 * (i1 * d0))))
  println((i0 * i1).getClass)
  println((i0 * d0).getClass)
  
  println((10 + 10 + (b0 / b0) + (b0 && true && b1 || b2 && false)))
  println((b1 * b0).getClass)
  println((i0 * i1 + i2))
  println(((i0 + i1) * i2))
  println((i0 * i1 * i2))
  println((i0 + i1 * i2) >= d0)
  
  val bools = new VBoolDomain("b",List(0 to 9, 0 to 25))
  
  val solver = new Z3Solver
  val constraints : List[EBool] = List(
      (i0 >= 0) && (i0 <= 25),
      i1 === (i0 + 1) % 26,
      b0 -> (i2 === (i1 + 1) % 26),
      !b0 -> (i2 === i1),
      i0 === 24,
      bools(1,2) + bools(1,2) >= 2
      
  )
  solver.solve(constraints)
      
}