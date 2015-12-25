package io.github.smpl.z3

import scala.collection.JavaConversions.mapAsJavaMap
import com.microsoft.z3.ArithExpr
import com.microsoft.z3.BoolExpr
import com.microsoft.z3.Context
import com.microsoft.z3.Expr
import com.microsoft.z3.IntNum
import com.microsoft.z3._
import io.github.smpl.CBool
import io.github.smpl.CDouble
import io.github.smpl.CInt
import io.github.smpl.E
import io.github.smpl.EBool
import io.github.smpl.EBoolAnd
import io.github.smpl.EBoolCompareEqual
import io.github.smpl.EBoolCompareGreater
import io.github.smpl.EBoolCompareGreaterEqual
import io.github.smpl.EBoolCompareLess
import io.github.smpl.EBoolCompareLessEqual
import io.github.smpl.EBoolCompareNotEqual
import io.github.smpl.EBoolOr
import io.github.smpl.EBoolProd
import io.github.smpl.EDoubleProd
import io.github.smpl.EDoubleSum
import io.github.smpl.EIntDiv
import io.github.smpl.EIntProd
import io.github.smpl.EIntSum
import io.github.smpl.VBool
import io.github.smpl.VDouble
import io.github.smpl.VInt
import io.github.smpl.Variable
import io.github.smpl.EDoubleDiv
import io.github.smpl.ISum
import io.github.smpl.IProd
import io.github.smpl.IDiv
import io.github.smpl.Variable
import io.github.smpl.EIntMod
import io.github.smpl.EBoolImplies
import io.github.smpl.EBoolNot

object Z3Solver {
  implicit def toArithExpr(expr: Expr) : ArithExpr = { expr.asInstanceOf[ArithExpr] }
}

class Z3Solver {
  
  var ctx : Context = null
  var solver : Solver = null
  var variables = Map[Variable[_],Expr]()
  
  def solve(constraints: Iterable[EBool]){
    ctx = new Context(mapAsJavaMap(Map("model" -> "true")));
    solver = ctx.mkSolver
    
    for(expr <- constraints) {
      solver.add(toNative(expr).asInstanceOf[BoolExpr])
    }
    
    val result = solver.check
    println(result)
    
    variables.foreach {case(key, value) => 
      key match {
        case key: VBool => key.setValue(solver.getModel.eval(value, false).asInstanceOf[BoolExpr].isTrue())
        case key: VInt => key.setValue(solver.getModel.eval(value, false).asInstanceOf[IntNum].getInt)
      }
    }
    
    variables.foreach {case(key, value) => 
      println(key.getName+" = "+key.value)
    }
  }
  
  def toNative(expr: E) : Expr = {
    expr match {
      case expr: EBoolCompareLess => ctx.mkLe(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs)))
      case expr: EBoolCompareLessEqual => ctx.mkLt(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs)))
      case expr: EBoolCompareGreater => ctx.mkGt(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs)))
      case expr: EBoolCompareGreaterEqual => ctx.mkGe(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs)))
      case expr: EBoolCompareEqual => ctx.mkEq(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs)))
      case expr: EBoolCompareNotEqual => ctx.mkNot(ctx.mkEq(arith(toNative(expr.getLhs)), arith(toNative(expr.getRhs))))
      case expr: CBool => ctx.mkBool(expr.value)
      case expr: CInt => ctx.mkInt(expr.value)
      case expr: CDouble => ctx.mkReal(0, 0) // TODO
      case expr: Variable[_] => variable(expr)
      case expr: ISum[_] => ctx.mkAdd(arith(for(e <- expr.list) yield toNative(e)):_*)
      case expr: EBoolProd => ctx.mkAnd(bool(for(e <- expr.list) yield toNative(e)):_*)
      case expr: IProd[_] => ctx.mkMul(arith(for(e <- expr.list) yield toNative(e)):_*)
      case expr: IDiv[_] => 
        val divisorArithExpr = arith(toNative(expr.getDivisor))
        solver.add(ctx.mkNot(ctx.mkEq(divisorArithExpr, ctx.mkInt(0))))
        ctx.mkDiv(arith(toNative(expr.getDividend)), divisorArithExpr)
      case expr: EBoolAnd => ctx.mkAnd(bool(for(e <- expr.list) yield toNative(e)):_*)
      case expr: EBoolOr => ctx.mkOr(bool(for(e <- expr.list) yield toNative(e)):_*)
      case expr: EIntMod => ctx.mkMod(arith(toNative(expr.lhs)).asInstanceOf[IntExpr], 
                                      arith(toNative(expr.rhs)).asInstanceOf[IntExpr])
      case expr: EBoolImplies => ctx.mkImplies(bool(toNative(expr.lhs)), 
                                               bool(toNative(expr.rhs)))
      case expr: EBoolNot => ctx.mkNot(bool(toNative(expr.expr)))
    }
  }
  
  def bool(expr: Expr) : BoolExpr = expr.asInstanceOf[BoolExpr]
  def bool(expr: List[Expr]) : List[BoolExpr] = expr.asInstanceOf[List[BoolExpr]]
  def arith(expr: Expr) : ArithExpr = { 
    expr match {
      case expr: BoolExpr => ctx.mkITE(expr, ctx.mkInt(1), ctx.mkInt(0)).asInstanceOf[ArithExpr]
      case expr: ArithExpr => expr
    }
  }
  def arith(expr: List[Expr]) : List[ArithExpr] = { for(e <- expr) yield arith(e) }
  def variable(variable: Variable[_]) : Expr = {
    if(!variables.contains(variable)){
      val expr = variable match {
        case variable: VBool => ctx.mkBoolConst(variable.getName)
        case variable: VInt => ctx.mkIntConst(variable.getName)
        case variable: VDouble => ctx.mkRealConst(variable.getName)
      }
      variables = variables + (variable -> expr);
    }
    variables(variable)
  }
  
}