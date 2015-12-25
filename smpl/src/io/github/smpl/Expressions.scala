package io.github.smpl

import scala.reflect._
import scala.collection.mutable.MutableList

object E {
  implicit def toCInt(value: Int) = {new CInt(value)}
  implicit def toCDouble(value: Double) = {new CDouble(value)}
  implicit def toCBool(value: Boolean) = {new CBool(value)}
}

trait E {
  override def toString = getString(null)
  def getString(container: Any) : String = "undefined"
}

trait EDouble extends E {
  def +(that: EDouble) = new EDoubleSum(this,that)
  def *(that: EDouble) = new EDoubleProd(this,that) 
  def /(that: EDouble) = new EDoubleDiv(this,that)
  def <(that: EDouble) = new EBoolCompareLess(this,that)
  def <=(that: EDouble) = new EBoolCompareLessEqual(this,that)
  def >(that: EDouble) = new EBoolCompareGreater(this,that)
  def >=(that: EDouble) = new EBoolCompareGreaterEqual(this,that)
  def ===(that: EDouble) = new EBoolCompareEqual(this,that)
  def !==(that: EDouble) = new EBoolCompareNotEqual(this,that)
}

trait EInt extends EDouble {
  def +(that: EInt) = new EIntSum(this,that)
  def *(that: EInt) = new EIntProd(this,that)
  def /(that: EInt) = new EIntDiv(this,that) 
  def %(that: EInt) = new EIntMod(this,that)
}

trait EBool extends EInt {
  def *(that: EBool) = new EBoolProd(this,that) 
  def &&(that: EBool) = new EBoolAnd(this,that)
  def ||(that: EBool) = new EBoolOr(this,that)
  def ->(that: EBool) = new EBoolImplies(this,that)
  def unary_!() = new EBoolNot(this)
}

class IDiv[A <: EDouble](dividend: A, divisor: A) {
  def getDividend : A = dividend
  def getDivisor : A = divisor
  override def toString = dividend.toString + " / " + divisor.toString
}

class IMod[A <: EInt](lhs: A, rhs: A) {
  override def toString = lhs.toString + " % " + rhs.toString
}

class INot[A <: EBool](expr: A){
  override def toString = "!"+expr.toString
}

class ICompare[A <: EDouble](lhs: A, rhs: A) {
  def getLhs : A = lhs
  def getRhs : A = rhs
}
class ICompareLess[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " < " + rhs.toString
}
class ICompareLessEqual[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " <= " + rhs.toString
}
class ICompareGreater[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " > " + rhs.toString
}
class ICompareGreaterEqual[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " >= " + rhs.toString
}
class ICompareEqual[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " == " + rhs.toString
}
class ICompareNotEqual[A <: EDouble](lhs: A, rhs: A) extends ICompare[A](lhs,rhs) {
  override def toString = lhs.toString + " != " + rhs.toString
}

class IList[A](list: List[A]) { def getList() = list }
class IBinaryOperator[A <: EDouble](a: A, b: A) extends E { def getA() = a; def getB() = b}

object IList {
   def toList[A <: EDouble,B <: IBinaryOperator[A] : ClassTag](a: A) : MutableList[A] = {
     val list = new MutableList[A]
     def flatten(element : A, list: MutableList[A]) {
       element match {
         case e : B => flatten(e.getA(),list); flatten(e.getB(), list)
         case default => list += element
       }
     }
     flatten(a,list)
     list
   }
   def getStringForBinaryOperator[A <: EDouble,B <: IBinaryOperator[A] : ClassTag]
     (container : Any, c : B, separator : String) = {
     container match {
       case e : B => f"${c.getA().getString(c)} $separator ${c.getB().getString(c)}"
       case default => f"(${c.getA().getString(c)} $separator ${c.getB().getString(c)})"
     }
   }
}

case class ISum[A <: EDouble](a: A, b: A) extends IBinaryOperator(a,b) {
  override def getString(container: Any) = IList.getStringForBinaryOperator[A,ISum[A]](container, this, "+")
}
case class IProd[A <: EDouble](a: A, b: A) extends IBinaryOperator(a,b) {
  override def getString(container: Any) = IList.getStringForBinaryOperator[A,IProd[A]](container, this, "*")
}

case class IOr[A <: EBool](a: A, b: A) extends IBinaryOperator(a,b) {
  override def getString(container: Any) = IList.getStringForBinaryOperator[A,IOr[A]](container, this, "||")
}

case class IAnd[A <: EBool](a: A, b: A) extends IBinaryOperator(a,b) {
  override def getString(container: Any) = IList.getStringForBinaryOperator[A,IAnd[A]](container, this, "&&")
}

class Variable[A](name: String, var value: A) extends E {
  override def getString(container: Any) = name
  def getName = name
  def getValue: A = value
  def setValue(v : A) { value = v }
}

class Constant[A](value: A) extends E {
  override def getString(container: Any) = value.toString()
}

abstract class VariableDomain[A](name: String, domain: List[_], var map: Map[List[_],A]) {
  def apply(elements: Any*) = {
    val key = elements.toList
    if(!map.contains(key)){
      map = map + (key -> newVar(name+key.mkString("(",",",")")))
    }
    map(key)
  }
  protected def newVar(name: String) : A 
}
class VBoolDomain(name: String, domain: List[_]) extends VariableDomain[VBool](name,domain, Map[List[_],VBool]()) {
  def newVar(name: String) : VBool = new VBool(name)
}

case class VInt(name: String) extends Variable[Int](name, 0) with EInt {}
case class VDouble(name: String) extends Variable[Double](name, 0.0) with EDouble {}
case class VBool(name: String) extends Variable[Boolean](name, false) with EBool {}

case class CInt(value: Int) extends Constant[Int](value) with EInt {}
case class CDouble(value: Double) extends Constant[Double](value) with EDouble {}
case class CBool(value: Boolean) extends Constant[Boolean](value) with EBool {}

class EDoubleSum(a: EDouble, b: EDouble) extends ISum[EDouble](a,b) with EDouble {}
class EDoubleProd(a: EDouble, b: EDouble) extends IProd[EDouble](a,b) with EDouble {}
class EDoubleDiv(dividend: EDouble, divisor: EDouble) extends IDiv[EDouble](dividend, divisor) with EDouble {}

class EIntSum (a: EInt, b: EInt) extends ISum[EInt](a,b) with EInt {}
class EIntProd (a: EInt, b: EInt) extends IProd[EInt](a,b) with EInt {}
case class EIntDiv(dividend: EInt, divisor: EInt) extends IDiv[EInt](dividend, divisor) with EInt {}
case class EIntMod(lhs: EInt, rhs: EInt) extends IMod[EInt](lhs,rhs) with EInt {}

class EBoolProd(a: EBool, b: EBool) extends IProd[EBool](a,b) with EBool {}
class EBoolOr(a: EBool, b: EBool) extends IOr[EBool](a,b) with EBool {}
class EBoolAnd(a: EBool, b: EBool) extends IAnd[EBool](a,b) with EBool {}
case class EBoolImplies(lhs: EBool, rhs: EBool) extends EBool {}
case class EBoolNot(expr: EBool) extends INot(expr) with EBool {}
class EBoolCompareLess(lhs: EDouble, rhs: EDouble) extends ICompareLess(lhs,rhs) with EBool {}
class EBoolCompareLessEqual(lhs: EDouble, rhs: EDouble) extends ICompareLessEqual(lhs,rhs) with EBool {}
class EBoolCompareGreater(lhs: EDouble, rhs: EDouble) extends ICompareGreater(lhs,rhs) with EBool {}
class EBoolCompareGreaterEqual(lhs: EDouble, rhs: EDouble) extends ICompareGreaterEqual(lhs,rhs) with EBool {}
class EBoolCompareEqual(lhs: EDouble, rhs: EDouble) extends ICompareEqual(lhs,rhs) with EBool {}
class EBoolCompareNotEqual(lhs: EDouble, rhs: EDouble) extends ICompareNotEqual(lhs,rhs) with EBool {}


