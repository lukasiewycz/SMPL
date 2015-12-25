package io.github.smpl

import scala.reflect._

object E {
  implicit def toCInt(value: Int) = {new CInt(value)}
  implicit def toCDouble(value: Double) = {new CDouble(value)}
  implicit def toCBool(value: Boolean) = {new CBool(value)}
}

trait E {}
trait EDouble extends E {
  def +(that: EDouble) = new EDoubleSum(IList.flattenOnOperator[EDouble,ISum[EDouble]](this,that))
  def *(that: EDouble) = new EDoubleProd(IList.flattenOnOperator[EDouble,IProd[EDouble]](this,that)) 
  def /(that: EDouble) = new EDoubleDiv(this,that)
  def <(that: EDouble) = new EBoolCompareLess(this,that)
  def <=(that: EDouble) = new EBoolCompareLessEqual(this,that)
  def >(that: EDouble) = new EBoolCompareGreater(this,that)
  def >=(that: EDouble) = new EBoolCompareGreaterEqual(this,that)
  def ===(that: EDouble) = new EBoolCompareEqual(this,that)
  def !==(that: EDouble) = new EBoolCompareNotEqual(this,that)
}
trait EInt extends EDouble {
  def +(that: EInt) = new EIntSum(IList.flattenOnOperator[EInt,ISum[EInt]](this,that))
  def *(that: EInt) = new EIntProd(IList.flattenOnOperator[EInt,IProd[EInt]](this,that)) 
  def /(that: EInt) = new EIntDiv(this,that) 
  def %(that: EInt) = new EIntMod(this,that)
}
trait EBool extends EInt {
  def *(that: EBool) = new EBoolProd(IList.flattenOnOperator[EBool,IProd[EBool]](this,that)) 
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
object IList {
   def  flattenOnOperator[A <: EDouble,B <: IList[A] : ClassTag](a: A, b: A) = {
     def asList(c: A) : List[A] = {
       if(classTag[B].runtimeClass isAssignableFrom c.getClass){
         c.asInstanceOf[B].getList()
       } else {
         List(c)
       }
     }
     asList(a) ::: asList(b)
   }
}

case class ISum[A <: EDouble](list: List[A]) extends IList(list) {
  override def toString = list mkString("(", " + ", ")")
}
case class IProd[A <: EDouble](list: List[A]) extends IList(list) {
  override def toString = list mkString("(", " * ", ")")
}
case class IOr[A <: EBool](a: A, b: A) {
  override def toString = "(" + a + " || " + b + ")"
}
case class IAnd[A <: EBool](a: A, b: A) {
  override def toString = "(" + a + " && " + b + ")"
}

class Variable[A](name: String, var value: A) {
  override def toString = name
  def getName = name
  def getValue: A = value
  def setValue(v : A) { value = v }
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


case class CInt(value: Integer) extends EInt {
  override def toString = value.toString
}
case class CDouble(value: Double) extends EDouble {
  override def toString = value.toString
}
case class CBool(value: Boolean) extends EBool {
  override def toString = value.toString
}

class EIntSum (list: List[EInt]) extends ISum[EInt](list) with EInt {}
class EIntProd (list: List[EInt]) extends IProd[EInt](list) with EInt {}
case class EIntDiv(dividend: EInt, divisor: EInt) extends IDiv[EInt](dividend, divisor) with EInt {}
case class EIntMod(lhs: EInt, rhs: EInt) extends IMod[EInt](lhs,rhs) with EInt {}

class EDoubleSum(list: List[EDouble]) extends ISum[EDouble](list) with EDouble {}
class EDoubleProd(list: List[EDouble]) extends IProd[EDouble](list) with EDouble {}
class EDoubleDiv(dividend: EDouble, divisor: EDouble) extends IDiv[EDouble](dividend, divisor) with EDouble {}

class EBoolProd(list: List[EBool]) extends IProd[EBool](list) with EBool {}
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


