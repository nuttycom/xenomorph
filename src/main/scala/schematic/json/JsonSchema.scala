package schematic.json

import schematic.Schema._

object JsonSchema {
  sealed trait JType[A]
  case object JNullT   extends JType[Unit]
  case object JBoolT   extends JType[Boolean]

  case object JByteT   extends JType[Byte]
  case object JShortT  extends JType[Short]
  case object JIntT    extends JType[Int]
  case object JLongT   extends JType[Long]

  case object JFloatT  extends JType[Float]
  case object JDoubleT extends JType[Double]

  case object JCharT   extends JType[Char]
  case object JStrT    extends JType[String]

  case class JArrayT[A](elemSchema: Schema[JType, A]) extends JType[Vector[A]]

  def jNull =   prim[JType, Unit](JNullT)
  def jBool =   prim[JType, Boolean](JBoolT)
  def jShort =  prim[JType, Short](JShortT)
  def jInt =    prim[JType, Int](JIntT)
  def jLong =   prim[JType, Long](JLongT)
  def jFloat =  prim[JType, Float](JFloatT)
  def jDouble = prim[JType, Double](JDoubleT)
  def jChar =   prim[JType, Char](JCharT)
  def jStr =    prim[JType, String](JStrT)
  def jArray[A](elem: Schema[JType, A]) = prim[JType, Vector[A]](JArrayT(elem))
}

