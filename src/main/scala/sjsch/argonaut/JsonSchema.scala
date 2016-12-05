package sjsch.argonaut

import sjsch._
import sjsch.Schema._
import _root_.argonaut._
import Argonaut._

object JsonSchema {
  sealed trait JType[A]
  case object JNullT   extends JType[Unit]
  case object JBoolT   extends JType[Boolean]

  //case object JByteT   extends JType[Byte]
  case object JShortT  extends JType[Short]
  case object JIntT    extends JType[Int]
  case object JLongT   extends JType[Long]

  case object JFloatT  extends JType[Float]
  case object JDoubleT extends JType[Double]

  case object JCharT   extends JType[Char]
  case object JStrT    extends JType[String]
  //case class JConstT[A](a: A) extends JType[A]

  def jNull[E] =   prim[E, JType, Unit](JNullT)
  def jBool[E] =   prim[E, JType, Boolean](JBoolT)
  def jShort[E] =  prim[E, JType, Short](JShortT)
  def jInt[E] =    prim[E, JType, Int](JIntT)
  def jLong[E] =   prim[E, JType, Long](JLongT)
  def jFloat[E] =  prim[E, JType, Float](JFloatT)
  def jDouble[E] = prim[E, JType, Double](JDoubleT)
  def jChar[E] =   prim[E, JType, Char](JCharT)
  def jStr[E] =    prim[E, JType, String](JStrT)

  def toDecodeJson[A](t: JType[A]): DecodeJson[A] = t match {
    case JNullT  => implicitly[DecodeJson[Unit]]
    case JBoolT  => implicitly[DecodeJson[Boolean]]
    case JShortT => implicitly[DecodeJson[Short]]
    case JIntT   => implicitly[DecodeJson[Int]]
    case JLongT  => implicitly[DecodeJson[Long]]
    case JFloatT => implicitly[DecodeJson[Float]]
    case JDoubleT => implicitly[DecodeJson[Double]]
    case JCharT  => implicitly[DecodeJson[Char]]
    case JStrT   => implicitly[DecodeJson[String]]
  }

  implicit class SchemaCodec[E, A](s: Schema[E, JType, A]) {
    def toDecodeJson(f: E => String): DecodeJson[A] = {
      s.tail match {
        case PrimitiveSchema(jtype) => JsonSchema.toDecodeJson(jtype)
        case _ => ???
      }
    }
  }
}

