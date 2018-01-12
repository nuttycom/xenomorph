package xenomorph.scalacheck

import xenomorph.Schema.Schema
import xenomorph.json.JType.JSchema
import xenomorph.json._

import scalaz.~>

object Implicits {

  implicit val toGen: ToGen[JSchema] = new ToGen[JSchema] { self =>
    import org.scalacheck.Arbitrary._
    import org.scalacheck.Gen
    import org.scalacheck.Gen._
    def toGen = new (JSchema ~> Gen) {
      def apply[A](s: JSchema[A]): Gen[A] = s.unmutu match {
        case JNullT()   => arbitrary[Unit]
        case JBoolT()   => arbitrary[Boolean]
        case JByteT()   => arbitrary[Byte]
        case JShortT()  => arbitrary[Short]
        case JIntT()    => arbitrary[Int]
        case JLongT()   => arbitrary[Long]
        case JFloatT()  => arbitrary[Float]
        case JDoubleT() => arbitrary[Double]
        case JCharT()   => arbitrary[Char]
        case JStrT()    => arbitrary[String]
        case arr: JArrayT[Schema[JSchema, ?], i] =>
          val baseDecoder: Gen[i] = ToGen.schemaToGen[JSchema](self).toGen(arr.elem)
          containerOf[Vector, i](baseDecoder)
      }
    }
  }

}
