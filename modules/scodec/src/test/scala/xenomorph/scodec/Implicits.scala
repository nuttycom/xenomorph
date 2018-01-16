package xenomorph.scodec

import scodec.bits.BitVector
import scodec.codecs.implicits._
import scodec.{Attempt, Codec, Decoder, Encoder, _}
import xenomorph.Schema.Schema
import xenomorph.json.JType.JSchema
import xenomorph.json._

import scalaz.~>

object Implicits {

  implicit val toEncoder: ToEncoder[JSchema] = new ToEncoder[JSchema] { self =>
    override val toEncoder = new (JSchema ~> Encoder) {
      def apply[A](s: JSchema[A]): Encoder[A] = s.unmutu match {
        case JNullT()    => Encoder(_ => Attempt.successful(BitVector.empty))
        case JBoolT()    => implicitly[Encoder[Boolean]]
        case JByteT()    => implicitly[Encoder[Byte]]
        case JShortT()   => implicitly[Encoder[Short]]
        case JIntT()     => implicitly[Encoder[Int]]
        case JLongT()    => implicitly[Encoder[Long]]
        case JFloatT()   => implicitly[Encoder[Float]]
        case JDoubleT()  => implicitly[Encoder[Double]]
        case JCharT()    => implicitly[Encoder[Byte]].xmap((_: Byte).toChar, (_: Char).toByte)
        case JStrT()     => implicitly[Encoder[String]]
        case arr: JArrayT[Schema[JSchema, ?], i] =>
          val baseEncoder: Encoder[i] = ToEncoder.schemaToEncoder[JSchema](self).toEncoder(arr.elem)
          implicit val codec = Codec[i](baseEncoder, null)
          implicitly[Encoder[Vector[i]]]
      }
    }
  }

  implicit val toDecoder: ToDecoder[JSchema] = new ToDecoder[JSchema] { self =>
    override val toDecoder = new (JSchema ~> Decoder) {
      def apply[A](s: JSchema[A]): Decoder[A] = s.unmutu match {
        case JNullT()    => Decoder.point(())
        case JBoolT()    => implicitly[Decoder[Boolean]]
        case JByteT()    => implicitly[Decoder[Byte]]
        case JShortT()   => implicitly[Decoder[Short]]
        case JIntT()     => implicitly[Decoder[Int]]
        case JLongT()    => implicitly[Decoder[Long]]
        case JFloatT()   => implicitly[Decoder[Float]]
        case JDoubleT()  => implicitly[Decoder[Double]]
        case JCharT()    => implicitly[Decoder[Byte]].xmap((_: Byte).toChar, (_: Char).toByte)
        case JStrT()     => implicitly[Decoder[String]]
        case arr: JArrayT[Schema[JSchema, ?], i] =>
          val baseDecoder: Decoder[i] = ToDecoder.schemaToDecoder[JSchema](self).toDecoder(arr.elem)
          implicit val codec = Codec[i](null, baseDecoder)
          implicitly[Codec[Vector[i]]].asDecoder
      }
    }
  }

}
