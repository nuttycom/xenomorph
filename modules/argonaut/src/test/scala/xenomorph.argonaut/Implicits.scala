package xenomorph.argonaut

import argonaut.DecodeJson._
import argonaut.{DecodeJson, Json}
import xenomorph.Schema.Schema
import xenomorph.json.JType.JSchema
import xenomorph.json._

import scalaz.~>

object Implicits {

  implicit val primToJson: ToJson[JSchema] = new ToJson[JSchema] { self =>
    val serialize = new (JSchema ~> (? => Json)) {
      def apply[I](s: JSchema[I]): I => Json = s.unmutu match {
        case JNullT()    => (_: I) => Json.jNull
        case JBoolT()    => Json.jBool(_)
        case JByteT()    => i => Json.jNumber(i.toInt)
        case JShortT()   => i => Json.jNumber(i.toInt)
        case JIntT()     => Json.jNumber(_)
        case JLongT()    => Json.jNumber(_)
        case JFloatT()   => f => Json.jNumberOrString(f.toDouble)
        case JDoubleT()  => Json.jNumberOrString(_)
        case JCharT()    => c => Json.jString(c.toString)
        case JStrT()     => Json.jString(_)
        case JArrayT(elem) =>
          xs => Json.jArray(xs.map(sToJ.serialize(elem)).toList)
      }
    }

    val sToJ: ToJson[Schema[JSchema, ?]] = ToJson.schemaToJson(self)
  }

  implicit val primFromJson: FromJson[JSchema] = new FromJson[JSchema] { self =>
    val decoder = new (JSchema ~> DecodeJson) {
      def apply[I](s: JSchema[I]): DecodeJson[I] = s.unmutu match {
        case JNullT()    => UnitDecodeJson
        case JBoolT()    => BooleanDecodeJson
        case JByteT()    => IntDecodeJson.map(_.toByte)
        case JShortT()   => ShortDecodeJson
        case JIntT()     => IntDecodeJson
        case JLongT()    => LongDecodeJson
        case JFloatT()   => FloatDecodeJson
        case JDoubleT()  => DoubleDecodeJson
        case JCharT()    => CharDecodeJson
        case JStrT()     => StringDecodeJson
        case JArrayT(elem) =>
          ListDecodeJson(sFromJ.decoder(elem)).map(_.toVector)
      }
    }

    val sFromJ: FromJson[Schema[JSchema, ?]] = FromJson.schemaFromJson(self)
  }

}

