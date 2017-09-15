/*
 * Copyright (C) 2017 Kris Nuttycombe
 * All rights reserved.
 *
 * This file is part of the Scala Xenomorph library.
 *
 * GNU Lesser General Public License Usage
 * This file may be used under the terms of the GNU Lesser
 * General Public License version 3.0 as published by the Free Software
 * Foundation and appearing in the file LICENSE included in the
 * packaging of this file.  Please review the following information to
 * ensure the GNU Lesser General Public License version 3.0 requirements
 * will be met: https://www.gnu.org/licenses/lgpl-3.0.txt
 */
package xenomorph.json

import scalaz.~>

import argonaut.Json
import argonaut.DecodeJson
import argonaut.DecodeJson._

import xenomorph.HMutu
import xenomorph.Schema._
import xenomorph.scalacheck.ToGen
import xenomorph.scalacheck.ToGen._


sealed trait JType[F[_], I]

case class JNullT[F[_]]()   extends JType[F, Unit]
case class JBoolT[F[_]]()   extends JType[F, Boolean]

case class JByteT[F[_]]()   extends JType[F, Byte]
case class JShortT[F[_]]()  extends JType[F, Short]
case class JIntT[F[_]]()    extends JType[F, Int]
case class JLongT[F[_]]()   extends JType[F, Long]

case class JFloatT[F[_]]()  extends JType[F, Float]
case class JDoubleT[F[_]]() extends JType[F, Double]

case class JCharT[F[_]]()   extends JType[F, Char]
case class JStrT[F[_]]()    extends JType[F, String]

case class JArrayT[F[_], I](elem: F[I]) extends JType[F, Vector[I]]

object JType {
  type JSchema[I] = HMutu[JType, Schema, I]

  val jNull   = prim(HMutu[JType, Schema, Unit](JNullT()))
  val jBool   = prim(HMutu[JType, Schema, Boolean](JBoolT()))
  val jShort  = prim(HMutu[JType, Schema, Short](JShortT()))
  val jInt    = prim(HMutu[JType, Schema, Int](JIntT()))
  val jLong   = prim(HMutu[JType, Schema, Long](JLongT()))
  val jFloat  = prim(HMutu[JType, Schema, Float](JFloatT()))
  val jDouble = prim(HMutu[JType, Schema, Double](JDoubleT()))
  val jChar   = prim(HMutu[JType, Schema, Char](JCharT()))
  val jStr    = prim(HMutu[JType, Schema, String](JStrT()))
  def jArray[I](elem: Schema[JSchema, I]) = prim(HMutu[JType, Schema, Vector[I]](JArrayT(elem)))

  implicit val toJson: ToJson[JSchema] = new ToJson[JSchema] { self => 
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

  implicit val fromJson: FromJson[JSchema] = new FromJson[JSchema] { self => 
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

  implicit val toGen: ToGen[JSchema] = new ToGen[JSchema] {
    import org.scalacheck.Gen
    import org.scalacheck.Gen._
    import org.scalacheck.Arbitrary._
    def toGen = new (JSchema ~> Gen) {
      def apply[A](s: JSchema[A]): Gen[A] = s.unmutu match {
        case JNullT()    => arbitrary[Unit]
        case JBoolT()    => arbitrary[Boolean]
        case JByteT()    => arbitrary[Byte]
        case JShortT()   => arbitrary[Short]
        case JIntT()     => arbitrary[Int]
        case JLongT()    => arbitrary[Long]
        case JFloatT()   => arbitrary[Float]
        case JDoubleT()  => arbitrary[Double]
        case JCharT()    => arbitrary[Char]
        case JStrT()     => arbitrary[String]
        case arr: JArrayT[Schema[JSchema, ?], i] => 
          containerOf[Vector, i](arr.elem.toGen)
      }
    }
  }
}
