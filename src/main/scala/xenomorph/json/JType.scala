/*
 * Copyright (C) 2017 Kris Nuttycombe
 * All rights reserved.
 *
 * This file is part of the Scala Schematic library.
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

import argonaut._
import argonaut.Json._
import argonaut.DecodeJson._

import scalaz.~>
import scalaz.Applicative
import scalaz.State
import scalaz.State._
import scalaz.FreeAp
import scalaz.syntax.monad._
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._

import xenomorph._
import xenomorph.Schema._

sealed trait JType[A, I]

case class JNullT[A]()   extends JType[A, Unit]
case class JBoolT[A]()   extends JType[A, Boolean]

case class JByteT[A]()   extends JType[A, Byte]
case class JShortT[A]()  extends JType[A, Short]
case class JIntT[A]()    extends JType[A, Int]
case class JLongT[A]()   extends JType[A, Long]

case class JFloatT[A]()  extends JType[A, Float]
case class JDoubleT[A]() extends JType[A, Double]

case class JCharT[A]()   extends JType[A, Char]
case class JStrT[A]()    extends JType[A, String]

case class JArrayT[A, I](elemSchema: Schema[A, JType[A, ?], I]) extends JType[A, Vector[I]]

object JType {
  def jNull   = prim[JType[Unit, ?], Unit](JNullT())
  def jBool   = prim[JType[Unit, ?], Boolean](JBoolT())
  def jShort  = prim[JType[Unit, ?], Short](JShortT())
  def jInt    = prim[JType[Unit, ?], Int](JIntT())
  def jLong   = prim[JType[Unit, ?], Long](JLongT())
  def jFloat  = prim[JType[Unit, ?], Float](JFloatT())
  def jDouble = prim[JType[Unit, ?], Double](JDoubleT())
  def jChar   = prim[JType[Unit, ?], Char](JCharT())
  def jStr    = prim[JType[Unit, ?], String](JStrT())
  def jArray[A, I](elem: Schema[A, JType[A, ?], I]) = prim[JType[A, ?], Vector[I]](JArrayT(elem))
}

trait ToJson[S[_]] {
  def serialize[A](schema: S[A], value: A): Json
}

object ToJson {
  implicit def jSchemaToJson[A, P[_]: ToJson]: ToJson[Schema[A, P, ?]] = new ToJson[Schema[A, P, ?]] {
    def serialize[I](schema: Schema[A, P, I], value: I): Json = {
      schema.tail.value match {
        case PrimSchema(p) => implicitly[ToJson[P]].serialize(p, value)

        case OneOfSchema(alts) => 
          val results = alts flatMap {
            case Alt(id, base, prism) => 
              prism.getOption(value).map(serialize(base, _)).toList map { json => 
                jObject(JsonObject.single(id, json))
              }
          } 

          results.head //yeah, I know

        case RecordSchema(props) => serializeObj(props, value)
      }
    }

    def serializeObj[I](rb: FreeAp[PropSchema[I, Schema[A, P, ?], ?], I], value: I): Json = {
      jObject(
        rb.foldMap[State[JsonObject, ?]](
          new (PropSchema[I, Schema[A, P, ?], ?] ~> State[JsonObject, ?]) {
            def apply[B](ps: PropSchema[I, Schema[A, P, ?], B]): State[JsonObject, B] = {
              for {
                obj <- get
                _ <- ps match {
                  case Required(field, schema, getter, _) => 
                    put(obj + (field, serialize(schema, getter.get(value))))

                  case opt: Optional[I, Schema[A, P, ?], i] =>
                    opt.getter.get(value).cata(
                      v => put(obj + (opt.fieldName, serialize(opt.valueSchema, v))),
                      ().pure[State[JsonObject, ?]]
                    )
                }
              } yield ps.getter.get(value)
            }
          }
        ).exec(JsonObject.empty)
      )
    }
  }
}

trait FromJson[S[_]] {
  def decoder[A](schema: S[A]): DecodeJson[A]
}

object FromJson {
  implicit def jSchemaFromJson[A, P[_]: FromJson]: FromJson[Schema[A, P, ?]] = new FromJson[Schema[A, P, ?]] {
    def decoder[I](schema: Schema[A, P, I]): DecodeJson[I] = {
      schema.tail.value match {
        case PrimSchema(p) => implicitly[FromJson[P]].decoder(p)
        case OneOfSchema(alts) => 
          DecodeJson { (c: HCursor) => 
            val results = for {
              fields <- c.fields.toList
              altResult <- alts flatMap {
                case Alt(id, base, prism) =>
                  fields.exists(_ == id).option(
                    c.downField(id).as(decoder(base)).map(prism.reverseGet)
                  ).toList
              }
            } yield altResult 

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => DecodeResult.fail(s"No fields found matching any of ${altIds}", c.history)
              case _ => DecodeResult.fail(s"More than one matching field found among ${altIds}", c.history)
            }
          }

        case RecordSchema(rb) => decodeObj(rb)
      }
    }

    def decodeObj[I](rb: FreeAp[PropSchema[I, Schema[A, P, ?], ?], I]): DecodeJson[I] = {
      implicit val djap: Applicative[DecodeJson] = new Applicative[DecodeJson] {
        def point[T](a: => T) = DecodeJson(_ => DecodeResult.ok(a))
        def ap[T, U](fa: => DecodeJson[T])(ff: => DecodeJson[T => U]): DecodeJson[U] = {
          fa.flatMap(a => ff.map(_(a)))
        }
      }

      rb.foldMap(
        new (PropSchema[I, Schema[A, P, ?], ?] ~> DecodeJson) {
          def apply[B](ps: PropSchema[I, Schema[A, P, ?], B]): DecodeJson[B] = ps match {
            case Required(field, schema, _, _) => 
              DecodeJson(_.downField(field).as(decoder(schema)))

            case opt: Optional[I, Schema[A, P, ?], i] => 
              DecodeJson(_.downField(opt.fieldName).as(OptionDecodeJson(decoder(opt.valueSchema))))
          }
        }
      )
    }
  }
}
