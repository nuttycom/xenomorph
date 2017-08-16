package xenomorph.tutorial

import argonaut.Json
import argonaut.Json._
import argonaut.DecodeJson
import argonaut.DecodeJson._
import argonaut.DecodeResult
import argonaut.HCursor
import argonaut.JsonObject

import scalaz.NaturalTransformation
import scalaz.Applicative
import scalaz.FreeAp
import scalaz.State
import scalaz.State._
import scalaz.syntax.std.boolean._

object schemaPrim {
  sealed trait JSchema[P[_], A]

  sealed trait ToJson[S[_]] {
    def serialize[A](schema: S[A], value: A): Json
  }

  sealed trait FromJson[S[_]] {
    def decoder[A](schema: S[A]): DecodeJson[A]
  }

  class JSchemaToJson[P[_]: ToJson] extends ToJson[JSchema[P, ?]] {
    def serialize[A](schema: JSchema[P, A], value: A): Json = {
      schema match {
        case JPrimT(p) => implicitly[ToJson[P]].serialize(p, value)
        case JSumT(alts) => 
          val results = alts flatMap {
            case Alt(id, base, _, preview) => 
              preview(value).map(serialize(base, _)).toList map { json => 
                jObject(JsonObject.single(id, json))
              }
          } 

          results.head //yeah, I know

        case JObjT(rb) => serializeObj(rb, value)
        case vecT: JVecT[P, a] => jArray((value: Vector[a]).map(this.serialize(vecT.elemType, _)).toList)
      }
    }

    def serializeObj[A](rb: FreeAp[PropSchema[P, A, ?], A], value: A): Json = {
      jObject(
        rb.foldMap[State[JsonObject, ?]](
          new NaturalTransformation[PropSchema[P, A, ?], State[JsonObject, ?]] {
            def apply[B](ps: PropSchema[P, A, B]): State[JsonObject, B] = {
              val elem: B = ps.accessor(value)
              for {
                obj <- get
                _ <- put(obj + (ps.fieldName, serialize(ps.valueSchema, elem)))
              } yield elem
            }
          }
        ).exec(JsonObject.empty)
      )
    }
  }

  class JSchemaFromJson[P[_]: FromJson] extends FromJson[JSchema[P, ?]] {
    def decoder[A](schema: JSchema[P, A]): DecodeJson[A] = {
      schema match {
        case JPrimT(p) => implicitly[FromJson[P]].decoder(p)
        case JVecT(elemSchema) => VectorDecodeJson(decoder(elemSchema))
        case JSumT(alts) => 
          DecodeJson { (c: HCursor) => 
            val results = for {
              fields <- c.fields.toList
              altResult <- alts flatMap {
                case Alt(id, base, review, _) =>
                  fields.exists(_ == id).option(
                    c.downField(id).as(decoder(base)).map(review)
                  ).toList
              }
            } yield altResult 

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => DecodeResult.fail(s"No fields found matching any of ${altIds}", c.history)
              case xs => DecodeResult.fail(s"More than one matching field found among ${altIds}", c.history)
            }
          }

        case JObjT(rb) => decodeObj(rb)
      }
    }

    def decodeObj[A](rb: FreeAp[PropSchema[P, A, ?], A]): DecodeJson[A] = {
      implicit val djap: Applicative[DecodeJson] = new Applicative[DecodeJson] {
        def point[A](a: => A) = DecodeJson(_ => DecodeResult.ok(a))
        def ap[A, B](fa: => DecodeJson[A])(ff: => DecodeJson[A => B]): DecodeJson[B] = {
          fa.flatMap(a => ff.map(_(a)))
        }
      }

      rb.foldMap(
        new NaturalTransformation[PropSchema[P, A, ?], DecodeJson] {
          def apply[B](ps: PropSchema[P, A, B]): DecodeJson[B] = {
            DecodeJson(_.downField(ps.fieldName).as(decoder(ps.valueSchema)))
          }
        }
      )
    }
  }

  case class JPrimT[P[_], A](prim: P[A]) extends JSchema[P, A]

  case class JVecT[P[_], A](elemType: JSchema[P, A]) extends JSchema[P, Vector[A]]

  case class JSumT[P[_], A](alternatives: List[Alt[P, A, B] forSome { type B }]) extends JSchema[P, A]
  case class Alt[P[_], A, B](id: String, base: JSchema[P, B], review: B => A, preview: A => Option[B])

  case class JObjT[P[_], A](recordBuilder: FreeAp[PropSchema[P, A, ?], A]) extends JSchema[P, A]
  case class PropSchema[P[_], O, A](fieldName: String, valueSchema: JSchema[P, A], accessor: O => A)

  sealed trait JsonPrim[A]
  case object JBoolT extends JsonPrim[Boolean]
  case object JStrT extends JsonPrim[String]
  case object JNumT extends JsonPrim[Double]
}
