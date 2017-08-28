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
import scalaz.Coproduct
import scalaz.FreeAp
import scalaz.State
import scalaz.State._
import scalaz.syntax.std.boolean._

sealed trait ToJson[S[_]] {
  def serialize[A](schema: S[A], value: A): Json
}

sealed trait FromJson[S[_]] {
  def decoder[A](schema: S[A]): DecodeJson[A]
}

object schemaPrim {
  sealed trait GSchema[P[_], A]

  case class PrimT[P[_], A](prim: P[A]) extends GSchema[P, A]

  case class VecT[P[_], A](elemType: GSchema[P, A]) extends GSchema[P, Vector[A]]

  case class SumT[P[_], A](alternatives: List[Alt[P, A, B] forSome { type B }]) extends GSchema[P, A]
  case class Alt[P[_], A, B](id: String, base: GSchema[P, B], review: B => A, preview: A => Option[B])

  case class ObjT[P[_], A](props: FreeAp[PropSchema[P, A, ?], A]) extends GSchema[P, A]
  case class PropSchema[P[_], O, A](fieldName: String, valueSchema: GSchema[P, A], accessor: O => A)

  object GSchema {
    implicit def jSchemaToJson[P[_]: ToJson]: ToJson[GSchema[P, ?]] = new ToJson[GSchema[P, ?]] {
      def serialize[A](schema: GSchema[P, A], value: A): Json = {
        schema match {
          case PrimT(p) => implicitly[ToJson[P]].serialize(p, value)
          case SumT(alts) => 
            val results = alts flatMap {
              case Alt(id, base, _, preview) => 
                preview(value).map(serialize(base, _)).toList map { json => 
                  jObject(JsonObject.single(id, json))
                }
            } 

            results.head //yeah, I know

          case ObjT(rb) => serializeObj(rb, value)
          case vecT: VecT[P, a] => jArray((value: Vector[a]).map(this.serialize(vecT.elemType, _)).toList)
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

    implicit def jSchemaFromJson[P[_]: FromJson]: FromJson[GSchema[P, ?]] = new FromJson[GSchema[P, ?]] {
      def decoder[A](schema: GSchema[P, A]): DecodeJson[A] = {
        schema match {
          case PrimT(p) => implicitly[FromJson[P]].decoder(p)
          case VecT(elemSchema) => VectorDecodeJson(decoder(elemSchema))
          case SumT(alts) => 
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

          case ObjT(rb) => decodeObj(rb)
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
  }
}

object schemaPrimJson {
  sealed trait JsonPrim[A]
  case object JBoolT extends JsonPrim[Boolean]
  case object JStrT extends JsonPrim[String]
  case object JNumT extends JsonPrim[Double]

  object JsonPrim {
    implicit val JsonPrimToJson = new ToJson[JsonPrim] {
      def serialize[A](p: JsonPrim[A], value: A): Json = {
        p match {
          case JStrT  => jString(value)
          case JNumT  => jNumber(value)
          case JBoolT => jBool(value)
        }
      }
    }
  }
}

object primCoproduct {
  implicit def primCoToJson[P[_]: ToJson, Q[_]: ToJson] = new ToJson[Coproduct[P, Q, ?]] {
    def serialize[A](p: Coproduct[P, Q, A], value: A): Json = {
      p.run.fold(
        implicitly[ToJson[P]].serialize(_, value),
        implicitly[ToJson[Q]].serialize(_, value)
      )
    }
  }

  implicit def primCoFromJson[P[_]: FromJson, Q[_]: FromJson] = new FromJson[Coproduct[P, Q, ?]] {
    def decoder[A](p: Coproduct[P, Q, A]): DecodeJson[A] = {
      p.run.fold(
        implicitly[FromJson[P]].decoder(_),
        implicitly[FromJson[Q]].decoder(_)
      )
    }
  }
}
