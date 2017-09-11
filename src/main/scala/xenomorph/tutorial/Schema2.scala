package xenomorph.tutorial

import argonaut.Json
import argonaut.Json._
import argonaut.DecodeJson
import argonaut.DecodeJson._
import argonaut.DecodeResult
import argonaut.HCursor
import argonaut.JsonObject

import scalaz.~>
import scalaz.Applicative
import scalaz.Coproduct
import scalaz.FreeAp
import scalaz.State
import scalaz.State._
import scalaz.syntax.std.boolean._

trait ToJson[S[_]] {
  def serializer: (S ~> (? => Json))
}

trait FromJson[S[_]] {
  def decoder: (S ~> DecodeJson)
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
      val serializer: (GSchema[P, ?] ~> (? => Json)) = new (GSchema[P, ?] ~> (? => Json)) {
        def apply[A](schema: GSchema[P, A]): A => Json = {
          schema match {
            case PrimT(p) => 
              implicitly[ToJson[P]].serializer(p)

            case SumT(alts) =>
              value => alts.flatMap({
                case Alt(id, base, _, preview) =>
                  preview(value).map(serializer(base)).toList map { json =>
                    jObject(JsonObject.single(id, json))
                  }
              }).head

            case ObjT(rb) => 
              serializeObj(rb, _)

            case vecT: VecT[P, a] => 
              (value: Vector[a]) => jArray(value.map(serializer(vecT.elemType)).toList)
          }
        }
      }

      def serializeObj[A](rb: FreeAp[PropSchema[P, A, ?], A], value: A): Json = {
        jObject(
          rb.foldMap[State[JsonObject, ?]](
            new (PropSchema[P, A, ?] ~> State[JsonObject, ?]) {
              def apply[B](ps: PropSchema[P, A, B]): State[JsonObject, B] = {
                val elem: B = ps.accessor(value)
                for {
                  obj <- get
                  _ <- put(obj + (ps.fieldName, serializer(ps.valueSchema)(elem)))
                } yield elem
              }
            }
          ).exec(JsonObject.empty)
        )
      }
    }

    implicit def jSchemaFromJson[P[_]: FromJson]: FromJson[GSchema[P, ?]] = new FromJson[GSchema[P, ?]] {
      val decoder: (GSchema[P, ?] ~> DecodeJson) = new (GSchema[P, ?] ~> DecodeJson) {
        def apply[A](schema: GSchema[P, A]): DecodeJson[A] = {
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
                  case _ => DecodeResult.fail(s"More than one matching field found among ${altIds}", c.history)
                }
              }

            case ObjT(rb) => decodeObj(rb)
          }
        }
      }

      def decodeObj[A](rb: FreeAp[PropSchema[P, A, ?], A]): DecodeJson[A] = {
        implicit val djap: Applicative[DecodeJson] = new Applicative[DecodeJson] {
          def point[A0](a: => A0) = DecodeJson(_ => DecodeResult.ok(a))
          def ap[A0, B](fa: => DecodeJson[A0])(ff: => DecodeJson[A0 => B]): DecodeJson[B] = {
            fa.flatMap(a => ff.map(_(a)))
          }
        }

        rb.foldMap(
          new (PropSchema[P, A, ?] ~> DecodeJson) {
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
  case object JNumT extends JsonPrim[Long]

  object JsonPrim {
    implicit val JsonPrimToJson = new ToJson[JsonPrim] {
      val serializer: (JsonPrim ~> (? => Json)) = new (JsonPrim ~> (? => Json)) {
        def apply[A](p: JsonPrim[A]): A => Json = {
          p match {
            case JStrT  => jString(_)
            case JNumT  => jNumber(_)
            case JBoolT => jBool(_)
          }
        }
      }
    }
  }
}

object primCoproduct {
  implicit def primCoToJson[P[_]: ToJson, Q[_]: ToJson] = new ToJson[Coproduct[P, Q, ?]] {
    val serializer = new (Coproduct[P, Q, ?] ~> (? => Json)) {
      def apply[A](p: Coproduct[P, Q, A]): A => Json = {
        p.run.fold(
          implicitly[ToJson[P]].serializer(_),
          implicitly[ToJson[Q]].serializer(_)
        )
      }
    }
  }

  implicit def primCoFromJson[P[_]: FromJson, Q[_]: FromJson] = new FromJson[Coproduct[P, Q, ?]] {
    val decoder = new (Coproduct[P, Q, ?] ~> DecodeJson) {
      def apply[A](p: Coproduct[P, Q, A]): DecodeJson[A] = {
        p.run.fold(
          implicitly[FromJson[P]].decoder(_),
          implicitly[FromJson[Q]].decoder(_),
        )
      }
    }
  }
}

object fixpoints {
  case class HFix[F[_[_], _], I](f: F[HFix[F, ?], I])

  case class HCofree[F[_[_], _], A, I](f: F[HCofree[F, A, ?], I], a: A) 

  trait HFunctor[F[_[_], _]] {
    def hfmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
  }

  def forget[F[_[_], _]: HFunctor, A]: (HCofree[F, A, ?] ~> HFix[F, ?]) = 
    new (HCofree[F, A, ?] ~> HFix[F, ?]) { self => 
      def apply[I](c: HCofree[F, A, I]): HFix[F, I] = {
        HFix(implicitly[HFunctor[F]].hfmap(self).apply[I](c.f))
      }
    }

  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
}
