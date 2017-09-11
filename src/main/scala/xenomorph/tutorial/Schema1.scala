package xenomorph.tutorial

import argonaut.Json
import argonaut.Json._
import argonaut.DecodeJson
import argonaut.DecodeJson._
import argonaut.DecodeResult
import argonaut.HCursor
import argonaut.JsonObject

import monocle.Prism

import scalaz.~>
import scalaz.NaturalTransformation
import scalaz.Applicative
import scalaz.FreeAp
import scalaz.State
import scalaz.State._
import scalaz.syntax.std.boolean._

object schema1 {
  sealed trait JSchema[A]

  case object JBoolT extends JSchema[Boolean]
  case object JStrT extends JSchema[String]
  case object JNumT extends JSchema[Long]
  case class JVecT[A](elemType: JSchema[A]) extends JSchema[Vector[A]]

  val serializer: (JSchema ~> (? => Json)) = new (JSchema ~> (? => Json)) {
    def apply[A](schema: JSchema[A]): A => Json = {
      schema match {
        case JBoolT => jBool(_)
        case JStrT  => jString(_)
        case JNumT  => jNumber(_)
        case JVecT(base) => 
          value => jArray(value.map(serializer(base)).toList)
      }
    }
  }

  val decoder: (JSchema ~> DecodeJson) = new (JSchema ~> DecodeJson) {
    def apply[A](schema: JSchema[A]): DecodeJson[A] = {
      schema match {
        case JBoolT => BooleanDecodeJson
        case JStrT  => StringDecodeJson
        case JNumT  => LongDecodeJson
        case JVecT(base) => VectorDecodeJson(decoder(base))
      }
    }
  }
}

object schema2 {
  sealed trait JSchema[A]

  case object JBoolT extends JSchema[Boolean]
  case object JStrT extends JSchema[String]
  case object JNumT extends JSchema[Long]
  case class JVecT[A](elemType: JSchema[A]) extends JSchema[Vector[A]]

  case class JObjT[A](props: Props[A, A]) extends JSchema[A]

  sealed trait Props[O, A]
  case class PureProps[O, A](a: A) extends Props[O, A]
  case class ApProps[O, A, B](
    hd: PropSchema[O, B],
    tl: Props[O, B => A]
  ) extends Props[O, A]

  case class PropSchema[O, A](fieldName: String, valueSchema: JSchema[A], accessor: O => A)

  object Props {
    def applicative[O] = new Applicative[Props[O, ?]] {
      def point[A](a: => A): Props[O, A] = PureProps(a)

      override def map[A,B](fa: Props[O, A])(f: A => B): Props[O, B] = {
        fa match {
          case PureProps(a) => PureProps(f(a))
          case ApProps(hd, tl) => ApProps(hd, map(tl)(f compose _))
        }
      }

      def ap[A,B](fa: => Props[O, A])(ff: => Props[O, A => B]): Props[O, B] = {
        ff match {
          case PureProps(f) => map(fa)(f)
          case aprb: ApProps[O, (A => B), i] =>
            ApProps(
              aprb.hd,
              ap(fa) {
                map[i => (A => B), A => (i => B)](aprb.tl) {
                  (g: i => (A => B)) => { (a: A) => { (i: i) => g(i)(a) } } // this is just flip
                }
              }
            )
        }
      }
    }
  }
}

object schema3 {
  sealed trait JSchema[A]

  case object JBoolT extends JSchema[Boolean]
  case object JStrT extends JSchema[String]
  case object JNumT extends JSchema[Long]
  case class JVecT[A](elemType: JSchema[A]) extends JSchema[Vector[A]]

  case class JObjT[A](props: Props[PropSchema, A, A]) extends JSchema[A]
  case class PropSchema[O, A](fieldName: String, valueSchema: JSchema[A], accessor: O => A)

  sealed trait Props[F[_, _], O, A]
  case class PureProps[F[_, _], O, A](a: A) extends Props[F, O, A]
  case class ApProps[F[_, _], O, A, B](hd: F[O, B], tl: Props[F, O, B => A]) extends Props[F, O, A]
}

object schema4 {
  sealed trait JSchema[A]

  case object JBoolT extends JSchema[Boolean]
  case object JStrT extends JSchema[String]
  case object JNumT extends JSchema[Long]
  case class JVecT[A](elemType: JSchema[A]) extends JSchema[Vector[A]]

  case class JObjT[A](props: Props[PropSchema[A, ?], A]) extends JSchema[A]
  case class PropSchema[O, A](fieldName: String, valueSchema: JSchema[A], accessor: O => A)

  sealed trait Props[F[_], A]
  case class PureProps[F[_], A](a: A) extends Props[F, A]
  case class ApProps[F[_], A, B](hd: F[B], tl: Props[F, B => A]) extends Props[F, A]
}

object schema5 {
  sealed trait JSchema[A]

  case object JBoolT extends JSchema[Boolean]
  case object JStrT extends JSchema[String]
  case object JNumT extends JSchema[Long]
  case class JVecT[A](elemType: JSchema[A]) extends JSchema[Vector[A]]

  case class JObjT[A](props: FreeAp[PropSchema[A, ?], A]) extends JSchema[A]
  case class PropSchema[O, A](fieldName: String, valueSchema: JSchema[A], accessor: O => A)

  case class JSumT[A](alternatives: List[Alt[A, B] forSome { type B }]) extends JSchema[A]
  case class Alt[A, B](id: String, base: JSchema[B], prism: Prism[A, B])

  object JSchema {
    val serializer: (JSchema ~> (? => Json)) = new (JSchema ~> (? => Json)) {
      def apply[A](schema: JSchema[A]): A => Json = {
        schema match {
          case JBoolT => jBool(_)
          case JStrT  => jString(_)
          case JNumT  => jNumber(_)
          case JVecT(elemSchema) => 
            value => jArray(value.map(serializer(elemSchema)).toList)
          case JSumT(alts) =>
            value => alts.flatMap({
              case Alt(id, base, prism) =>
                prism.getOption(value).map(serializer(base)).toList map { json =>
                  jObject(JsonObject.single(id, json))
                }
            }).head

          case JObjT(rb) => serializeObj(rb, _)
        }
      }
    }

    def serializeObj[A](rb: FreeAp[PropSchema[A, ?], A], value: A): Json = {
      jObject(
        rb.foldMap[State[JsonObject, ?]](
          new NaturalTransformation[PropSchema[A, ?], State[JsonObject, ?]] {
            def apply[B](ps: PropSchema[A, B]): State[JsonObject, B] = {
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

    val decoder: (JSchema ~> DecodeJson) = new (JSchema ~> DecodeJson) {
      def apply[A](schema: JSchema[A]): DecodeJson[A] = {
        schema match {
          case JBoolT => BooleanDecodeJson
          case JStrT  => StringDecodeJson
          case JNumT  => LongDecodeJson
          case JVecT(elemSchema) => VectorDecodeJson(decoder(elemSchema))
          case JSumT(alts) =>
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

          case JObjT(rb) => decodeObj(rb)
        }
      }
    }

    def decodeObj[A](rb: FreeAp[PropSchema[A, ?], A]): DecodeJson[A] = {
      implicit val djap: Applicative[DecodeJson] = new Applicative[DecodeJson] {
        def point[A0](a: => A0) = DecodeJson(_ => DecodeResult.ok(a))
        def ap[A0, B](fa: => DecodeJson[A0])(ff: => DecodeJson[A0 => B]): DecodeJson[B] = {
          fa.flatMap(a => ff.map(_(a)))
        }
      }

      rb.foldMap(
        new NaturalTransformation[PropSchema[A, ?], DecodeJson] {
          def apply[B](ps: PropSchema[A, B]): DecodeJson[B] = {
            DecodeJson(_.downField(ps.fieldName).as(decoder(ps.valueSchema)))
          }
        }
      )
    }
  }
}
