
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
package xenomorph.argonaut

import argonaut._
import argonaut.DecodeJson._

import scalaz.~>
import scalaz.Coproduct
import scalaz.Applicative
import scalaz.FreeAp
import scalaz.syntax.foldable._
import scalaz.syntax.std.boolean._

import xenomorph._
import xenomorph.Schema._
import xenomorph.HFunctor._

trait FromJson[S[_]] {
  def decoder: S ~> DecodeJson
}

object FromJson {
  implicit class FromJsonOps[F[_], A](fa: F[A]) {
    def fromJson(a: Json)(implicit FJ: FromJson[F]): DecodeResult[A] = {
      FJ.decoder(fa).decodeJson(a)
    }
  }

  implicit def schemaFromJson[P[_]: FromJson]: FromJson[Schema[P, ?]] = new FromJson[Schema[P, ?]] {
    def decoder = new (Schema[P, ?] ~> DecodeJson) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], DecodeJson](decoderAlg[P]).apply(schema)
      }
    }
  }

  def decoderAlg[P[_]: FromJson]: HAlgebra[SchemaF[P, ?[_], ?], DecodeJson] =
    new HAlgebra[SchemaF[P, ?[_], ?], DecodeJson] {
      def apply[I](s: SchemaF[P, DecodeJson, I]): DecodeJson[I] = s match {
        case PrimSchema(p) =>
          implicitly[FromJson[P]].decoder(p)

        case OneOfSchema(alts) =>
          DecodeJson { (c: HCursor) =>
            val results = for {
              fields <- c.fields.toList
              altResult <- alts.toList flatMap {
                case Alt(id, base, prism) =>
                  fields.contains(id).option(
                    c.downField(id).as(base).map(prism.reverseGet)
                  ).toList
              }
            } yield altResult

            val altIds = alts.map(_.id)
            results match {
              case x :: Nil => x
              case Nil => DecodeResult.fail(s"No fields found matching any of $altIds", c.history)
              case _ => DecodeResult.fail(s"More than one matching field found among $altIds}", c.history)
            }
          }

        case RecordSchema(rb) =>
          decodeObj(rb)

        case IsoSchema(base, iso) =>
          base.map(iso.get)
      }
  }

  def decodeObj[I](rb: FreeAp[PropSchema[I, DecodeJson, ?], I]): DecodeJson[I] = {
    implicit val djap: Applicative[DecodeJson] = new Applicative[DecodeJson] {
      def point[T](a: => T) = DecodeJson(_ => DecodeResult.ok(a))
      def ap[T, U](fa: => DecodeJson[T])(ff: => DecodeJson[T => U]): DecodeJson[U] = {
        fa.flatMap(a => ff.map(_(a)))
      }
    }

    rb.foldMap(
      new (PropSchema[I, DecodeJson, ?] ~> DecodeJson) {
        def apply[B](ps: PropSchema[I, DecodeJson, B]): DecodeJson[B] = ps match {
          case Required(field, base, _, _) =>
            DecodeJson(_.downField(field).as(base))

          case opt: Optional[I, DecodeJson, i] =>
            DecodeJson(_.downField(opt.fieldName).as(OptionDecodeJson(opt.base)))
        }
      }
    )
  }

  implicit def coproductFromJson[P[_]: FromJson, Q[_]: FromJson] = new FromJson[Coproduct[P, Q, ?]] {
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
