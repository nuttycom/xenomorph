/*
 * Copyright (C) 2017 Kris Nuttycombe, Antonio Alonso Dominguez, Doug Clinton
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
package xenomorph.scodec

import scodec._
import scodec.bits._
import scodec.codecs.implicits._

import scalaz.~>
import scalaz.StateT
import scalaz.{IList, ICons, INil}
import scalaz.FreeAp
import scalaz.syntax.foldable._
import scalaz.syntax.monad._

import xenomorph._
import xenomorph.Schema._
import xenomorph.HFunctor._

trait ToEncoder[S[_]] {
  def toEncoder: S ~> Encoder
}

object ToEncoder {
  implicit class ToEncoderOps[S[_], A](s: S[A]) {
    def toEncoder(implicit TE: ToEncoder[S]): Encoder[A] = TE.toEncoder(s)
  }

  implicit def schemaToEncoder[P[_]: ToEncoder]: ToEncoder[Schema[P, ?]] = new ToEncoder[Schema[P, ?]] {
    override val toEncoder = new (Schema[P, ?] ~> Encoder) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], Encoder](genAlg).apply(schema)
      }
    }
  }

  def genAlg[P[_]: ToEncoder]: HAlgebra[SchemaF[P, ?[_], ?], Encoder] =
    new HAlgebra[SchemaF[P, ?[_], ?], Encoder] {
      def apply[I](schema: SchemaF[P, Encoder, I]): Encoder[I] = schema match {
        case s: PrimSchema[P, Encoder, I] => implicitly[ToEncoder[P]].toEncoder(s.prim)
        case s: OneOfSchema[P, Encoder, I] =>
          Encoder(
            (value: I) => {
              val results = s.alts.toList flatMap {
                case alt: Alt[Encoder, I, i] => {
                  alt.prism.getOption(value).map(
                    (baseValue: i) => for {
                      idBits <- Encoder[String].encode(alt.id)
                      valueBits <- alt.base.encode(baseValue)
                    } yield idBits ++ valueBits
                  ).toList
                }
              }

              results.head
            }
          )
        case s: RecordSchema[P, Encoder, I] => recordEncoder[P, I](s.props)
        case s: IsoSchema[P, Encoder, i0, I] => Encoder((value: I) => s.base.encode(s.iso.reverseGet(value)))
        case s: PrismSchema[P, Encoder, i0, I] => Encoder((value: I) => s.base.encode(s.prism.reverseGet(value)))
      }
    }

  def recordEncoder[P[_]: ToEncoder, I](rb: FreeAp[PropSchema[I, Encoder, ?], I]): Encoder[I] = Encoder(
    (value: I) => rb.foldMap[StateT[Attempt, Vector[BitVector], ?]](
      new (PropSchema[I, Encoder, ?] ~> StateT[Attempt, Vector[BitVector], ?]) {
        implicit val sms = StateT.stateTMonadState[Vector[BitVector], Attempt]

        def apply[B](ps: PropSchema[I, Encoder, B]): StateT[Attempt, Vector[BitVector], B] = {
          for {
            newBytes <- ps match {
              case req: Required[I, Encoder, i] =>
                // skipping here the field name ??
                req.base.encode(req.getter.get(value)).liftM[StateT[?[_], Vector[BitVector], ?]]
              case opt: Optional[I, Encoder, i] =>
                implicit val baseCodec: Codec[i] = Codec(opt.base, null)
                implicitOptionCodec[i].encode(opt.getter.get(value)).liftM[StateT[?[_], Vector[BitVector], ?]]
            }
            _ <- sms.modify { (bytes: Vector[BitVector]) => bytes :+ newBytes }
          } yield ps.getter.get(value)
        }
      }
    ).exec(Vector.empty).map(BitVector.concat _)
  )
}

trait ToDecoder[S[_]] {
  def toDecoder: S ~> Decoder
}

object ToDecoder {
  implicit class ToDecoderOps[S[_], A](s: S[A]) {
    def toDecoder(implicit TE: ToDecoder[S]): Decoder[A] = TE.toDecoder(s)
  }

  implicit def schemaToDecoder[P[_]: ToDecoder]: ToDecoder[Schema[P, ?]] = new ToDecoder[Schema[P, ?]] {
    override val toDecoder = new (Schema[P, ?] ~> Decoder) {
      override def apply[I](schema: Schema[P, I]) = {
        HFix.cataNT[SchemaF[P, ?[_], ?], Decoder](genAlg).apply(schema)
      }
    }
  }

  def genAlg[P[_]: ToDecoder]: HAlgebra[SchemaF[P, ?[_], ?], Decoder] =
    new HAlgebra[SchemaF[P, ?[_], ?], Decoder] {
      def apply[I](schema: SchemaF[P, Decoder, I]): Decoder[I] = schema match {
        case s: PrimSchema[P, Decoder, I] => implicitly[ToDecoder[P]].toDecoder(s.prim)
        case s: OneOfSchema[P, Decoder, I] =>
          Decoder[String] flatMap { altId =>
            val underlying: IList[Decoder[I]] = s.alts.list.flatMap{
              case Alt(id, base, prism) =>
                if (id == altId) IList.single(base.map(prism.reverseGet)) else IList.empty
            }

            val altIds = s.alts.map(_.id)
            underlying match {
              case ICons(x, INil()) => x
              case INil() => Decoder.liftAttempt(Attempt.failure(Err(s"No fields found matching any of ${altIds}")))
              case _ => Decoder.liftAttempt(Attempt.failure(Err(s"More than one matching field found among ${altIds}")))
            }
          }

        case s: RecordSchema[P, Decoder, I] => recordDecoder[P, I](s.props)
        case s: IsoSchema[P, Decoder, i0, I] => s.base.map(s.iso.get(_))
        case s: PrismSchema[P, Decoder, i0, I] => s.base.flatMap(x => Decoder.liftAttempt(Attempt.fromOption(s.prism.getOption(x), Err("Decode error"))))
      }
    }

  def recordDecoder[P[_]: ToDecoder, I](rb: FreeAp[PropSchema[I, Decoder, ?], I]): Decoder[I] = {
    rb.foldMap(
      new (PropSchema[I, Decoder, ?] ~> Decoder) {
        def apply[B](ps: PropSchema[I, Decoder, B]): Decoder[B] = ps match {
          case Required(_, base, _, _) => base
          case opt: Optional[I, Decoder, i] =>
            implicit val baseCodec: Codec[i] = Codec(null, opt.base)
            implicitOptionCodec[i].asDecoder
        }
      }
    )
  }
}

final class ToCodec[S[_]](val toCodec: S ~> Codec) {
  val toToEncoder: ToEncoder[S] = new ToEncoder[S] {
    override val toEncoder = new (S ~> Encoder) {
      def apply[I](s: S[I]): Encoder[I] = toCodec(s).asEncoder
    }
  }

  val toToDecoder: ToDecoder[S] = new ToDecoder[S] {
    override val toDecoder = new (S ~> Decoder) {
      def apply[I](s: S[I]): Decoder[I] = toCodec(s).asDecoder
    }
  }
}

object ToCodec {
  import ToEncoder._
  import ToDecoder._

  implicit class ToCodecOps[S[_], A](s: S[A]) {
    def toCodec(implicit TC: ToCodec[S]): Codec[A] = TC.toCodec(s)
  }

  implicit def schemaToCodec[P[_]: ToCodec]: ToCodec[Schema[P, ?]] = new ToCodec[Schema[P, ?]](
    new (Schema[P, ?] ~> Codec) {
      override def apply[I](schema: Schema[P, I]) = Codec(
        schemaToEncoder(implicitly[ToCodec[P]].toToEncoder).toEncoder(schema),
        schemaToDecoder(implicitly[ToCodec[P]].toToDecoder).toDecoder(schema)
      )
    }
  )
}

