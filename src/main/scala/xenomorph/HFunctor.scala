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
package xenomorph

import scalaz.~>
import scalaz.Functor
import scalaz.Name
import scalaz.Need

trait HFunctor[F[_[_], _]] {
  def hfmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

object HFunctor {
  def apply[F[_[_], _]](implicit v: HFunctor[F]) = v

  final implicit class HFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: HFunctor[F]) {
    def hfmap[N[_]](nt: M ~> N): F[N, A] = F.hfmap(nt)(fa)
  }

  type HAlgebra[F[_[_], _], G[_]] = F[G, ?] ~> G
}



/** Fixpoint data type that can preserve a type index through
 *  its recursive step.
 */
case class HCofree[F[_[_], _], A, I](head: A, tail: Name[F[HCofree[F, A, ?], I]])

//final case class HEnvT[E, F[_[_], _], A[_], I](ask: E, fa: F[A, I])
//
//final case class HFix[F[_[_], _], I](hfix: F[HFix[F, ?], I]

object HCofree {
  import HFunctor._

  /** Simple fixpoint type that can preserve a type index
   *  through its recursive step
   */
  type HFix[F[_[_], _], I] = HCofree[F, Unit, I]

  def cata[F[_[_], _]: HFunctor, G[_], I](f: HFix[F, I], alg: HAlgebra[F, G]): G[I] = {
    alg.apply(f.tail.value.hfmap[G](cata[F, G, I](_: HFix[F, I], alg)))
  }

  /** Functor over the annotation type of an HCofree value */
  implicit def functor[F[_[_], _], I](implicit HF: HFunctor[F]): Functor[HCofree[F, ?, I]] = new Functor[HCofree[F, ?, I]] {
    def map[A, B](fa: HCofree[F, A, I])(f: A => B): HCofree[F, B, I] = {
      HCofree(
        f(fa.head), 
        Need(
          HF.hfmap[HCofree[F, A, ?], HCofree[F, B, ?]](
            new (HCofree[F, A, ?] ~> HCofree[F, B, ?]) {
              def apply[I0](gcf: HCofree[F, A, I0]) = functor(HF).map(gcf)(f)
            }
          ).apply(
            fa.tail.value
          )
        )
      )
    }
  }

  /** Smart constructor for HCofree values. */
  def annotate[F[_[_], _], A, I](a: A, fga: => F[HCofree[F, A, ?], I]): HCofree[F, A, I] = 
    HCofree[F, A, I](a, Need(fga))

  /** Smart constructor for HFix values. */
  def hfix[F[_[_], _], I](fga: => F[HFix[F, ?], I]): HFix[F, I] = 
    annotate((), fga)
}
