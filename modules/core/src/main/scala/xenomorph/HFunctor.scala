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
final case class HFix[F[_[_], _], I](unfix: Name[F[HFix[F, ?], I]])

object HFix {
  import HFunctor._

  def hfix[F[_[_], _], I](fa: => F[HFix[F, ?], I]): HFix[F, I] =
    HFix[F, I](Need(fa))

  def cataNT[F[_[_], _]: HFunctor, G[_]](alg: HAlgebra[F, G]): (HFix[F, ?] ~> G) = 
    new (HFix[F, ?] ~> G) { self => 
      def apply[I](f: HFix[F, I]): G[I] = {
        alg.apply[I](f.unfix.value.hfmap[G](self))
      }
    }

  type HCofree[F[_[_], _], A, I] = HFix[HEnvT[A, F, ?[_], ?], I]

  /** Smart constructor for HCofree values. */
  def hcofree[F[_[_], _], A, I](ask: A, fga: => F[HCofree[F, A, ?], I]): HCofree[F, A, I] = 
    hfix[HEnvT[A, F, ?[_], ?], I](HEnvT(ask, fga))

  /**
   * Algebra to discard the annotations from an HCofree structure.
   */
  def forget[F[_[_], _], A] = new HAlgebra[HEnvT[A, F, ?[_], ?], HFix[F, ?]] {
    def apply[I](env: HEnvT[A, F, HFix[F, ?], I]) = hfix(env.fa)
  }

  /** Functor over the annotation type of an HCofree value */
  implicit def functor[F[_[_], _], I](implicit HF: HFunctor[F]): Functor[HCofree[F, ?, I]] = 
    new Functor[HCofree[F, ?, I]] { 
      def map[A, B](fa: HCofree[F, A, I])(f: A => B): HCofree[F, B, I] = {
        val step = fa.unfix.value
        val hf = new (HCofree[F, A, ?] ~> HCofree[F, B, ?]) {
          def apply[I0](gcf: HCofree[F, A, I0]) = functor(HF).map(gcf)(f)
        }

        hcofree(
          f(step.ask), 
          HF.hfmap[HCofree[F, A, ?], HCofree[F, B, ?]](hf).apply(step.fa)
        )
      }
    } 
}

final case class HMutu[F[_[_], _], G[_[_], _], I](unmutu: F[G[HMutu[F, G, ?], ?], I])

final case class HEnvT[E, F[_[_], _], G[_], I](ask: E, fa: F[G, I])

object HEnvT {
  import HFunctor._

  implicit def hfunctor[E, F[_[_], _]: HFunctor]: HFunctor[HEnvT[E, F, ?[_], ?]] =
    new HFunctor[HEnvT[E, F, ?[_], ?]] {
      def hfmap[M[_], N[_]](nt: M ~> N) = new (HEnvT[E, F, M, ?] ~> HEnvT[E, F, N, ?]) {
        def apply[I](fm: HEnvT[E, F, M, I]) = HEnvT(fm.ask, fm.fa.hfmap[N](nt))
      }
    }
}

