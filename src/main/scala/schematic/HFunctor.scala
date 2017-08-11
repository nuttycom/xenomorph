package schematic

import scalaz.~>
import scalaz.Functor
import scalaz.Applicative
import scalaz.Const
import scalaz.Name
import scalaz.Need
import scalaz.NaturalTransformation
import scalaz.syntax.applicative._

trait HFunctor[F[_[_], _]] {
  def hfmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

trait HONatTrans[F[_[_], _], G[_[_], _]] {
  def apply[M[_], A](f: F[M, A]): G[M, A]
}


object HFunctor {
  def apply[F[_[_], _]](implicit v: HFunctor[F]) = v

  final implicit class HFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: HFunctor[F]) {
    def hfmap[N[_]](nt: M ~> N): F[N, A] = F.hfmap(nt)(fa)
  }
}

case class HCofree[F[_[_], _], A, I](head: A, tail: Name[F[HCofree[F, A, ?], I]])

object HCofree {
  implicit def functor[F[_[_], _], I](implicit HF: HFunctor[F]): Functor[HCofree[F, ?, I]] = new Functor[HCofree[F, ?, I]] {
    def map[A, B](fa: HCofree[F, A, I])(f: A => B): HCofree[F, B, I] = {
      HCofree(
        f(fa.head), 
        Need(
          HF.hfmap[HCofree[F, A, ?], HCofree[F, B, ?]](
            new NaturalTransformation[HCofree[F, A, ?], HCofree[F, B, ?]] {
              def apply[I0](gcf: HCofree[F, A, I0]) = functor(HF).map(gcf)(f)
            }
          ).apply(
            fa.tail.value
          )
        )
      )
    }
  }

  type HFix[F[_[_], _], I] = HCofree[F, Unit, I]

  def hfix[F[_[_], _], A](fga: F[HFix[F, ?], A]): HFix[F, A] = HCofree[F, Unit, A]((), Need(fga))
}
