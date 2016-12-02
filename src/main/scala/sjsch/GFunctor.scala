package sjsch

import scalaz.~>

trait GFunctor[F[_[_], _]] {
  def gmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

object GFunctor {
  final implicit class GFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: GFunctor[F]) {
    def gmap[N[_]](nt: M ~> N): F[N, A] = F.gmap(nt)(fa)
  }
}

case class GFix[F[_[_], _], A](project: F[GFix[F, ?], A])

case class GCofree[F[_[_], _], A, B](head: B, tail: F[GCofree[F, ?, B], A])
