package sjsch

import scalaz.~>

trait GFunctor[F[_[_], _]] {
  def gmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

object GFunctor {
  def apply[F[_[_], _]](implicit v: GFunctor[F]) = v

  final implicit class GFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: GFunctor[F]) {
    def gmap[N[_]](nt: M ~> N): F[N, A] = F.gmap(nt)(fa)
  }
}

// A is the annotation type; X is the type argument to F that must be carried
// through the tree.
case class GCofree[F[_[_], _], A[_], I](head: A[I], tail: F[GCofree[F, A, ?], I]) {
//  def gmap[G[_[_], _]](nt: F ~> N)(implicit G: GFunctor[F]): ({ type λ[α] = GCofree[F[M, α], α, A] })#λ ~> ({ type λ[α] = GCofree[F[M, α], α, A] })#λ = 
}

object GCofree {
  type GFix[F[_[_], _], A[_]] = GCofree[F, A, Unit]

//  implicit def instances[F[_[_], _]: GFunctor, A] = new GFunctor[({ type λ[ƒ[_], α] = GCofree[F[ƒ, α], α, A] })#λ] {
//    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[({ type λ[α] = GCofree[F[M, α], α, A] })#λ, ({ type λ[α] = GCofree[F[N, α], α, A] })#λ] {
//      def apply[A]
//    }
//  }
}
