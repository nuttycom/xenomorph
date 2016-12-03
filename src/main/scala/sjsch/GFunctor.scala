package sjsch

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.NaturalTransformation
import scalaz.syntax.applicative._

trait GFunctor[F[_[_], _]] {
  def gmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

object GFunctor {
  def apply[F[_[_], _]](implicit v: GFunctor[F]) = v

  final implicit class GFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: GFunctor[F]) {
    def gmap[N[_]](nt: M ~> N): F[N, A] = F.gmap(nt)(fa)
  }
}

case class GCofree[F[_[_], _], A[_], I](head: A[I], tail: F[GCofree[F, A, ?], I]) { 
  def gmap[B[_]](nt: A ~> B)(implicit G: GFunctor[F]): GCofree[F, B, I] = {
    GCofree(nt(head), G.gmap[GCofree[F, A, ?], GCofree[F, B, ?]](GCofree.gnt[F, A, B](nt))(tail))
  }
}

object GCofree {
  type GFix[F[_[_], _], A] = GCofree[F, Const[Unit, ?], A]

  def gfix[F[_[_], _], A](fga: F[GFix[F, ?], A]): GFix[F, A] = GCofree[F, Const[Unit, ?], A](Const(Unit), fga)

  def gnt[F[_[_], _]: GFunctor, A[_], B[_]](nt: A ~> B): GCofree[F, A, ?] ~> GCofree[F, B, ?] = 
    new NaturalTransformation[GCofree[F, A, ?], GCofree[F, B, ?]] {
      def apply[I](gcf: GCofree[F, A, I]) = gcf.gmap(nt)
    }

  implicit def instances[F[_[_], _]: GFunctor]: GFunctor[GCofree[F, ?[_], ?]] = new GFunctor[GCofree[F, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N): GCofree[F, M, ?] ~> GCofree[F, N, ?] = gnt(nt)
  }
}
