package sjsch

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.NaturalTransformation
import scalaz.syntax.applicative._

trait GFunctor[F[_[_], _]] {
  def gmap[M[_], N[_]](nt: M ~> N): F[M, ?] ~> F[N, ?]
}

trait HONatTrans[F[_[_], _], G[_[_], _]] {
  def apply[M[_], A](f: F[M, A]): G[M, A]
}


object GFunctor {
  def apply[F[_[_], _]](implicit v: GFunctor[F]) = v

  final implicit class GFunctorOps[F[_[_], _], M[_], A](val fa: F[M, A])(implicit F: GFunctor[F]) {
    def gmap[N[_]](nt: M ~> N): F[N, A] = F.gmap(nt)(fa)
  }
}

case class GCofree[F[_[_], _], A[_], I](head: A[I], tail: F[GCofree[F, A, ?], I]) { 
  import GCofree._
  def gmap[B[_]](nt: A ~> B)(implicit G: GFunctor[F]): GCofree[F, B, I] = {
    GCofree(nt(head), G.gmap[GCofree[F, A, ?], GCofree[F, B, ?]](gnt[F, A, B](nt))(tail))
  }

  def kmap[G[_[_], _]](hont: HONatTrans[F, G])(implicit GF: GFunctor[F], GG: GFunctor[G]): GCofree[G, A, I] = {
    val nt: F[GCofree[F, A, ?], ?] ~> F[GCofree[G, A, ?], ?] = 
      GF.gmap[GCofree[F, A, ?], GCofree[G, A, ?]](knt[F, G, A](hont))

    GCofree(head, hont[GCofree[G, A, ?], I](nt(tail)))
  }
}

object GCofree {
  type GFix[F[_[_], _], A] = GCofree[F, Const[Unit, ?], A]

  def gfix[F[_[_], _], A](fga: F[GFix[F, ?], A]): GFix[F, A] = GCofree[F, Const[Unit, ?], A](Const(Unit), fga)

  implicit def instances[F[_[_], _]: GFunctor]: GFunctor[GCofree[F, ?[_], ?]] = new GFunctor[GCofree[F, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N): GCofree[F, M, ?] ~> GCofree[F, N, ?] = gnt(nt)
  }

  def gnt[F[_[_], _]: GFunctor, A[_], B[_]](nt: A ~> B): GCofree[F, A, ?] ~> GCofree[F, B, ?] = 
    new NaturalTransformation[GCofree[F, A, ?], GCofree[F, B, ?]] {
      def apply[I](gcf: GCofree[F, A, I]) = gcf.gmap(nt)
    }

  def knt[F[_[_], _], G[_[_], _], A[_]](hont: HONatTrans[F, G])(implicit GF: GFunctor[F], GG: GFunctor[G]): GCofree[F, A, ?] ~> GCofree[G, A, ?] =
    new NaturalTransformation[GCofree[F, A, ?], GCofree[G, A, ?]] {
      def apply[I0](gcf: GCofree[F, A, I0]): GCofree[G, A, I0] = gcf.kmap[G](hont)
    }
}
