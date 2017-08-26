package schematic

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.NaturalTransformation
import scalaz.Need
import scalaz.syntax.functor._
import scalaz.std.anyVal._

import HFunctor._

sealed trait SchemaF[P[_], F[_], A] {
  def hfmap[G[_]](nt: F ~> G): SchemaF[P, G, A]
  def pmap[Q[_]](nt: P ~> Q): SchemaF[Q, F, A]
}

object SchemaF {
  implicit def hfunctor[P[_]]: HFunctor[SchemaF[P, ?[_], ?]] = new HFunctor[SchemaF[P, ?[_], ?]] {
    def hfmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[P, M, ?], SchemaF[P, N, ?]] {
      def apply[A](fa: SchemaF[P, M, A]): SchemaF[P, N, A] = fa.hfmap(nt)
    }
  }
}

case class PrimSchema[P[_], F[_], A](prim: P[A]) extends SchemaF[P, F, A] {
  def hfmap[G[_]](nt: F ~> G) = PrimSchema[P, G, A](prim)
  def pmap[Q[_]](nt: P ~> Q) = PrimSchema[Q, F, A](nt(prim))
}

case class OneOfSchema[P[_], F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[P, F, A] {
  def hfmap[G[_]](nt: F ~> G) = OneOfSchema[P, G, A](alternatives.map(_.hfmap(nt)))
  def pmap[Q[_]](nt: P ~> Q) = OneOfSchema[Q, F, A](alternatives)
}

case class Alternative[F[_], A, B](id: String, base: F[B], f: B => A, g: A => Option[B]) {
  def hfmap[G[_]](nt: F ~> G): Alternative[G, A, B] = Alternative(id, nt(base), f, g)
}

case class RecordSchema[P[_], F[_], A](props: FreeAp[PropSchema[A, F, ?], A]) extends SchemaF[P, F, A] {
  def hfmap[G[_]](nt: F ~> G) = RecordSchema[P, G, A](props.hoist[PropSchema[A, G, ?]](PropSchema.instances[A].hfmap[F, G](nt)))
  def pmap[Q[_]](nt: P ~> Q) = RecordSchema[Q, F, A](props)
}


sealed trait PropSchema[O, F[_], A] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, A]
}

case class Required[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => A) extends PropSchema[O, F, A] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, A] = Required(fieldName, nt(valueSchema), accessor)
}

case class Optional[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => Option[A]) extends PropSchema[O, F, Option[A]] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[A]] = Optional(fieldName, nt(valueSchema), accessor)
}

object PropSchema {
  implicit def instances[O] = new HFunctor[PropSchema[O, ?[_], ?]] {
    def hfmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[PropSchema[O, M, ?], PropSchema[O, N, ?]] {
      def apply[A](ps: PropSchema[O, M, A]): PropSchema[O, N, A] = ps.hfmap(nt)
    }
  }
}

case class LazySchema[P[_], F[_], A](s: Need[F[A]]) extends SchemaF[P, F, A] {
  def hfmap[G[_]](nt: F ~> G) = LazySchema[P, G, A](s.map(nt))
  def pmap[Q[_]](nt: P ~> Q) = LazySchema[Q, F, A](s)
}

