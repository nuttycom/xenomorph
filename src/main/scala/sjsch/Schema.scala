package sjsch

import scalaz.~>
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.NaturalTransformation
import scalaz.Need
import scalaz.syntax.functor._

import GFunctor._

object Schema {
  type Schema[E, P[_], A] = GCofree.GFix[SchemaF[E, P, ?[_], ?], Const[A, ?]]
}

object SchemaF {
  implicit def instances[E, P[_]]: GFunctor[SchemaF[E, P, ?[_], ?]] = new GFunctor[SchemaF[E, P, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[E, P, M, ?], SchemaF[E, P, N, ?]] {
      def apply[A](fa: SchemaF[E, P, M, A]): SchemaF[E, P, N, A] = fa match {
        case PrimitiveSchema(pa) => PrimitiveSchema(pa)
        case ConstSchema(a) => ConstSchema(a)

        case ObjectSchema(free: FreeAp[PropSchema[A, M, ?], A]) => 
          ObjectSchema(free.hoist[PropSchema[A, N, ?]](PropSchema.instances[A].gmap[M, N](nt)))

        case ArraySchema(elemSchema)   => ArraySchema(nt(elemSchema))
        case OneOfSchema(alternatives) => OneOfSchema(alternatives.map(_.gmap(nt)))
        case ParseSchema(base, f, g)   => ParseSchema(nt(base), f, g)
        case LazySchema(s)             => LazySchema(s.map(nt))
      }
    }
  }
}

sealed trait SchemaF[E, P[_], F[_], A]

case class PrimitiveSchema[E, P[_], F[_], A](prim: P[A]) extends SchemaF[E, P, F, A] 

case class ConstSchema[E, P[_], F[_], A](a: A) extends SchemaF[E, P, F, A] 

case class ObjectSchema[E, P[_], F[_], A](free: FreeAp[PropSchema[A, F, ?], A]) extends SchemaF[E, P, F, A]

case class ArraySchema[E, P[_], F[_], A](elemSchema: F[A]) extends SchemaF[E, P, F, A]

case class OneOfSchema[E, P[_], F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[E, P, F, A]

case class ParseSchema[E, P[_], F[_], A, B](base: F[A], f: A => ParseResult[A, B, E], g: B => A) extends SchemaF[E, P, F, B]

case class LazySchema[E, P[_], F[_], A](s: Need[F[A]]) extends SchemaF[E, P, F, A]


sealed trait ParseResult[S, A, E]
case class PSuccess[S, A, E](a: A) extends ParseResult[S, A, E]
case class PFailure[S, A, E](e: E, s: S) extends ParseResult[S, A, E]

case class Alternative[F[_], A, B](id: String, base: F[B], f: B => A, g: A => Option[B]) {
  def gmap[G[_]](nt: F ~> G): Alternative[G, A, B] = Alternative(id, nt(base), f, g)
}

object PropSchema {
  implicit def instances[O] = new GFunctor[PropSchema[O, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[PropSchema[O, M, ?], PropSchema[O, N, ?]] {
      def apply[A](ps: PropSchema[O, M, A]): PropSchema[O, N, A] = ps.gmap(nt)
    }
  }
}

sealed trait PropSchema[O, F[_], A] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, A]
}

case class Required[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => A) extends PropSchema[O, F, A] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, A] = Required(fieldName, nt(valueSchema), accessor)
}

case class Optional[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => Option[A]) extends PropSchema[O, F, Option[A]] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[A]] = Optional(fieldName, nt(valueSchema), accessor)
}


