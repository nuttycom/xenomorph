package sjsch

import scalaz.~>
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.NaturalTransformation
import scalaz.Need

import GFunctor._

object Schema {
  type Schema[E, A] = GCofree.GFix[SchemaF[E, ?[_], ?], Const[A, ?]]
}

object SchemaF {
  implicit def instances[E]: GFunctor[SchemaF[E, ?[_], ?]] = new GFunctor[SchemaF[E, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[E, M, ?], SchemaF[E, N, ?]] {
      def apply[A](fa: SchemaF[E, M, A]): SchemaF[E, N, A] = fa match {
        case BoolSchema()   => BoolSchema[E, N]
        case DoubleSchema() => DoubleSchema[E, N]
        case IntSchema()    => IntSchema[E, N]
        case StrSchema()    => StrSchema[E, N]
        case ConstSchema(a) => ConstSchema[E, N, A](a)

        case ObjectSchema(free: FreeAp[PropSchema[A, M, ?], A]) => 
          ObjectSchema(free.hoist[PropSchema[A, N, ?]](PropSchema.instances[A].gmap[M, N](nt)))
        case ArraySchema(elemSchema) => ???
        case OneOfSchema(alternatives) => ???
        case ParseSchema(base, f, g) => ???
        case LazySchema(s) => ???
      }
    }
  }
}

sealed trait SchemaF[E, F[_], A]
case class BoolSchema[E, F[_]]() extends SchemaF[E, F, Boolean] 
case class DoubleSchema[E, F[_]]() extends SchemaF[E, F, Double] 
case class IntSchema[E, F[_]]() extends SchemaF[E, F, Int] 
case class StrSchema[E, F[_]]() extends SchemaF[E, F, String] 
case class ConstSchema[E, F[_], A](a: A) extends SchemaF[E, F, A] 

case class ObjectSchema[E, F[_], A](free: FreeAp[PropSchema[A, F, ?], A]) extends SchemaF[E, F, A]
case class ArraySchema[E, F[_], A](elemSchema: F[A]) extends SchemaF[E, F, A]
case class OneOfSchema[E, F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[E, F, A]
case class ParseSchema[E, F[_], A, B](base: F[A], f: A => ParseResult[A, B, E], g: B => A) extends SchemaF[E, F, B]
case class LazySchema[E, F[_], A](s: Need[F[A]]) extends SchemaF[E, F, A]


sealed trait ParseResult[S, A, E]
case class PSuccess[S, A, E](a: A) extends ParseResult[S, A, E]
case class PFailure[S, A, E](e: E, s: S) extends ParseResult[S, A, E]

case class Alternative[F[_], A, B](id: String, base: F[B], f: B => A, g: A => Option[B])

sealed trait PropSchema[O, F[_], A] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, A]
}

object PropSchema {
  implicit def instances[O] = new GFunctor[PropSchema[O, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[PropSchema[O, M, ?], PropSchema[O, N, ?]] {
      def apply[A](ps: PropSchema[O, M, A]): PropSchema[O, N, A] = ps.gmap(nt)
    }
  }
}

case class Required[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => A) extends PropSchema[O, F, A] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, A] = Required(fieldName, nt(valueSchema), accessor)
}

case class Optional[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => Option[A]) extends PropSchema[O, F, Option[A]] {
  def gmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[A]] = Optional(fieldName, nt(valueSchema), accessor)
}


