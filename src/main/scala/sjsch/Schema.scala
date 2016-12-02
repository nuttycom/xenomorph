package sjsch

import scalaz.~>
import scalaz.Functor
import scalaz.FreeAp
import scalaz.NaturalTransformation
import scalaz.Need

sealed trait SchemaF[F[_], A, +E]

object Schema {
  type Schema[A, E] = GFix[({ type λ[ƒ[_], α] = SchemaF[ƒ, α, E] })#λ, A]
}

object SchemaF {
  implicit def instances[E]: GFunctor[({ type λ[ƒ[_], α] = SchemaF[ƒ, α, E] })#λ] = new GFunctor[({ type λ[ƒ[_], α] = SchemaF[ƒ, α, E] })#λ] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[M, ?, E], SchemaF[N, ?, E]] {
      def apply[A](fa: SchemaF[M, A, E]) = fa match {
        case BoolSchema()   => BoolSchema[N]
        case DoubleSchema() => DoubleSchema[N]
        case IntSchema()    => IntSchema[N]
        case StrSchema()    => StrSchema[N]
        case ConstSchema(a) => ConstSchema[N, A](a)

        case ObjectSchema(free) => ???
        case ArraySchema(elemSchema) => ???
        case OneOfSchema(alternatives) => ???
        case ParseSchema(base, f, g) => ???
        case LazySchema(s) => ???
      }
    }
  }
}

case class BoolSchema[F[_]]() extends SchemaF[F, Boolean, Nothing] 
case class DoubleSchema[F[_]]() extends SchemaF[F, Double, Nothing] 
case class IntSchema[F[_]]() extends SchemaF[F, Int, Nothing] 
case class StrSchema[F[_]]() extends SchemaF[F, String, Nothing] 
case class ConstSchema[F[_], A](a: A) extends SchemaF[F, A, Nothing] 

case class ObjectSchema[F[_], A, +E](free: FreeAp[({ type λ[α] = PropSchema[α, F, α] })#λ, A]) extends SchemaF[F, A, E]
case class ArraySchema[F[_], A, +E](elemSchema: F[A]) extends SchemaF[F, A, E]
case class OneOfSchema[F[_], A, +E](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[F, A, E]
case class ParseSchema[F[_], A, B, +E](base: F[A], f: A => ParseResult[A, B, E], g: B => A) extends SchemaF[F, B, E]
case class LazySchema[F[_], A, +E](s: Need[F[A]]) extends SchemaF[F, A, E]


sealed trait ParseResult[S, A, +E]
case class PSuccess[S, A, +E](a: A) extends ParseResult[S, A, E]
case class PFailure[S, A, +E](e: E, s: S) extends ParseResult[S, A, E]

case class Alternative[F[_], A, B](id: String, base: F[B], f: B => A, g: A => Option[B])

sealed trait PropSchema[O, F[_], A];
case class Required[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => A) extends PropSchema[O, F, A]
case class Optional[O, F[_], A](fieldName: String, valueSchema: F[A], accessor: O => Option[A]) extends PropSchema[O, F, Option[A]]


