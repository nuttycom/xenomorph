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

object Schema {
  import ParseSchema._

  /**
   * @param E The error type for parse failures. 
   * @param P The GADT type constructor for a sum type which defines the set of
   *        primitive types used in the schema. 
   * @param A The type of the Scala value to be produced (or consumed)
   *        by an interpreter of the schema.
   */
  type Schema[E, P[_], A] = HCofree.HFix[SchemaF[E, P, ?[_], ?], A]

  type ObjectBuilder[E, P[_], A] = FreeAp[PropSchema[A, Schema[E, P, ?], ?], A]

  def schema[E, P[_], A](sf: SchemaF[E, P, Schema[E, P, ?], A]): Schema[E, P, A] = 
    HCofree.hfix[SchemaF[E, P, ?[_], ?], A](sf)

  def prim[E, P[_], A](pa: P[A]): Schema[E, P, A] = 
    schema(PrimitiveSchema[E, P, Schema[E, P, ?], A](pa))

  def obj[E, P[_], A](builder: ObjectBuilder[E, P, A]): Schema[E, P, A] = 
    schema(ObjectSchema[E, P, Schema[E, P, ?], A](builder))

  def arr[E, P[_], A](elemSchema: Schema[E, P, A]): Schema[E, P, List[A]] = 
    schema(ArraySchema[E, P, Schema[E, P, ?], A](elemSchema))

  def oneOf[E, P[_], A](alternatives: List[Alternative[Schema[E, P, ?], A, B] forSome { type B }]): Schema[E, P, A] = 
    schema(OneOfSchema[E, P, Schema[E, P, ?], A](alternatives))

  def parser[E, P[_], A, B](base: Schema[E, P, A], f: A => ParseResult[E, A, B], g: B => A): Schema[E, P, B] =
    schema(ParseSchema[E, P, Schema[E, P, ?], A, B](base, f, g))

  def iso[E, P[_], A, B](base: Schema[E, P, A], f: A => B, g: B => A): Schema[E, P, B] = 
    parser(base, f.andThen(PSuccess[E, A, B](_)), g)

  def lazily[E, P[_], A](s: => Schema[E, P, A]): Schema[E, P, A] =
    schema(LazySchema[E, P, Schema[E, P, ?], A](Need(s)))
}

sealed trait SchemaF[E, P[_], F[_], A] {
  def mapError[E0](f: E => E0): SchemaF[E0, P, F, A]
  def hfmap[G[_]](nt: F ~> G): SchemaF[E, P, G, A]
  def pmap[Q[_]](nt: P ~> Q): SchemaF[E, Q, F, A]
}

object SchemaF {
  implicit def hfunctor[E, P[_]]: HFunctor[SchemaF[E, P, ?[_], ?]] = new HFunctor[SchemaF[E, P, ?[_], ?]] {
    def hfmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[E, P, M, ?], SchemaF[E, P, N, ?]] {
      def apply[A](fa: SchemaF[E, P, M, A]): SchemaF[E, P, N, A] = fa.hfmap(nt)
    }
  }
}

case class PrimitiveSchema[E, P[_], F[_], A](prim: P[A]) extends SchemaF[E, P, F, A] {
  def mapError[E0](f: E => E0) = PrimitiveSchema[E0, P, F, A](prim)
  def hfmap[G[_]](nt: F ~> G) = PrimitiveSchema[E, P, G, A](prim)
  def pmap[Q[_]](nt: P ~> Q) = PrimitiveSchema[E, Q, F, A](nt(prim))
}

case class ObjectSchema[E, P[_], F[_], A](builder: FreeAp[PropSchema[A, F, ?], A]) extends SchemaF[E, P, F, A] {
  def mapError[E0](f: E => E0) = ObjectSchema[E0, P, F, A](builder)
  def hfmap[G[_]](nt: F ~> G) = ObjectSchema[E, P, G, A](builder.hoist[PropSchema[A, G, ?]](PropSchema.instances[A].hfmap[F, G](nt)))
  def pmap[Q[_]](nt: P ~> Q) = ObjectSchema[E, Q, F, A](builder)
}

case class ArraySchema[E, P[_], F[_], A](elemSchema: F[A]) extends SchemaF[E, P, F, List[A]] {
  def mapError[E0](f: E => E0) = ArraySchema[E0, P, F, A](elemSchema)
  def hfmap[G[_]](nt: F ~> G) = ArraySchema[E, P, G, A](nt(elemSchema))
  def pmap[Q[_]](nt: P ~> Q) = ArraySchema[E, Q, F, A](elemSchema)
}

case class OneOfSchema[E, P[_], F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[E, P, F, A] {
  def mapError[E0](f: E => E0) = OneOfSchema[E0, P, F, A](alternatives)
  def hfmap[G[_]](nt: F ~> G) = OneOfSchema[E, P, G, A](alternatives.map(_.hfmap(nt)))
  def pmap[Q[_]](nt: P ~> Q) = OneOfSchema[E, Q, F, A](alternatives)
}

case class ParseSchema[E, P[_], F[_], A, B](base: F[A], parser: A => ParseSchema.ParseResult[E, A, B], g: B => A) extends SchemaF[E, P, F, B] {
  def mapError[E0](f: E => E0) = ParseSchema[E0, P, F, A, B](base, parser.andThen(_.mapError(f)), g)
  def hfmap[G[_]](nt: F ~> G) = ParseSchema[E, P, G, A, B](nt(base), parser, g)
  def pmap[Q[_]](nt: P ~> Q) = ParseSchema[E, Q, F, A, B](base, parser, g)
}

object ParseSchema {
  sealed trait ParseResult[E, S, A] {
    def mapError[E0](f: E => E0): ParseResult[E0, S, A]
  }

  case class PSuccess[E, S, A](a: A) extends ParseResult[E, S, A]  {
    def mapError[E0](f: E => E0) = PSuccess[E0, S, A](a)
  }

  case class PFailure[E, S, A](e: E, s: S) extends ParseResult[E, S, A] {
    def mapError[E0](f: E => E0) = PFailure[E0, S, A](f(e), s)
  }
}

case class LazySchema[E, P[_], F[_], A](s: Need[F[A]]) extends SchemaF[E, P, F, A] {
  def mapError[E0](f: E => E0)    = LazySchema[E0, P, F, A](s)
  def hfmap[G[_]](nt: F ~> G) = LazySchema[E, P, G, A](s.map(nt))
  def pmap[Q[_]](nt: P ~> Q) = LazySchema[E, Q, F, A](s)
}

case class Alternative[F[_], A, B](id: String, base: F[B], f: B => A, g: A => Option[B]) {
  def hfmap[G[_]](nt: F ~> G): Alternative[G, A, B] = Alternative(id, nt(base), f, g)
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

