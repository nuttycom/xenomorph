package sjsch

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.NaturalTransformation
import scalaz.Need
import scalaz.syntax.functor._
import scalaz.std.anyVal._

import GFunctor._

object Schema {
  import ParseSchema._

  /**
   * @param E The error type for parse failures. 
   * @param P The GADT type constructor for a sum type which defines the set of
   *        primitive types used in the schema. 
   * @param A The type of the Scala value to be produced (or consumed)
   *        by an interpreter of the schema.
   */
  type Schema[E, P[_], A] = GCofree.GFix[SchemaF[E, P, ?[_], ?], A]

  type ObjectBuilder[E, P[_], A] = FreeAp[PropSchema[A, Schema[E, P, ?], ?], A]

  def schema[E, P[_], A](sf: SchemaF[E, P, Schema[E, P, ?], A]): Schema[E, P, A] = 
    GCofree.gfix[SchemaF[E, P, ?[_], ?], A](sf)

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

  implicit def instances[P[_], A]: Functor[Schema[?, P, A]] = new Functor[Schema[?, P, A]] {
    def map[E, E0](fa: Schema[E, P, A])(f: E => E0): Schema[E0, P, A] = {
      fa.kmap[SchemaF[E0, P, ?[_], ?]](SchemaF.errorHONT[E, E0, P](f))
    }
  }
}

object SchemaF {
  implicit def gfa[E, P[_]]: GFunctor[SchemaF[E, P, ?[_], ?]] = new GFunctor[SchemaF[E, P, ?[_], ?]] {
    def gmap[M[_], N[_]](nt: M ~> N) = new NaturalTransformation[SchemaF[E, P, M, ?], SchemaF[E, P, N, ?]] {
      def apply[A](fa: SchemaF[E, P, M, A]): SchemaF[E, P, N, A] = fa.gmap(nt)
    }
  }

  implicit def fe[P[_], F[_], A]: Functor[SchemaF[?, P, F, A]] = new Functor[SchemaF[?, P, F, A]] {
    def map[E, E0](fa: SchemaF[E, P, F, A])(f: E => E0): SchemaF[E0, P, F, A] = fa.map(f)
  }

  def errorHONT[E, E0, P[_]](f: E => E0): HONatTrans[SchemaF[E, P, ?[_], ?], SchemaF[E0, P, ?[_], ?]] = 
    new HONatTrans[SchemaF[E, P, ?[_], ?], SchemaF[E0, P, ?[_], ?]] {
      def apply[M[_], A](fe: SchemaF[E, P, M, A]) = fe.map(f)
    }

  def primHONT[E, P[_], Q[_]](nt: P ~> Q): HONatTrans[SchemaF[E, P, ?[_], ?], SchemaF[E, Q, ?[_], ?]] =
    new HONatTrans[SchemaF[E, P, ?[_], ?], SchemaF[E, Q, ?[_], ?]] {
      def apply[M[_], A](fe: SchemaF[E, P, M, A]) = fe.pmap(nt)
    }
}

trait SchemaFAlg[Repr[_[_], _]] {
  def prim[E, P[_], F[_], A](prim: P[A]): Repr[F, A]
  def product[E, P[_], F[_], A](builder: FreeAp[PropSchema[A, F, ?], A]): Repr[F, A]
  def sum[E, P, F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]): Repr[F, A]
}

sealed abstract class SchemaF[E, P[_], F[_], A] {
  def map[E0](f: E => E0): SchemaF[E0, P, F, A]
  def gmap[G[_]](nt: F ~> G): SchemaF[E, P, G, A]
  def pmap[Q[_]](nt: P ~> Q): SchemaF[E, Q, F, A]
}

final case class PrimitiveSchema[E, P[_], F[_], A](prim: P[A]) extends SchemaF[E, P, F, A] {
  def map[E0](f: E => E0)    = PrimitiveSchema[E0, P, F, A](prim)
  def gmap[G[_]](nt: F ~> G) = PrimitiveSchema[E, P, G, A](prim)
  def pmap[Q[_]](nt: P ~> Q) = PrimitiveSchema[E, Q, F, A](nt(prim))
}

final case class ObjectSchema[E, P[_], F[_], A](builder: FreeAp[PropSchema[A, F, ?], A]) extends SchemaF[E, P, F, A] {
  def map[E0](f: E => E0)    = ObjectSchema[E0, P, F, A](builder)
  def gmap[G[_]](nt: F ~> G) = ObjectSchema[E, P, G, A](builder.hoist[PropSchema[A, G, ?]](PropSchema.instances[A].gmap[F, G](nt)))
  def pmap[Q[_]](nt: P ~> Q) = ObjectSchema[E, Q, F, A](builder)
}

final case class ArraySchema[E, P[_], F[_], A](elemSchema: F[A]) extends SchemaF[E, P, F, List[A]] {
  def map[E0](f: E => E0)    = ArraySchema[E0, P, F, A](elemSchema)
  def gmap[G[_]](nt: F ~> G) = ArraySchema[E, P, G, A](nt(elemSchema))
  def pmap[Q[_]](nt: P ~> Q) = ArraySchema[E, Q, F, A](elemSchema)
}

final case class OneOfSchema[E, P[_], F[_], A](alternatives: List[Alternative[F, A, B] forSome { type B }]) extends SchemaF[E, P, F, A] {
  def map[E0](f: E => E0)    = OneOfSchema[E0, P, F, A](alternatives)
  def gmap[G[_]](nt: F ~> G) = OneOfSchema[E, P, G, A](alternatives.map(_.gmap(nt)))
  def pmap[Q[_]](nt: P ~> Q) = OneOfSchema[E, Q, F, A](alternatives)
}

final case class ParseSchema[E, P[_], F[_], A, B](base: F[A], parser: A => ParseSchema.ParseResult[E, A, B], g: B => A) extends SchemaF[E, P, F, B] {
  def map[E0](f: E => E0)    = ParseSchema[E0, P, F, A, B](base, parser.andThen(_.mapError(f)), g)
  def gmap[G[_]](nt: F ~> G) = ParseSchema[E, P, G, A, B](nt(base), parser, g)
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

final case class LazySchema[E, P[_], F[_], A](s: Need[F[A]]) extends SchemaF[E, P, F, A] {
  def map[E0](f: E => E0)    = LazySchema[E0, P, F, A](s)
  def gmap[G[_]](nt: F ~> G) = LazySchema[E, P, G, A](s.map(nt))
  def pmap[Q[_]](nt: P ~> Q) = LazySchema[E, Q, F, A](s)
}

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


