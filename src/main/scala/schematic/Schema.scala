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
  /** The type of unannotated schema.
   *
   *  @tparam P The GADT type constructor for a sum type which defines the set of
   *          primitive types used in the schema. 
   *  @tparam A The type of the Scala value to be produced (or consumed)
   *          by an interpreter of the schema.
   */
  type Schema[P[_], A] = HCofree.HFix[SchemaF[P, ?[_], ?], A]

  /** The type of free applicative builders for record properties
   *
   *  @tparam P The GADT type constructor for a sum type which defines the set of
   *          primitive types used in the schema. 
   *  @tparam A The type of the Scala value to be produced (or consumed)
   *          by an interpreter of the schema.
   */
  type Props[P[_], A] = FreeAp[PropSchema[A, Schema[P, ?], ?], A]

  def schema[P[_], A](sf: SchemaF[P, Schema[P, ?], A]): Schema[P, A] = 
    HCofree.hfix[SchemaF[P, ?[_], ?], A](sf)

  def prim[P[_], A](pa: P[A]): Schema[P, A] = 
    schema(PrimSchema[P, Schema[P, ?], A](pa))

  def rec[P[_], A](props: Props[P, A]): Schema[P, A] = 
    schema(RecordSchema[P, Schema[P, ?], A](props))

  def empty[P[_]]: Schema[P, Unit] = 
    rec[P, Unit](FreeAp.pure[PropSchema[Unit, Schema[P, ?], ?], Unit](Unit))

  def oneOf[P[_], A](alternatives: List[Alternative[Schema[P, ?], A, B] forSome { type B }]): Schema[P, A] = 
    schema(OneOfSchema[P, Schema[P, ?], A](alternatives))

  def lazily[P[_], A](s: => Schema[P, A]): Schema[P, A] =
    schema(LazySchema[P, Schema[P, ?], A](Need(s)))
}
