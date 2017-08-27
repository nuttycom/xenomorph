package schematic

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.Need
import scalaz.syntax.functor._
import scalaz.std.anyVal._

import HFunctor._

/** Data types and smart constructors which simplify the creation
 *  of schema values.
 *
 *  @define PDefn The GADT type constructor for a sum type which defines 
 *          the set of primitive types used in the schema.
 *  @define IDefn The type of the Scala value to be produced (or consumed)
 *          by an interpreter of the schema. Also known as the "index" type
 *          of the schema.
 *  @define ODefn The type of a Scala record - an object or a tuple,
 *          the property of which is being defined.
 *  @define ADefn The type of the annotation applied to each node of the schema
 */
object Schema {
  /** The type of an annotated schema. 
   *
   *  This is an alias for the HCofree fixpoint applied to the SchemaF type constructor.
   *
   *  @tparam A The type of the annotation applied to each node of the schema.
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  type Schema[A, P[_], I] = HCofree[SchemaF[P, ?[_], ?], A, I]

  /** The type of free applicative values which are used to capture the structure
   *  of individual record properties.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   */
  type Prop[A, P[_], O, I] = FreeAp[PropSchema[O, Schema[A, P, ?], ?], I]

  /** The type of free applicative values which are used to capture the structure
   *  of record (product) types.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam R The type of the Scala value to be produced (or consumed)
   *          by an interpreter of the schema. This is usually the type
   *          of a record - an object or a tuple.
   */
  type Props[A, P[_], R] = Prop[A, P, R, R]

  implicit def propApplicative[A, P[_], O]: Applicative[Prop[A, P, O, ?]] =
    FreeAp.freeInstance[PropSchema[O, Schema[A, P, ?], ?]]

  /** Lifts a SchemaF value into an annotated Schema 
   *  
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema
   *  @param sf The value to be annotated
   *  @return the newly constructed schema value
   */
  def schema[A, P[_], I](a: A, sf: => SchemaF[P, Schema[A, P, ?], I]): Schema[A, P, I] = 
    HCofree.annotate[SchemaF[P, ?[_], ?], A, I](a, sf)

  /** Lifts a value in an algebra of primitives into an unannotated Schema
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param p a value of the `P` algebra
   *  @return the newly constructed schema value
   */
  def prim[P[_], I](p: P[I]): Schema[Unit, P, I] = 
    schema((), PrimSchema[P, Schema[Unit, P, ?], I](p))

  /** Lifts a value in an algebra of primitives into an annotated Schema
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema.
   *  @param p a value of the `P` algebra
   *  @return the newly constructed schema value
   */
  def annPrim[A, P[_], I](a: A, p: P[I]): Schema[A, P, I] = 
    schema(a, PrimSchema[P, Schema[A, P, ?], I](p))

  /** Builds an un-annotated schema for a record type from the free
   *  applicative capture of that record's structure.
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param props The free-applicative value that captures the structure 
   *    of the record type.
   */
  def rec[P[_], I](props: Props[Unit, P, I]): Schema[Unit, P, I] = 
    schema((), RecordSchema[P, Schema[Unit, P, ?], I](props))

  /** Smart constructor for required Prop instances.
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   *  @param fieldName name of the record property
   *  @param valueSchema schema for the record property's type
   *  @param accessor function from the record type to the property's value
   */
  def required[P[_], O, I](fieldName: String, valueSchema: Schema[Unit, P, I])(accessor: O => I): Prop[Unit, P, O, I] = {
    FreeAp.lift[PropSchema[O, Schema[Unit, P, ?], ?], I](
      Required[O, Schema[Unit, P, ?], I](fieldName, valueSchema, accessor)
    )
  }
  
  /** Smart constructor for optional Prop instances.
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   *  @param fieldName name of the record property
   *  @param valueSchema schema for the record property's type
   *  @param accessor function from the record type to the property's value
   */
  def optional[P[_], O, I](fieldName: String, valueSchema: Schema[Unit, P, I])(accessor: O => Option[I]): Prop[Unit, P, O, Option[I]] = {
    FreeAp.lift[PropSchema[O, Schema[Unit, P, ?], ?], Option[I]](
      Optional[O, Schema[Unit, P, ?], I](fieldName, valueSchema, accessor)
    )
  }

  /** Builds an annotated schema for a record type from the free
   *  applicative capture of that record's structure.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema.
   *  @param props The free-applicative value that captures the structure 
   *    of the record type.
   */
  def annRec[A, P[_], I](a: A, props: Props[A, P, I]): Schema[A, P, I] = 
    schema(a, RecordSchema[P, Schema[A, P, ?], I](props))

  /** The unannotated empty record schema.
   *
   *  @tparam P $PDefn
   */
  def empty[P[_]]: Schema[Unit, P, Unit] = 
    rec[P, Unit](FreeAp.pure[PropSchema[Unit, Schema[Unit, P, ?], ?], Unit](Unit))

  /** Builds an un-annotated schema for the sum type `I` from a list of alternatives. 
   *
   *  Each alternative value in the list describes a single constructor of `I`.
   *  For example, to construct the schema for [[scala.util.Either]] one would provide
   *  two alternatives, one for the `Left` constructor and one for `Right`.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  def oneOf[P[_], I](alts: List[Alt[Schema[Unit, P, ?], I, _]]): Schema[Unit, P, I]
   *  }}}
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  def oneOf[P[_], I](alts: List[Alt[Schema[Unit, P, ?], I, J] forSome {type J}]): Schema[Unit, P, I] = 
    schema((), OneOfSchema[P, Schema[Unit, P, ?], I](alts))

  /** Builds an annotated schema for the sum type `I` from a list of alternatives. 
   *
   *  Each alternative value in the list describes a single constructor of `I`.
   *  For example, to construct the schema for [[scala.util.Either]] one would provide
   *  two alternatives, one for the `Left` constructor and one for `Right`.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  def oneOf[P[_], I](alts: List[Alt[Schema[Unit, P, ?], I, _]]): Schema[Unit, P, I]
   *  }}}
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  def annOneOf[A, P[_], I](a: A, alts: List[Alt[Schema[A, P, ?], I, J] forSome {type J}]): Schema[A, P, I] = 
    schema(a, OneOfSchema[P, Schema[A, P, ?], I](alts))

  /** Convenience constructor for oneOf schema alternatives.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @tparam J The type of the base value which can be mapped into the `I` algebra.
   */
  def alt[A, P[_], I, J](id: String, base: Schema[A, P, J], review: J => I, preview: I => Option[J]) = 
    Alt[Schema[A, P, ?], I, J](id, base, review, preview)

  /** Constructs the HFunctor instance for a Schema.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  implicit def hfunctor[A]: HFunctor[Schema[A, ?[_], ?]]
   *  }}}
   *
   *  @tparam A $ADefn
   */
  implicit def hfunctor[A]: HFunctor[Schema[A, ?[_], ?]] = new HFunctor[Schema[A, ?[_], ?]] {
    def hfmap[P[_], Q[_]](nt: P ~> Q) = new (Schema[A, P, ?] ~> Schema[A, Q, ?]) { self =>
      def apply[I](s: Schema[A, P, I]): Schema[A, Q, I] = {
        val sf: SchemaF[Q, Schema[A, P, ?], I] = s.tail.value.pmap(nt)
        schema(s.head, sf.hfmap[Schema[A, Q, ?]](self))
      }
    }
  }
}

