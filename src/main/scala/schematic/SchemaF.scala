/*
 * Copyright (C) 2017 Kris Nuttycombe
 * All rights reserved.
 *
 * This file is part of the Scala Schematic library.
 *
 * GNU Lesser General Public License Usage
 * This file may be used under the terms of the GNU Lesser
 * General Public License version 3.0 as published by the Free Software
 * Foundation and appearing in the file LICENSE included in the
 * packaging of this file.  Please review the following information to
 * ensure the GNU Lesser General Public License version 3.0 requirements
 * will be met: https://www.gnu.org/licenses/lgpl-3.0.txt
 */
package schematic

import scalaz.~>
import scalaz.Applicative
import scalaz.Const
import scalaz.Functor
import scalaz.FreeAp
import scalaz.Need
import scalaz.syntax.functor._
import scalaz.std.anyVal._

import monocle.Getter
import monocle.Prism

import HFunctor._

/** The base trait for the schema GADT.
 *
 *  @define PDefn The GADT type constructor for a sum type which defines 
 *          the set of primitive types used in the schema.
 *  @define IDefn The type of the Scala value to be produced (or consumed)
 *          by an interpreter of the schema. Also known as the "index" type
 *          of the schema.
 *  @define FDefn The functor through which the structure of the schema will
 *          be interpreted. This will almost always be a fixpoint type such as
 *          [[schematic.HCofree]], which is used to introduce the ability to
 *          create recursive (tree-structured) schema.
 *
 *  @tparam P $PDefn
 *  @tparam F $FDefn
 *  @tparam I $IDefn
 */
sealed trait SchemaF[P[_], F[_], I] {
  /** HFunctor operation which allows transformation of the
   *  functor through which the structure of the schema will
   *  be interpreted.
   *
   *  Defining this operation directly on the SchemaF type
   *  rather than in [[schematic.SchemaF.hfunctor]] simplifies
   *  type inference.
   */
  def hfmap[G[_]](nt: F ~> G): SchemaF[P, G, I]

  /** HFunctor operation which allows transformation of the
   *  primitive algebra of the schema.
   *
   *  Defining this operation directly on the SchemaF type
   *  rather than in [[schematic.SchemaF.hfunctor]] simplifies
   *  type inference.
   */
  def pmap[Q[_]](nt: P ~> Q): SchemaF[Q, F, I]
}

object SchemaF {
  implicit def hfunctor[P[_]]: HFunctor[SchemaF[P, ?[_], ?]] = new HFunctor[SchemaF[P, ?[_], ?]] {
    def hfmap[M[_], N[_]](nt: M ~> N) = new (SchemaF[P, M, ?] ~> SchemaF[P, N, ?]) {
      def apply[I](fa: SchemaF[P, M, I]): SchemaF[P, N, I] = fa.hfmap(nt)
    }
  }
}

/** Schema constructor that wraps a value of an underlying GADT
 *  of allowed primitive types.
 *
 *  The underlying GADT defines a set of types via GADT constructors;
 *  see [[schematic.json.JType]] for an example. This set of types
 *  defines what types may be treated as primitive (and have parsing/
 *  serialization/etc deferred to an external handler) when interpreting
 *  a schema value. For example, one might want to construct a GADT for
 *  for the Scala primitive types as such:
 *
 *  {{{
 *  sealed trait SType[I]
 *
 *  case object SNullT   extends SType[Unit]
 *  case object SBoolT   extends SType[Boolean]
 *  
 *  case object SByteT   extends SType[Byte]
 *  case object SShortT  extends SType[Short]
 *  case object SIntT    extends SType[Int]
 *  case object SLongT   extends SType[Long]
 *  
 *  case object SFloatT  extends SType[Float]
 *  case object SDoubleT extends SType[Double]
 *  
 *  case object SCharT   extends SType[Char]
 *  case object SStrT    extends SType[String]
 *  }}}
 *
 *  This example treats String values as primitive as well, even though
 *  strictly speaking they're reference types, just because virtually 
 *  any interpreter for a schema algebra will not want to represent
 *  strings in terms of sum or product types. The same might hold true
 *  for, for example, [[scala.Array]] but for the purposes of this example
 *  issues related to `ClassManifest` instances would introduce excessive
 *  complexity.
 *
 *  @tparam P $PDefn
 *  @tparam F $FDefn
 *  @tparam I $IDefn
 *  @param prim value identifying a primitive type.
 */
case class PrimSchema[P[_], F[_], I](prim: P[I]) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G) = PrimSchema[P, G, I](prim)
  def pmap[Q[_]](nt: P ~> Q) = PrimSchema[Q, F, I](nt(prim))
}

/** Constructor that enables creation of schema for sum types.
 *
 *  Each constructor of the sum type `I` is represented as a member
 *  of the list of alternatives. Each alternative defines a prism
 *  between a single constructor of the sum type, and an underlying
 *  type describing the arguments demanded by that constructor.
 *
 *  Consider the following sum type. The first constructor takes
 *  no arguments; the second takes two.
 *
 *  {{{
 *  sealed trait Role
 *  
 *  case object User extends Role
 *  case class Administrator(department: String, subordinateCount: Int) extends Role
 *  }}}
 *
 *  A schema value for this type looks like:
 *
 *  {{{
 *  val roleSchema = oneOf(
 *    alt[Unit, Prim, Role, Unit](
 *      "user", 
 *      Schema.empty,
 *      (_: Unit) => User, 
 *      { 
 *        case User => Some(Unit)
 *        case _ => None
 *      }
 *    ) ::
 *    alt[Unit, Prim, Role, Administrator](
 *      "administrator", 
 *      rec[Prim, Administrator](
 *        ^[Schema.Prop[Unit, Prim, Administrator, ?], String, Int, Administrator](
 *          required("department", Prim.str, (_: Administrator).department),
 *          required("subordinateCount", Prim.int, (_: Administrator).subordinateCount)
 *        )(Administrator(_, _))
 *      ),
 *      identity,
 *      { 
 *        case a @ Administrator(_, _) => Some(a)
 *        case _ => None
 *      }
 *    ) :: Nil
 *  )
 *  }}}
 *
 *  @tparam P $PDefn
 *  @tparam F $FDefn
 *  @tparam I $IDefn
 */
case class OneOfSchema[P[_], F[_], I](alts: List[Alt[F, I, I0] forSome { type I0 }]) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G) = OneOfSchema[P, G, I](alts.map(_.hfmap(nt)))
  def pmap[Q[_]](nt: P ~> Q) = OneOfSchema[Q, F, I](alts)
}

/** A prism between a base type containing the arguments required by
 *  a single constructor of a sum type, and that sum type, along with
 *  the schema for the base type is used to describe those constructor
 *  arguments. The identifier is used to distinguish which constructor
 *  is being represented in the serialized form.
 *
 *  @define IDefn The type of the Scala value to be produced (or consumed)
 *          by an interpreter of the schema. Also known as the "index" type
 *          of the schema.
 *
 *  @define FDefn The functor through which the structure of the schema will
 *          be interpreted. This will almost always be a fixpoint type such as
 *          [[schematic.HCofree]], which is used to introduce the ability to
 *          create recursive (tree-structured) schema.
 *
 *  @tparam F $FDefn
 *  @tparam I $IDefn
 *  @tparam I0 The base type which corresponds to the arguments to
 *          the selected constructor.
 *  @param id The unique identifier of the constructor
 *  @param base The schema for the `I0` type
 *  @param prism Prism between the sum type and the selected constructor.
 */
case class Alt[F[_], I, I0](id: String, base: F[I0], prism: Prism[I, I0]) {
  def hfmap[G[_]](nt: F ~> G): Alt[G, I, I0] = Alt(id, nt(base), prism)
}

/** Wrapper for the free applicative structure which is used to construct
 *  and disassemble values of product types.
 *  
 *  @tparam P $PDefn
 *  @tparam F $FDefn
 *  @tparam I $IDefn
 *  @param props the free applicative value composed of zero or more PropSchema instances
 */
case class RecordSchema[P[_], F[_], I](props: FreeAp[PropSchema[I, F, ?], I]) extends SchemaF[P, F, I] {
  def hfmap[G[_]](nt: F ~> G) = RecordSchema[P, G, I](props.hoist[PropSchema[I, G, ?]](PropSchema.instances[I].hfmap[F, G](nt)))
  def pmap[Q[_]](nt: P ~> Q) = RecordSchema[Q, F, I](props)
}

/** Base trait for values which describe record properties. 
 *
 *  @define FDefn The functor through which the structure of the schema will
 *          be interpreted. This will almost always be a fixpoint type such as
 *          [[schematic.HCofree]], which is used to introduce the ability to
 *          create recursive (tree-structured) schema.
 *
 *  @tparam O The record type.
 *  @tparam F $FDefn
 *  @tparam A The type of the property value.
 */
sealed trait PropSchema[O, F[_], A] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, A]
}

/** Class describing a required property of a record.
 *
 * @param fieldName The name of the property.
 * @param valueSchema Schema for the property's value type.
 * @param getter Getter lens from the record type to the property.
 * @param default Optional default value, for use in the case that a
 *        serialized form is missing the property.
 */
case class Required[O, F[_], A](
  fieldName: String, 
  valueSchema: F[A], 
  getter: Getter[O, A], 
  default: Option[A]
) extends PropSchema[O, F, A] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, A] = 
    Required(fieldName, nt(valueSchema), getter, default)
}

/** Class describing an optional property of a record. Since in many
 *  serialized forms optional properties may be omitted entirely from
 *  the serialized form, a distinct type is needed in order to be able
 *  to correctly interpret the absence of a field.
 *
 * @param fieldName The name of the property.
 * @param valueSchema Schema for the property's value type.
 * @param getter Getter lens from the record type to the property.
 */
case class Optional[O, F[_], A](
  fieldName: String, 
  valueSchema: F[A], 
  getter: Getter[O, Option[A]]
) extends PropSchema[O, F, Option[A]] {
  def hfmap[G[_]](nt: F ~> G): PropSchema[O, G, Option[A]] = 
    Optional(fieldName, nt(valueSchema), getter)
}

object PropSchema {
  implicit def instances[O] = new HFunctor[PropSchema[O, ?[_], ?]] {
    def hfmap[M[_], N[_]](nt: M ~> N) = new (PropSchema[O, M, ?] ~> PropSchema[O, N, ?]) {
      def apply[A](ps: PropSchema[O, M, A]): PropSchema[O, N, A] = ps.hfmap(nt)
    }
  }
}
