/*
 * Copyright (C) 2017 Kris Nuttycombe
 * All rights reserved.
 *
 * This file is part of the Scala Xenomorph library.
 *
 * GNU Lesser General Public License Usage
 * This file may be used under the terms of the GNU Lesser
 * General Public License version 3.0 as published by the Free Software
 * Foundation and appearing in the file LICENSE included in the
 * packaging of this file.  Please review the following information to
 * ensure the GNU Lesser General Public License version 3.0 requirements
 * will be met: https://www.gnu.org/licenses/lgpl-3.0.txt
 */
package xenomorph

import scalaz.~>
import scalaz.Applicative
import scalaz.Profunctor
import scalaz.FreeAp

import monocle.Getter
import monocle.Prism

import xenomorph.HFix._
import xenomorph.HFunctor._

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
  /** The type of an unannotated schema. 
   *
   *  This is an alias for the HFix fixpoint applied to the SchemaF type constructor.
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  type Schema[P[_], I] = HFix[SchemaF[P, ?[_], ?], I]

  /** The type of an annotated schema. 
   *
   *  This is an alias for the HCofree fixpoint applied to the SchemaF type constructor.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  //type AnnSchema[A, P, I] = HCofree[SchemaF[P, ?[_], ?], A, I]

  /** The type of free applicative values which are used to capture the structure
   *  of individual record properties.
   *
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   */
  type Prop[P[_], O, I] = FreeAp[PropSchema[O, Schema[P, ?], ?], I]

  /** The type of free applicative values which are used to capture the structure
   *  of record (product) types.
   *
   *  @tparam P $PDefn
   *  @tparam R The type of the Scala value to be produced (or consumed)
   *          by an interpreter of the schema. This is usually the type
   *          of a record - an object or a tuple.
   */
  type Props[P[_], R] = Prop[P, R, R]

  implicit def propApplicative[P[_], O]: Applicative[Prop[P, O, ?]] =
    FreeAp.freeInstance[PropSchema[O, Schema[P, ?], ?]]

  /** Lifts a SchemaF value into an unannotated Schema 
   *  
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema
   *  @param sf The value to be annotated
   *  @return the newly constructed schema value
   */
  def schema[P[_], I](sf: => SchemaF[P, Schema[P, ?], I]): Schema[P, I] = 
    hfix[SchemaF[P, ?[_], ?], I](sf)

  /** Lifts a SchemaF value into an annotated Schema 
   *  
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema
   *  @param sf The value to be annotated
   *  @return the newly constructed schema value
   */
  //def aschema[A, P[_], I](a: A, sf: => SchemaF[P, Schema[P, ?], I]): Schema[P, I] = 
  //  hcofree[SchemaF[P, ?[_], ?], A, I](a, sf)

  /** Lifts a value in an algebra of primitives into an unannotated Schema
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param p a value of the `P` algebra
   *  @return the newly constructed schema value
   */
  def prim[P[_], I](p: P[I]): Schema[P, I] = 
    schema(PrimSchema[P, Schema[P, ?], I](p))

  /** Lifts a value in an algebra of primitives into an annotated Schema
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema.
   *  @param p a value of the `P` algebra
   *  @return the newly constructed schema value
   */
  //def annPrim[P[_], I](a: A, p: P[I]): Schema[A, P, I] = 
  //  aschema(a, PrimSchema[P, Schema[A, P, ?], I](p))

  /** Builds an un-annotated schema for a record type from the free
   *  applicative capture of that record's structure.
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param props The free-applicative value that captures the structure 
   *         of the record type.
   */
  def rec[P[_], I](props: Props[P, I]): Schema[P, I] = 
    schema(RecordSchema[P, Schema[P, ?], I](props))

  /** Builds an annotated schema for a record type from the free
   *  applicative capture of that record's structure.
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @param a The annotation value to attach to the schema.
   *  @param props The free-applicative value that captures the structure 
   *         of the record type.
   */
  //def annRec[A, P[_], I](a: A, props: Props[A, P, I]): Schema[A, P, I] = 
  //  aschema(a, RecordSchema[P, Schema[A, P, ?], I](props))

  /** Smart constructor for required Prop instances.
   *
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   *  @param fieldName name of the record property
   *  @param valueSchema schema for the record property's type
   *  @param getter Getter lens from the record type to the property's value
   */
  def required[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], getter: Getter[O, I]): Prop[P, O, I] = {
    FreeAp.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, getter, None)
    )
  }

  /** Smart constructor for required Prop instances, with a default
   *  provided for the case where a serialized form is missing the 
   *  required field.
   *
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   *  @param fieldName Name of the record property
   *  @param valueSchema Schema for the record property's type
   *  @param default Default value for use in the case that a serialized form
   *         is missing the required field.
   *  @param getter Getter lens from the record type to the property's value
   */
  def property[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], default: I, getter: Getter[O, I]): Prop[P, O, I] = {
    FreeAp.lift[PropSchema[O, Schema[P, ?], ?], I](
      Required[O, Schema[P, ?], I](fieldName, valueSchema, getter, Some(default))
    )
  }
  
  /** Smart constructor for optional Prop instances.
   *  @tparam P $PDefn
   *  @tparam O $ODefn
   *  @tparam I $IDefn
   *  @param fieldName name of the record property
   *  @param valueSchema schema for the record property's type
   *  @param getter Getter lens from the record type to the property's value
   */
  def optional[P[_], O, I](fieldName: String, valueSchema: Schema[P, I], getter: Getter[O, Option[I]]): Prop[P, O, Option[I]] = {
    FreeAp.lift[PropSchema[O, Schema[P, ?], ?], Option[I]](
      Optional[O, Schema[P, ?], I](fieldName, valueSchema, getter)
    )
  }

  /** The unannotated empty record schema.
   *
   *  @tparam P $PDefn
   */
  def empty[P[_]]: Schema[P, Unit] = 
    rec[P, Unit](FreeAp.pure[PropSchema[Unit, Schema[P, ?], ?], Unit](()))

  /** Builds an un-annotated schema for the sum type `I` from a list of alternatives. 
   *
   *  Each alternative value in the list describes a single constructor of `I`.
   *  For example, to construct the schema for [[scala.util.Either]] one would provide
   *  two alternatives, one for the `Left` constructor and one for `Right`.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  def oneOf[P[_], I](alts: List[Alt[Schema[P, ?], I, _]]): Schema[P, I]
   *  }}}
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  def oneOf[P[_], I](alts: List[Alt[Schema[P, ?], I, J] forSome {type J}]): Schema[P, I] = 
    schema(OneOfSchema[P, Schema[P, ?], I](alts))

  /** Builds an annotated schema for the sum type `I` from a list of alternatives. 
   *
   *  Each alternative value in the list describes a single constructor of `I`.
   *  For example, to construct the schema for [[scala.util.Either]] one would provide
   *  two alternatives, one for the `Left` constructor and one for `Right`.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  def oneOf[P[_], I](alts: List[Alt[Schema[P, ?], I, _]]): Schema[P, I]
   *  }}}
   *
   *  @tparam A $ADefn
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   */
  //def annOneOf[A, P[_], I](a: A, alts: List[Alt[Schema[A, P, ?], I, J] forSome {type J}]): Schema[A, P, I] = 
  //  schema(a, OneOfSchema[P, Schema[A, P, ?], I](alts))

  /** Convenience constructor for oneOf schema alternatives.
   *
   *  @tparam P $PDefn
   *  @tparam I $IDefn
   *  @tparam J The type of the base value which can be mapped into the `I` algebra.
   *  @param id The unique identifier of the constructor
   *  @param base The schema for the `J` type
   *  @param prism Prism between the sum type and the selected constructor.
   */
  def alt[P[_], I, J](id: String, base: Schema[P, J], prism: Prism[I, J]) = 
    Alt[Schema[P, ?], I, J](id, base, prism)

  /** HAlgebra for primitive type constructor transformation.
   */
  def hfmapAlg[P[_], Q[_]](nt: P ~> Q) = new HAlgebra[SchemaF[P, ?[_], ?], Schema[Q, ?]] {
    def apply[I](s: SchemaF[P, Schema[Q, ?], I]): Schema[Q, I] = hfix(s.pmap(nt))
  }

  /** Constructs the HFunctor instance for a Schema.
   *
   *  An easier-to-read type signature for this function is below:
   *
   *  {{{
   *  implicit def hfunctor: HFunctor[Schema]
   *  }}}
   */
  implicit def hfunctor: HFunctor[Schema] = new HFunctor[Schema] {
    def hfmap[P[_], Q[_]](nt: P ~> Q) = cataNT(hfmapAlg(nt))
  }

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
  //implicit def hfunctor[A]: HFunctor[Schema[A, ?[_], ?]] = new HFunctor[Schema[A, ?[_], ?]] {
  //  def hfmap[P[_], Q[_]](nt: P ~> Q) = new (Schema[A, P, ?] ~> Schema[A, Q, ?]) { self =>
  //    def apply[I](s: Schema[A, P, I]): Schema[A, Q, I] = {
  //      val sf: SchemaF[Q, Schema[A, P, ?], I] = s.hfix.value.fa.pmap(nt)
  //      schema(s.hfix.value.ask, sf.hfmap[Schema[A, Q, ?]](self))
  //    }
  //  }
  //}

  implicit def propProfunctor[P[_]]: Profunctor[Prop[P, ?, ?]] = new Profunctor[Prop[P, ?, ?]] {
    def mapfst[O, I, N](prop: Prop[P, O, I])(f: N => O): Prop[P, N, I] = prop.hoist[PropSchema[N, Schema[P, ?], ?]](
      PropSchema.contraNT[O, N, Schema[P, ?]](f)
    )

    def mapsnd[O, I, J](prop: Prop[P, O, I])(f: I => J): Prop[P, O, J] = prop.map(f)    
  }
}

