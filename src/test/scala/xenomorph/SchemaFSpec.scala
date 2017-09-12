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

import scalaz.syntax.apply._

import org.specs2._
//import org.scalacheck._
//import org.scalacheck.Gen._

import monocle.Iso
import monocle.macros._

import org.joda.time.Instant

import xenomorph.Schema._

@Lenses case class Person(
  name: String, 
  birthDate: Instant,
  roles: Vector[Role]
)

sealed trait Role

case object User extends Role {
  val prism = GenPrism[Role, User.type]
}

@Lenses case class Administrator(department: String, subordinateCount: Int) extends Role
object Administrator {
  val prism = GenPrism[Role, Administrator]
}

sealed trait Prim[A]
case object StrPrim extends Prim[String]
case object IntPrim extends Prim[Int]
case object LongPrim extends Prim[Long]
case class ArrPrim[I](elemSchema: Schema[Prim, I]) extends Prim[Vector[I]]

object Prim {
  type PSchema[A] = Schema[Prim, A]
  def str: PSchema[String] = Schema.prim(StrPrim)
  def int: PSchema[Int] = Schema.prim(IntPrim)
  def long: PSchema[Long] = Schema.prim(LongPrim)
  def arr[A](elem: PSchema[A]): PSchema[Vector[A]] = 
    Schema.prim[Prim, Vector[A]](ArrPrim(elem))
}


class SchemaFSpec extends Specification with org.specs2.ScalaCheck {
  def is = """
  """

  type TProp[O, A] = Schema.Prop[Prim, O, A]

  val roleSchema: Schema[Prim, Role] = Schema.oneOf(
    alt[Prim, Role, User.type](
      "user", 
      Schema.const(User),
      User.prism
    ) ::
    alt[Prim, Role, Administrator](
      "administrator", 
      rec(
        ^(
          required("department", Prim.str, Administrator.department.asGetter),
          required("subordinateCount", Prim.int, Administrator.subordinateCount.asGetter)
        )(Administrator.apply _)
      ),
      Administrator.prism
    ) :: Nil
  )

  val personSchema: Schema[Prim, Person] = rec(
    ^^(
      required("name", Prim.str, Person.name.asGetter),
      required(
        "birthDate", Prim.long.composeIso(Iso(new Instant(_:Long))((_:Instant).getMillis)), 
        Person.birthDate.asGetter
      ),
      required("roles", Prim.arr(roleSchema), Person.roles.asGetter)
    )(Person.apply _)
  )
}
