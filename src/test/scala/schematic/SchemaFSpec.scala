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

import scalaz.syntax.apply._
import scalaz.FreeAp

import org.specs2._
import org.scalacheck._
import org.scalacheck.Gen._

import monocle.macros._

import schematic.Schema._

@Lenses case class Person(
  name: String, 
  birthDate: Double, // seconds since the epoch
  roles: Vector[Role]
)

sealed trait Role

case object Role {
  val user = GenPrism[Role, User.type]
  val admin = GenPrism[Role, Administrator]
}

case object User extends Role
@Lenses case class Administrator(department: String, subordinateCount: Int) extends Role

sealed trait Prim[A]
case object StrPrim extends Prim[String]
case object IntPrim extends Prim[Int]
case object DoublePrim extends Prim[Double]
case class ArrPrim[I](elemSchema: Schema[Unit, Prim, I]) extends Prim[Vector[I]]

object Prim {
  type PSchema[A] = Schema[Unit, Prim, A]
  def str: PSchema[String] = Schema.prim(StrPrim)
  def int: PSchema[Int] = Schema.prim(IntPrim)
  def double: PSchema[Double] = Schema.prim(DoublePrim)
  def arr[A](elem: PSchema[A]): PSchema[Vector[A]] = 
    Schema.prim[Prim, Vector[A]](ArrPrim(elem))
}


class SchemaFSpec extends Specification with org.specs2.ScalaCheck {
  def is = """
  """

  type TProp[O, A] = Schema.Prop[Unit, Prim, O, A]

  val roleSchema: Schema[Unit, Prim, Role] = Schema.oneOf(
    alt[Unit, Prim, Role, Unit](
      "user", 
      Schema.empty,
      Role.user composeIso GenIso.unit[User.type]
    ) ::
    alt[Unit, Prim, Role, Administrator](
      "administrator", 
      rec[Prim, Administrator](
        ^[TProp[Administrator, ?], String, Int, Administrator](
          required("department", Prim.str, Administrator.department.asGetter),
          required("subordinateCount", Prim.int, Administrator.subordinateCount.asGetter)
        )(Administrator.apply _)
      ),
      Role.admin
    ) :: Nil
  )

  val personSchema = rec[Prim, Person](
    ^^[TProp[Person, ?], String, Double, Vector[Role], Person](
      required("name", Prim.str, Person.name.asGetter),
      required("birthDate", Prim.double, Person.birthDate.asGetter),
      required("roles", Prim.arr(roleSchema), Person.roles.asGetter)
    )(Person.apply _)
  )
}
