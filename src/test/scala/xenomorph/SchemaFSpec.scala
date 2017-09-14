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

import scalaz.NonEmptyList
import scalaz.syntax.apply._

import org.specs2._
//import org.scalacheck._
//import org.scalacheck.Gen._

import monocle.Iso
import monocle.macros._

import org.joda.time.Instant

import xenomorph.Schema._
import xenomorph.json.JType._
import xenomorph.json.ToJson._
import xenomorph.json.FromJson._

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

class SchemaFSpec extends Specification with org.specs2.ScalaCheck {
  def is = s2"""
  Serialization of values to JSON should
    serialize a value to JSON $toJson
    read a value from JSON $fromJson
  """
//    round-trip values produced by a generator $gen

  type TProp[O, A] = Schema.Prop[JSchema, O, A]

  val roleSchema: Schema[JSchema, Role] = Schema.oneOf(
    NonEmptyList(
      alt[JSchema, Role, User.type](
        "user", 
        Schema.const(User),
        User.prism
      ),
      alt[JSchema, Role, Administrator](
        "administrator", 
        rec(
          ^(
            required("department", jStr, Administrator.department.asGetter),
            required("subordinateCount", jInt, Administrator.subordinateCount.asGetter)
          )(Administrator.apply _)
        ),
        Administrator.prism
      )
    )
  )

  val personSchema: Schema[JSchema, Person] = rec(
    ^^(
      required("name", jStr, Person.name.asGetter),
      required(
        "birthDate", jLong.composeIso(Iso(new Instant(_:Long))((_:Instant).getMillis)), 
        Person.birthDate.asGetter
      ),
      required("roles", jArray(roleSchema), Person.roles.asGetter)
    )(Person.apply _)
  )

  def toJson = {
    val p = Person(
      "Kris Nuttycombe", 
      new Instant(20147028000l), 
      Vector(Administrator("windmill-tilting", 0))
    )

    val result = personSchema.toJson(p) 
    result.toString must_== """{"roles":[{"administrator":{"subordinateCount":0,"department":"windmill-tilting"}}],"birthDate":20147028000,"name":"Kris Nuttycombe"}"""
  }

  def fromJson = {
    val p = Person(
      "Kris Nuttycombe", 
      new Instant(20147028000l), 
      Vector(Administrator("windmill-tilting", 0))
    )

    val result = personSchema.toJson(p) 
    personSchema.fromJson(result).toOption must_== Some(p)
  }
}
