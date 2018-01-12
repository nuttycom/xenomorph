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
package xenomorph.argonaut

import org.scalacheck._
import org.specs2._
import xenomorph.samples._

class ArgonautSpec extends Specification with org.specs2.ScalaCheck {
  def is = s2"""
  Serialization of values to JSON should
    serialize a value to JSON $toJson
    read a value from JSON $fromJson
    round-trip values produced by a generator $gen
  """

  import xenomorph.scalacheck.ToGen._
  import xenomorph.scalacheck.Implicits._
  import xenomorph.argonaut.Implicits._
  import xenomorph.argonaut.ToJson._
  import xenomorph.argonaut.FromJson._

  def toJson = {
    val result = Person.schema.toJson(person)
    result.toString must_== """{"roles":[{"administrator":{"subordinateCount":0,"department":"windmill-tilting"}}],"birthDate":20147028000,"name":"Kris Nuttycombe"}"""
  }

  def fromJson = {
    val result = Person.schema.toJson(person)
    Person.schema.fromJson(result).toOption must_== Some(person)
  }

  def gen = {
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.toGen)
    prop(
      (p: Person) =>
        Person.schema.fromJson(Person.schema.toJson(p)).toOption must_== Some(p)
    )
  }
}
