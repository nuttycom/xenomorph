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
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import xenomorph.samples._

class ArgonautSpec extends FunSuite with Checkers {

  import xenomorph.scalacheck.ToGen._
  import xenomorph.scalacheck.Implicits._
  import xenomorph.argonaut.Implicits._
  import xenomorph.argonaut.ToJson._
  import xenomorph.argonaut.FromJson._

  test("A value should serialise to JSON") {
    val result = Person.schema.toJson(person)
    assert(result.toString == """{"roles":[{"administrator":{"subordinateCount":0,"department":"windmill-tilting"}}],"birthDate":20147028000,"name":"Kris Nuttycombe"}""")
  }

  test("A value should be deserialised from JSON"){
    val result = Person.schema.toJson(person)
    assert(Person.schema.fromJson(result).toOption == Some(person))
  }


  test("Serialization should round-trip values produced by a generator"){
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(Person.schema.toGen)
    check{
      (p: Person) =>
        Person.schema.fromJson(Person.schema.toJson(p)).toOption == Some(p)
    }
  }

}
