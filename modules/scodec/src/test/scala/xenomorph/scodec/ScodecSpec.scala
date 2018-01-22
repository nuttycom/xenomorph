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
package xenomorph.scodec

import _root_.scodec.Attempt
import _root_.scodec.bits.BitVector
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import xenomorph.Schema.Schema
import xenomorph.json.JType.JSchema
import xenomorph.samples.Person

class ScodecSpec extends FunSuite with Checkers {

  import xenomorph.scalacheck.ToGen._
  import xenomorph.scalacheck.Implicits._
  import xenomorph.scodec.ToEncoder._
  import xenomorph.scodec.ToDecoder._
  import xenomorph.scodec.Implicits._

  test("Serialization of values to binary should round-trip values produced by a generator"){

    val schema: Schema[JSchema, Person] = Person.schema
    implicit val arbPerson : Arbitrary[Person] = Arbitrary(schema.toGen)

    check(
      (p: Person) => {
        val res = for {
          enc <- schema.toEncoder.encode(p)
          dec <- schema.toDecoder.decode(enc)
        } yield dec

        (res.map(_.value) == Attempt.successful(p)) &&
          (res.map(_.remainder) == Attempt.successful(BitVector.empty))
      }
    )
  }

}
