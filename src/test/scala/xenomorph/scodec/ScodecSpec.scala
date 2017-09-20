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

import org.specs2._
import org.scalacheck._

import _root_.scodec.Attempt
import _root_.scodec.bits.BitVector

import xenomorph.scodec.ToEncoder._
import xenomorph.scodec.ToDecoder._
import xenomorph.scalacheck.ToGen._

import xenomorph.samples._

class ScodecSpec extends Specification with org.specs2.ScalaCheck {
  def is = s2"""
  Serialization of values to binary should
    round-trip values produced by a generator $gen
  """

  def gen = {
    val schema = Person.schema
    implicit val arbPerson = Arbitrary(schema.toGen)
    prop(
      (p: Person) => {
        val res = for {
          enc <- schema.toEncoder.encode(p)
          dec <- schema.toDecoder.decode(enc)
        } yield dec

        (res.map(_.value) must_== Attempt.successful(p)) and
        (res.map(_.remainder) must_== Attempt.successful(BitVector.empty))
      }
    )
  }
}
