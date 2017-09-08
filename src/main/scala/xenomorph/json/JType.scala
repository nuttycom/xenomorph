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
package xenomorph.json

import xenomorph.Schema._

sealed trait JType[A, I]

case class JNullT[A]()   extends JType[A, Unit]
case class JBoolT[A]()   extends JType[A, Boolean]

case class JByteT[A]()   extends JType[A, Byte]
case class JShortT[A]()  extends JType[A, Short]
case class JIntT[A]()    extends JType[A, Int]
case class JLongT[A]()   extends JType[A, Long]

case class JFloatT[A]()  extends JType[A, Float]
case class JDoubleT[A]() extends JType[A, Double]

case class JCharT[A]()   extends JType[A, Char]
case class JStrT[A]()    extends JType[A, String]

case class JArrayT[A, I](elemSchema: Schema[A, JType[A, ?], I]) extends JType[A, Vector[I]]

object JType {
  def jNull   = prim[JType[Unit, ?], Unit](JNullT())
  def jBool   = prim[JType[Unit, ?], Boolean](JBoolT())
  def jShort  = prim[JType[Unit, ?], Short](JShortT())
  def jInt    = prim[JType[Unit, ?], Int](JIntT())
  def jLong   = prim[JType[Unit, ?], Long](JLongT())
  def jFloat  = prim[JType[Unit, ?], Float](JFloatT())
  def jDouble = prim[JType[Unit, ?], Double](JDoubleT())
  def jChar   = prim[JType[Unit, ?], Char](JCharT())
  def jStr    = prim[JType[Unit, ?], String](JStrT())
  def jArray[A, I](elem: Schema[A, JType[A, ?], I]) = prim[JType[A, ?], Vector[I]](JArrayT(elem))
}
