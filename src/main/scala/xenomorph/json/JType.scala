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
package xenomorph.json

import xenomorph.HMutu
import xenomorph.Schema._

sealed trait JType[F[_], I]

case class JNullT[F[_]]()   extends JType[F, Unit]
case class JBoolT[F[_]]()   extends JType[F, Boolean]

case class JByteT[F[_]]()   extends JType[F, Byte]
case class JShortT[F[_]]()  extends JType[F, Short]
case class JIntT[F[_]]()    extends JType[F, Int]
case class JLongT[F[_]]()   extends JType[F, Long]

case class JFloatT[F[_]]()  extends JType[F, Float]
case class JDoubleT[F[_]]() extends JType[F, Double]

case class JCharT[F[_]]()   extends JType[F, Char]
case class JStrT[F[_]]()    extends JType[F, String]

case class JArrayT[F[_], I](elemSchema: F[I]) extends JType[F, Vector[I]]

object JType {
  type JSchema[I] = HMutu[JType, Schema, I]

  def jNull   = prim(HMutu[JType, Schema, Unit](JNullT()))
  def jBool   = prim(HMutu[JType, Schema, Boolean](JBoolT()))
  def jShort  = prim(HMutu[JType, Schema, Short](JShortT()))
  def jInt    = prim(HMutu[JType, Schema, Int](JIntT()))
  def jLong   = prim(HMutu[JType, Schema, Long](JLongT()))
  def jFloat  = prim(HMutu[JType, Schema, Float](JFloatT()))
  def jDouble = prim(HMutu[JType, Schema, Double](JDoubleT()))
  def jChar   = prim(HMutu[JType, Schema, Char](JCharT()))
  def jStr    = prim(HMutu[JType, Schema, String](JStrT()))
  def jArray[I](elem: Schema[JSchema, I]) = prim(HMutu[JType, Schema, Vector[I]](JArrayT(elem)))
}
