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
//
//case class JByteT[A]()   extends JType[A, Byte]
//case class JShortT[A]()  extends JType[A, Short]
//case class JIntT[A]()    extends JType[A, Int]
//case class JLongT[A]()   extends JType[A, Long]
//
//case class JFloatT[A]()  extends JType[A, Float]
//case class JDoubleT[A]() extends JType[A, Double]
//
//case class JCharT[A]()   extends JType[A, Char]
//case class JStrT[A]()    extends JType[A, String]

case class JArrayT[F[_], I](elemSchema: F[I]) extends JType[F, Vector[I]]

object JType {
  type JSchema[A, I] = HMutu[JType, Schema[A, ?[_], ?], I]

  def jNull   = prim(HMutu[JType, Schema[Unit, ?[_], ?], Unit](JNullT()))
  def jBool   = prim(HMutu[JType, Schema[Unit, ?[_], ?], Boolean](JBoolT()))
//  def jShort  = prim[JType[Unit, ?], Short](JShortT())
//  def jInt    = prim[JType[Unit, ?], Int](JIntT())
//  def jLong   = prim[JType[Unit, ?], Long](JLongT())
//  def jFloat  = prim[JType[Unit, ?], Float](JFloatT())
//  def jDouble = prim[JType[Unit, ?], Double](JDoubleT())
//  def jChar   = prim[JType[Unit, ?], Char](JCharT())
//  def jStr    = prim[JType[Unit, ?], String](JStrT())
  def jArray[I](elem: Schema[Unit, JSchema[Unit, ?], I]) = 
    prim(HMutu[JType, Schema[Unit, ?[_], ?], Vector[I]](JArrayT(elem)))
}
