package xenomorph.tutorial

import scalaz.FreeAp

import org.specs2._
import monocle.macros._

import xenomorph.tutorial.schema5._

case class Person(
  name: String, 
  birthDate: Long,
  roles: Vector[Role]
)

sealed trait Role

case object User extends Role {
  val prism = monocle.macros.GenPrism[Role, User.type]
}

case class Administrator(department: String) extends Role
object Administrator {
  val prism = monocle.macros.GenPrism[Role, Administrator]
}

class JSchemaSpec extends Specification with org.specs2.ScalaCheck {
  def is = s2"""
  Serialization of primitive values to JSON should
    serialize boolean values $serBool
    serialize double values $serLong
    serialize string values $serString
  """

  def serBool = {
    ok
  }

  def serLong = {
    ok
  }

  def serString = {
    ok
  }

  val roleSchema = JSumT(
    Alt[Role, Unit](
      "user", 
      JObjT(FreeAp.pure(())), 
      User.prism composeIso GenIso.unit[User.type]
    ) ::
    Alt[Role, String](
      "admin", 
      JObjT(FreeAp.lift(PropSchema("department", JStrT, identity))), 
      Administrator.prism composeIso GenIso[Administrator, String]
    ) :: Nil
  )
}
