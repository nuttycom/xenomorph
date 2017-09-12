package xenomorph.tutorial

import scalaz.FreeAp
import scalaz.syntax.apply._

import org.specs2._

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

final case class Administrator(department: String, subordinateCount: Long) extends Role
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
    Alt[Role, User.type](
      "user", 
      JObjT[User.type](FreeAp.pure(User)), 
      User.prism 
    ) ::
    Alt[Role, Administrator](
      "admin", 
      JObjT(
        ^(
          FreeAp.lift(PropSchema("department", JStrT, (_:Administrator).department)),
          FreeAp.lift(PropSchema("subordinateCount", JNumT, (_:Administrator).subordinateCount)),
        )(Administrator.apply _)
      ),
      Administrator.prism 
    ) :: Nil
  )
}
