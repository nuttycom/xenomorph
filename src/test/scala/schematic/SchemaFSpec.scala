package schematic

import scalaz.syntax.apply._
import scalaz.FreeAp

import org.specs2._
import org.scalacheck._
import org.scalacheck.Gen._

import schematic.Schema._

case class Person(
  name: String, 
  birthDate: Double, // seconds since the epoch
  roles: Vector[Role]
)

sealed trait Role

case object User extends Role
case class Administrator(department: String, subordinateCount: Int) extends Role

sealed trait Prim[A]
case object StrPrim extends Prim[String]
case object IntPrim extends Prim[Int]

object Prim {
  def str: Schema[Unit, Prim, String] = Schema.prim(StrPrim)
  def int: Schema[Unit, Prim, Int] = Schema.prim(IntPrim)
}


class SchemaFSpec extends Specification with org.specs2.ScalaCheck {
  def is = """
  """

  val roleSchema = oneOf(
    alt[Unit, Prim, Role, Unit](
      "user", 
      Schema.empty,
      (_: Unit) => User, 
      { 
        case User => Some(Unit)
        case _ => None
      }
    ) ::
    alt[Unit, Prim, Role, Administrator](
      "administrator", 
      rec[Prim, Administrator](
        ^[Schema.Prop[Unit, Prim, Administrator, ?], String, Int, Administrator](
          required("department", Prim.str, (_: Administrator).department),
          required("subordinateCount", Prim.int, (_: Administrator).subordinateCount)
        )(Administrator(_, _))
      ),
      identity,
      { 
        case a @ Administrator(_, _) => Some(a)
        case _ => None
      }
    ) :: Nil
  )
}
