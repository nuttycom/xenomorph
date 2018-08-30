package xenomorph

import java.time.Instant

import monocle.macros._
import shapeless.HNil
import xenomorph.Schema._
import xenomorph.json.JType._

import scalaz.syntax.apply._

package samples {

  import monocle.{Iso, Prism}

  import scala.util.Try

  case class PersonId(id: Long) extends AnyVal

  @Lenses case class Person(
    id: PersonId,
    name: String,
    birthDate: Instant,
    roles: Vector[Role]
  )

  object Person {

    def instantPrism: Prism[Long, Instant] =
      Prism[Long, Instant](x => Try(Instant.ofEpochMilli(x)).toOption)(_.toEpochMilli)

    def personIdIso: Iso[Long, PersonId] =
      Iso[Long, PersonId](PersonId.apply)(_.id)

    val jInstant = jLong.composePrism(instantPrism)

    val schema: Schema[JSchema, Person] = rec(
      (
        required("personId", jLong.composeIso(personIdIso), Person.id.asGetter) |@|
        required("name", jStr, Person.name.asGetter) |@|
        required("birthDate", jInstant, Person.birthDate.asGetter) |@|
        required("roles", jArray(Role.schema), Person.roles.asGetter)
      )(Person.apply)
    )
  }

  sealed trait Role

  object Role {
    val schema: Schema[JSchema, Role] = Schema.oneOf(
      alt[JSchema, Role, User.type](
        "user",
        Schema.const(User),
        User.prism
      ) ::
      alt[JSchema, Role, Administrator](
        "administrator",
        rec(
          ^(
            required("department", jStr, Administrator.department.asGetter),
            required("subordinateCount", jInt, Administrator.subordinateCount.asGetter)
          )(Administrator.apply)
        ),
        Administrator.prism
      ) :: HNil
    )
  }

  case object User extends Role {
    val prism = GenPrism[Role, User.type]
  }

  @Lenses case class Administrator(department: String, subordinateCount: Int) extends Role
  object Administrator {
    val prism = GenPrism[Role, Administrator]
  }
}

package object samples {
  val person = Person(
    PersonId(1),
    "Kris Nuttycombe",
    Instant.ofEpochMilli(20147028000l),
    Vector(Administrator("windmill-tilting", 0))
  )
}
