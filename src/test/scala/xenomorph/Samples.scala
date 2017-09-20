package xenomorph

import scalaz.NonEmptyList
import scalaz.syntax.apply._

import monocle.Iso
import monocle.macros._

import org.joda.time.Instant

import xenomorph.Schema._
import xenomorph.json.JType._

package samples {
  @Lenses case class Person(
    name: String, 
    birthDate: Instant,
    roles: Vector[Role]
  )

  object Person {
    val schema: Schema[JSchema, Person] = rec(
      ^^(
        required("name", jStr, Person.name.asGetter),
        required(
          "birthDate", jLong.composeIso(Iso(new Instant(_:Long))((_:Instant).getMillis)), 
          Person.birthDate.asGetter
        ),
        required("roles", jArray(Role.schema), Person.roles.asGetter)
      )(Person.apply _)
    )
  }

  sealed trait Role

  object Role {
    val schema: Schema[JSchema, Role] = Schema.oneOf(
      NonEmptyList(
        alt[JSchema, Role, User.type](
          "user", 
          Schema.const(User),
          User.prism
        ),
        alt[JSchema, Role, Administrator](
          "administrator", 
          rec(
            ^(
              required("department", jStr, Administrator.department.asGetter),
              required("subordinateCount", jInt, Administrator.subordinateCount.asGetter)
            )(Administrator.apply _)
          ),
          Administrator.prism
        )
      )
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
    "Kris Nuttycombe", 
    new Instant(20147028000l), 
    Vector(Administrator("windmill-tilting", 0))
  )
}
