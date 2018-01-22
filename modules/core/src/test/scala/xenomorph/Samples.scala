package xenomorph

import monocle.macros._
import shapeless.HNil
import xenomorph.Schema._
import xenomorph.json.JType._

import scalaz.syntax.apply._

package samples {


  @Lenses case class Person(
    name: String,
    birthDate: Long,
    roles: Vector[Role]
  )

  object Person {

    val schema: Schema[JSchema, Person] = rec(
      ^^(
        required("name", jStr, Person.name.asGetter),
        required(
          "birthDate", jLong, Person.birthDate.asGetter
        ),
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
    "Kris Nuttycombe",
    20147028000l,
    Vector(Administrator("windmill-tilting", 0))
  )
}
