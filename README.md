Xenomorph
=========

Xenomorph is a Scala library for building well-typed descriptions of other
Scala data structures, from which one can then automatically derive 
serialization, parsing, and generation functions. Implementations are
currently provided for producing [argonaut](http://argonaut.io) `DecodeJson`
and `EncodeJson` instances, as well as [scalacheck](http://scalacheck.org)
`Gen` values. Similar facilities for scodec, protobuf, and whatever else
might be useful will be coming as time permits and interest demands.

Getting Started
---------------

Xenomorph is still awaiting an initial release, so for now you'll have
to build it locally for yourself.

Creating a Schema
-----------------

Begin with the data type for which you wish to create a schema. Here's
an example:

~~~scala
import monocle.macros._

@Lenses case class Person(
  name: String, 
  birthDate: Instant,
  roles: Vector[Role]
)

sealed trait Role

case object User extends Role {
  val prism = GenPrism[Role, User.type]
}

@Lenses case class Administrator(department: String, subordinateCount: Int) extends Role
object Administrator {
  val prism = GenPrism[Role, Administrator]
}
~~~

In this example, you can see:
* A simple record type `Person`
* A sum type `Role` with two constructors, `User` and `Administrator`

To build a schema for the `Person` type, we will use the Scalaz applicative
constructor at arity 3 (`^^`) as shown below:

~~~scala
import scalaz.syntax.apply._
import xenomorph.Schema._
import xenomorph.json.JType._

val personSchema: Schema[JSchema, Person] = rec(
  ^^(
    required("name", jStr, Person.name.asGetter),
    required(
      "birthDate", jLong.composeIso(Iso(new Instant(_:Long))((_:Instant).getMillis)), 
      Person.birthDate.asGetter
    ),
    required("roles", jArray(roleSchema), Person.roles.asGetter)
  )(Person.apply _)
)
~~~

The schema for the `Role` sum type is created as a nonempty list of
alternatives, each of which provides a prism from the sum type to the
underlying data type associated with each constructor. In the case of the
`User` case object, the underlying schema is that of the empty object, which is
isomorphic to `Unit`.  `()` is the empty tuple, so we treat the empty record as
the `Unit` schema constructor.

~~~scala
val roleSchema: Schema[JSchema, Role] = Schema.oneOf(
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
      )(Administrator.apply _)
    ),
    Administrator.prism
  ) :: shapeless.HNil
)
~~~

This schema is constructed using the `JType` GADT to define the set of recognized
primitive types. However, the set of primitive types is a user-definable feature at
the time of schema construction.

Once you have a Schema value, you can use it to produce parsers, serializers, and 
generators.

~~~scala
import argonaut._
import xenomorph.json.ToJson._
import xenomorph.json.FromJson._
import xenomorph.scalacheck.ToGen._

val personJson: Json = personSchema.toJson(person) 

val parsedPerson: Option[Person] = personSchema.fromJson(personJson).toOption

val personGen: Gen[Person] = personSchema.toGen
~~~

Contributors
------------
Kris Nuttycombe (\@nuttycom)
Antonio Alonso Dominguez
Doug Clinton
