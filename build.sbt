name := "sjsch"

version := "0.0.1"

scalaVersion := "2.12.0"

scalacOptions := Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked"
)

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "io.argonaut" %% "argonaut" % "6.2-SNAPSHOT" 
)
