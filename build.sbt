name := "sjsch"

version := "0.0.1"

scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.2")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.3"
