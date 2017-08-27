name := "schematic"

version := "0.0.1"

scalaVersion := "2.12.2"

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

val monocleVersion = "1.4.0"     

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.14",
  "io.argonaut" %% "argonaut" % "6.2",
  "org.scalacheck" %% "scalacheck"  % "1.13.5",
  "com.github.julien-truffaut"  %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro"   % monocleVersion,        
  "com.github.julien-truffaut"  %%  "monocle-state"   % monocleVersion,     
  "com.github.julien-truffaut"  %%  "monocle-refined" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-law"     % monocleVersion % "test",
  "org.specs2"     %% "specs2-core" % "3.9.4"  % "test",
  "org.specs2"     %% "specs2-scalacheck" % "3.9.4"  % "test"
)

autoAPIMappings := true
