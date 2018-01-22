import sbt.Keys.scalaVersion
import sbt._
import sbtcrossproject.{CrossType, crossProject}

val monocleVersion = "1.4.0"

lazy val xenomorph = project
  .in(file("."))
  .aggregate(
    coreJVM, coreJS,
    scalacheckJVM, scalacheckJS,
    argonautJVM, argonautJS,
    scodecJVM
  )
  .settings(commonSettings)
  .settings(
    name := "xenomorph",
    publish := {},
    publishLocal := {}
  )

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(
    name := "xenomorph-core",
    libraryDependencies ++= Seq(
      "org.scalaz"                    %%% "scalaz-core"     % "7.2.18",
      "com.chuusai"                   %%% "shapeless"       % "2.3.2",
      "com.github.julien-truffaut"    %%% "monocle-core"    % monocleVersion,
      // test dependencies
      "com.github.julien-truffaut"  %%%  "monocle-generic"  % monocleVersion % "test",
      "com.github.julien-truffaut"  %%%  "monocle-macro"    % monocleVersion % "test",
      "com.github.julien-truffaut"  %%%  "monocle-state"    % monocleVersion % "test",
      "com.github.julien-truffaut"  %%%  "monocle-refined"  % monocleVersion % "test",
      "com.github.julien-truffaut"  %%%  "monocle-law"      % monocleVersion % "test",
      "org.scalatest"               %%% "scalatest"         % "3.0.4"        % "test",
      "org.scalacheck"              %%% "scalacheck"        % "1.13.4"       % "test"
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val argonaut = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/argonaut"))
  .dependsOn(core % "compile->compile;test->test", scalacheck % "test->test")
  .settings(commonSettings)
  .settings(
    name := "xenomorph-argonaut",
    libraryDependencies ++= Seq(
      "io.argonaut" %%% "argonaut" % "6.2.1"
    )
  )

lazy val argonautJVM = argonaut.jvm
lazy val argonautJS = argonaut.js

lazy val scodec = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/scodec"))
  .dependsOn(core % "compile->compile;test->test", scalacheck % "test->test")
  .settings(commonSettings)
  .settings(
    name := "xenomorph-scodec",
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-core" % "1.10.3",
      "org.scodec" %%% "scodec-scalaz" % "1.4.1a"
    )
  )

lazy val scodecJVM = scodec.jvm

lazy val scalacheck = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/scalacheck"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := "xenomorph-scalacheck",
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck"  % "1.13.5"
    )
  )

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS = scalacheck.js


lazy val commonSettings = Seq(
  version := "0.0.1" ,
  scalaVersion := "2.12.3",
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ),
  scalacOptions in (Compile, console) ~= (_.filterNot(Set(
    "-Ywarn-unused:imports",
    "-Xfatal-warnings"
  ))),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    // for @Lenses macro support
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  autoAPIMappings := true
)


