val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

ThisBuild / organization := "io.github.matteomeli"
ThisBuild / organizationName := "matteomeli"
ThisBuild / version := "0.3.0"
ThisBuild / scalaVersion := "2.12.7"

lazy val root = (project in file(".")).
  settings(
    name := "glue",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      scalaCheck % Test
    ),
    scalacOptions ++= Seq(
      "-deprecation",                       // Emit warning and location for usages of deprecated APIs.
      "-encoding", "UTF-8",                 // Specify character encoding used by source files to UTF8.
      "-explaintypes",                      // Explain type errors in more detail.
      "-feature",                           // Emit warning and location for usages of features that should be imported explicitly.
      "-Xfuture",                           // Turn on future language features.
      "-Ypartial-unification",              // Enable partial unification in type constructor inference.
      "-language:implicitConversions",      // Allow definition of implicit functions called views.
      "-language:higherKinds",              // Allow higher-kinded types.
      "-language:existentials",             // Existential types (besides wildcard types) can be written and inferred.
      "-language:postfixOps",               // Allow postfix operator notation, such as 1 to 10 toList.
      "-unchecked",                         // Enable additional warnings where generated code depends on assumptions.
      "-Xexperimental",                     // Enable experimental extensions (not necessary for Scala >= 2.13).
      "-Yno-adapted-args",                  // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Xlint:_,-type-parameter-shadow",    // A local type parameter shadows a type already in scope.
      "-Ywarn-dead-code",                   // Warn when dead code is identified.
      "-Ywarn-unused-import",               // Warn if an import selector is not referenced.
      "-Ywarn-numeric-widen",               // Warn when numerics are widened.
      "-Ywarn-value-discard",               // Warn when non-Unit expression results are unused.
      "-Xfatal-warnings"                    // Fail the compilation if there are any warnings.
    ),
    parallelExecution in Test := false,
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayPackageLabels := Seq("functional programming", "category theory", "scala"),
    bintrayVcsUrl := Some("https://github.com/matteomeli/glue.git")
  )
