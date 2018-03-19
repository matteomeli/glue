import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.typegrade",
      organizationName := "Typegrade",
      scalaVersion := "2.12.4",
      version      := "0.1.1"
    )),
    name := "Glue",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += scalaTest % Test,
    licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
    bintrayPackageLabels := Seq("functional programming", "category theory", "scala")
  )
