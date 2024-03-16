ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "scala-cats"
  )

val catsVersion = "2.7.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)