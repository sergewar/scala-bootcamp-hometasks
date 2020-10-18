name := "Scala bootcamp home tasks"
version := "0.0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.2.0"
val catsScalacheckVersion = "0.2.0"
val log4CatsVersion = "1.1.1"
val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-generic-extras" % circeVersion,
  "io.circe" %% "circe-optics" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion,
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0",
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2",
  "org.scalaj" %% "scalaj-http" % "2.4.2",

)
