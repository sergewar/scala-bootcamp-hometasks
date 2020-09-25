name := "Scala bootcamp home tasks"
version := "0.0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.0",
  "org.scalatest" %% "scalatest" % "3.2.0" % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,

)
