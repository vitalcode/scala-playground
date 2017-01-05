name := "Scala Playground"

version := "1.0"

scalaVersion := "2.11.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

val circeVersion = "0.6.0"
val sprayJsonVersion = "1.3.2"
val catsVersion = "0.8.1"
val playVersion = "2.5.9"
val scalazVersion = "7.2.7"
val shapelessVersion = "2.3.2"
val sprayJsonShapelessVersion = "1.3.0"
val simulacrumVersion = "0.10.0"
val scalaTestVersion = "2.2.5"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % sprayJsonVersion,
  "org.typelevel" %% "cats" % catsVersion,
  "com.typesafe.play" %% "play-json" % playVersion,
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "com.chuusai" %% "shapeless" % shapelessVersion,
  "com.github.fommil" %% "spray-json-shapeless" % sprayJsonShapelessVersion,
  "com.github.mpilquist" %% "simulacrum" % simulacrumVersion
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-optics"
).map(_ % circeVersion)

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion

