ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Scalix"
  )
libraryDependencies += "org.json4s" %% "json4s-ast" % "4.0.6"
libraryDependencies += "org.json4s" %% "json4s-native" % "4.0.6"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0"

