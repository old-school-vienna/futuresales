name := "futuresales"

version := "0.1"

scalaVersion := "2.13.3"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "net.entelijan" %% "viz" % "0.2-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"


