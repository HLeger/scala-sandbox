name := """future-test"""

version := "0.1"

scalaVersion := "2.12.4"


fork in run := true

javaOptions += "-Dscala.concurrent.context.maxThreads=8"
