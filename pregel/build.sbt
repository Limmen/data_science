import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "limmen.kth",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "pregel",
    libraryDependencies ++= Seq(
      scalaTest,
      mockito,
      akka,
      akkaTest,
      akkaRemote,
      scalaCsv,
      scallop
    )
  )
