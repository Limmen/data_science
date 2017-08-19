import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "limmen.kth",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "spark_kmeans_example",
    libraryDependencies ++= Seq(
      scalaTest,
      mockito,
      sparkCore,
      sparkSql,
      sparkMlLib
    )
  )
