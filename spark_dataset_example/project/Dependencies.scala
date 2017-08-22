import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  lazy val mockito = "org.mockito" % "mockito-all" % "1.9.5" % "test"
  lazy val sparkCore = "org.apache.spark" %% "spark-core" % "2.2.0"
  lazy val sparkSql = "org.apache.spark" %% "spark-sql" % "2.2.0"
  lazy val sparkMlLib = "org.apache.spark" %% "spark-mllib" % "2.2.0"
}
