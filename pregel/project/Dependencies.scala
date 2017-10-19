import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  lazy val mockito = "org.mockito" % "mockito-all" % "1.9.5" % "test"
  lazy val akka =  "com.typesafe.akka" %% "akka-actor" % "2.5.4"
  lazy val akkaTest = "com.typesafe.akka" %% "akka-testkit" % "2.5.4"
  lazy val akkaRemote = "com.typesafe.akka" % "akka-remote_2.11" % "2.5.4"
  lazy val scalaCsv = "com.github.tototoshi" %% "scala-csv" % "1.3.5"
  lazy val scallop = "org.rogach" %% "scallop" % "3.1.0"
}
