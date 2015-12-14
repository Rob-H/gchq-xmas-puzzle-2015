val scalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
val swing = "org.scala-lang" % "scala-swing" % "2.11+"

lazy val root = (project in file(".")).
    settings(
        name := "GCHQ puzzle solver",
        scalaVersion := "2.11.4",
        libraryDependencies ++= Seq(scalaTest, swing)
    )
