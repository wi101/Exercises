name := "Exercises"
version := "0.1"
scalaVersion := "2.13.0"

val ZIOVersion = "1.0.0-RC17"
libraryDependencies ++= Seq("dev.zio" %% "zio-test" % ZIOVersion % "test",
  "dev.zio" %% "zio-test-sbt" % ZIOVersion % "test"
)
testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
