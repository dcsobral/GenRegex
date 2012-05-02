name := "GenRegex"

version := "1.0"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "6.0.3",
  "org.scala-tools.testing" %% "scalacheck" % "1.9",
  "org.specs2" %% "specs2" % "1.9" % "test"
)

cancelable := true
