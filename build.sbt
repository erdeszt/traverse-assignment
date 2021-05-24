name := "samba-ai-assignment"

version := "0.1"

scalaVersion := "3.0.0"

scalacOptions ++= Seq(
  "-Ykind-projector"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.6.1",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
)
