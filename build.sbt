
val commonSettings = Seq(
  version            := "0.6.1",
  scalaVersion       := "3.7.2",
  crossScalaVersions := Seq("2.12.13", "2.13.4", "3.0.1", "3.2.0", "3.5.2"),
  organization       := "ch.epfl.lara",
)

lazy val silex = project
  .in(file("."))
  .settings(
    commonSettings,
    name               := "silex",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    ),

    Compile / doc / scalacOptions ++= Seq(
      "-groups",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-root-content", baseDirectory.value + "/project/root-doc.txt"
    ),

    Compile / doc / target := baseDirectory.value / "docs",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    ),

    bintrayOrganization := Some("epfl-lara"),
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    bintrayPackageLabels := Seq(
      "scala", "lexer", "lexing"
    ),
  )
