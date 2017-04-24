lazy val metaVersion = "1.6.0"
lazy val dottyVersion = "0.2.2-SNAPSHOT"

lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val gestaltSetting = Seq(
  name := "gestalt",
  version := "0.1.1",
  organization := "me.fengy",
  scalaVersion := "2.11.8",

  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % metaVersion,
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "me.fengy" % "dotty_2.11" % dottyVersion % "provided"
  ),

  credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),

  publishMavenStyle := true,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) {
      Some("snapshots" at nexus + "content/repositories/snapshots")
    } else {
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  },

  licenses += "BSD" -> url("https://github.com/liufengyun/eden/blob/master/LICENSE.md"),

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := (
    <url>https://github.com/liufengyun/eden</url>
    <scm>
      <url>http://github.com/liufengyun/eden</url>
      <connection>scm:git:git@github.com:liufengyun/eden.git</connection>
    </scm>
    <developers>
      <developer>
        <id>liufengyun</id>
        <name>Liu Fengyun</name>
        <url>http://fengy.me</url>
      </developer>
    </developers>
  )
) ++ common

lazy val gestalt = (project in file(".")).
  settings(gestaltSetting: _*)

lazy val macrosSetting = Seq(
  scalacOptions := {
    Seq("-Xprint:frontend,parser,macrosTransform") // "-Yplain-printer", "-Xprint:frontend,parser", "-Ylog:frontend",
  },

  traceLevel := 0,

  // Dotty version
  scalaVersion := dottyVersion,
  scalaOrganization := "me.fengy",

  // Dotty is compatible with Scala 2.11, as such you can use 2.11
  // binaries. However, when publishing - this version number should be set
  // to 0.1 (the dotty version number)
  scalaBinaryVersion := "2.11",

  // Don't import the stdlib for "scalaBinaryVersion"
  autoScalaLibrary := false,

  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.11" % "test",

    // Dotty depends on stdlib 2.11.5, best use that too (0.1-SNAPSHOT is
    // actually 2.11.5, published under ch.epfl.lamp)
    "ch.epfl.lamp" % "scala-library_2.11" % "0.1-SNAPSHOT",
    // Compiler on tool path
    "me.fengy" % "dotty_2.11" % dottyVersion % "scala-tool"
  ),

  // Bridge which allows REPL and compilation via dotty
  scalaCompilerBridgeSource := ("ch.epfl.lamp" % "dotty-sbt-bridge" % "0.1.1-20170203-da7d723-NIGHTLY" % "component").sources(),

  testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

) ++ common

lazy val macros = (project in file("macros")).
  settings(macrosSetting: _*).
  dependsOn(gestalt)

