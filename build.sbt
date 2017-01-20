lazy val metaVersion = "1.5.0.585"
lazy val dottyVersion = "0.1.2-SNAPSHOT"

lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.bintrayIvyRepo("scalameta", "maven"),
    Resolver.sonatypeRepo("snapshots")
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
    "me.fengy" % "dotty_2.11" % dottyVersion
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

