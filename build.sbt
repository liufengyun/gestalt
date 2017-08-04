lazy val metaVersion = "1.6.0"
lazy val dottyOrg = "me.fengy"
lazy val dottyVersion = "0.4.0-bin-SNAPSHOT"

lazy val common = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  )
)

lazy val gestaltSetting = Seq(
  name := "gestalt",
  version := "0.1.3",
  organization := "me.fengy",

  scalaOrganization := dottyOrg,
  scalaVersion := dottyVersion,

  libraryDependencies ++= Seq(
    ("org.scalameta" %% "scalameta" % metaVersion).withDottyCompat(),
    dottyOrg %% "dotty" % dottyVersion % "provided"
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

  licenses += "BSD" -> url("https://github.com/liufengyun/gestalt/blob/master/LICENSE.md"),

  publishArtifact in Test := false,

  pomIncludeRepository := { _ => false },

  pomExtra := (
    <url>https://github.com/liufengyun/gestalt</url>
    <scm>
      <url>http://github.com/liufengyun/gestalt</url>
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
    Seq("-Xprint:frontend,parser,macrosTransform", "-Ycheck:all") // "-Yplain-printer", "-Xprint:frontend,parser", "-Ylog:frontend",
  },

  traceLevel := 0,

  // Dotty version
  scalaVersion := dottyVersion,
  scalaOrganization := dottyOrg

) ++ common

lazy val macros = (project in file("macros")).
  settings(macrosSetting: _*).
  dependsOn(gestalt)

