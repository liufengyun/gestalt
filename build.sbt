lazy val dottyOrg = "me.fengy"
lazy val dottyVersion = "0.6.0-bin-SNAPSHOT"


lazy val commonSetting = Seq(
  name := "gestalt",
  version := "0.4.2",
  organization := "me.fengy",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  ),

  scalaOrganization := dottyOrg,
  scalaVersion := dottyVersion,

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
      <connection>scm:git:git@github.com:liufengyun/gestalt.git</connection>
    </scm>
    <developers>
      <developer>
        <id>liufengyun</id>
        <name>Liu Fengyun</name>
        <url>http://fengy.me</url>
      </developer>
    </developers>
  )
)

lazy val gestalt = (project in file("."))
  .settings(commonSetting: _*)

lazy val quasiquotes = (project in file("quasiquotes"))
  .dependsOn(gestalt)
  .settings(commonSetting: _*)
  .settings(
    name := "quasiquotes",
    libraryDependencies += ("org.scalameta" %% "scalameta" % "1.6.0").withDottyCompat()
  )

lazy val `dotty-backend` = (project in file("dotty"))
  .dependsOn(gestalt)
  .settings(commonSetting: _*)
  .settings(
    name := "dotty-backend",
    libraryDependencies += dottyOrg %% "dotty" % dottyVersion % "provided"
  )

lazy val macrosSetting = Seq(
  // scalacOptions := Seq("-Xprint:frontend,parser", "-Ycheck:all"), // "-Yplain-printer", "-Xprint:frontend,parser", "-Ylog:frontend",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeIvyRepo("releases")
  ),

  traceLevel := 0,

  // Dotty version
  scalaVersion := dottyVersion,
  scalaOrganization := dottyOrg
)

lazy val macros = (project in file("macros"))
  .dependsOn(gestalt, quasiquotes, `dotty-backend`)
  .settings(macrosSetting: _*)


/*-------------------- publish artefacts -----------------------*/

commands += Command.command("publishAll") { state =>
  "gestalt/publishSigned" ::
    "quasiquotes/publishSigned" ::
    "dotty-backend/publishSigned" ::
    "sonatypeReleaseAll" ::
    state
}

commands += Command.command("publishAllLocal") { state =>
  "gestalt/publishLocal" ::
    "quasiquotes/publishLocal" ::
    "dotty-backend/publishLocal" ::
    state
}
