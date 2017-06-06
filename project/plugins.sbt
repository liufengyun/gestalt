addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.1")

resolvers += Resolver.sonatypeRepo("snapshots")
