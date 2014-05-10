import AssemblyKeys._

name := "ggdrive"

version := "0.1"

scalaVersion := "2.11.0"

fork in run := true

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

libraryDependencies ++= {
  val akkaVersion = "2.3.2"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion
  )
}

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2"

libraryDependencies +=  "ch.qos.logback" % "logback-classic" % "0.9.24" % "compile"

libraryDependencies +=  "com.typesafe.slick" % "slick_2.11.0-RC4" % "2.1.0-M1"

libraryDependencies +=  "com.h2database" % "h2" % "1.4.178"


libraryDependencies += "com.google.api-client" % "google-api-client" % "1.18.0-rc"

libraryDependencies += "com.google.apis" % "google-api-services-drive" % "v2-rev121-1.18.0-rc"

libraryDependencies += "com.google.http-client" % "google-http-client-jackson2" % "1.18.0-rc"

libraryDependencies += "com.google.oauth-client" % "google-oauth-client" % "1.18.0-rc"

libraryDependencies += "com.google.apis" % "google-api-services-oauth2" % "v2-rev68-1.18.0-rc"

libraryDependencies += "net.java.dev.jna" % "jna" % "4.1.0"

scalacOptions in Compile += "-feature"

javaOptions in run += "-Djna.nosys=true"

assemblySettings
