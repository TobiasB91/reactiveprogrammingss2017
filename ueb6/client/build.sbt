enablePlugins(ScalaJSPlugin)

name := "ot-client"
scalaVersion := "2.12.2"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.2"
libraryDependencies += "org.akka-js" %%% "akkajsactor" % "1.2.5.2"
libraryDependencies += "org.akka-js" %%% "akkajsactorstream" % "1.2.5.2"
libraryDependencies += "io.circe" %%% "circe-core" % "0.7.0"
libraryDependencies += "io.circe" %%% "circe-parser" % "0.7.0"

artifactPath in (Compile, fastOptJS) := file("assets/client.js")

scalaJSUseMainModuleInitializer := true