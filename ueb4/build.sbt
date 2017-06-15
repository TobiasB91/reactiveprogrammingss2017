import sbt.Keys.scalaVersion

lazy val commonSettings = Seq(
  scalaVersion := "2.12.2"
)

lazy val shared = crossProject.in(file("scala-shared")).settings(
  commonSettings,
  name := "cis-shared",
  libraryDependencies ++= Seq(
    "io.circe" %%% "circe-core" % "0.7.0",
    "io.circe" %%% "circe-generic" % "0.7.0",
    "io.circe" %%% "circe-parser" % "0.7.0"
  )
).jsConfigure(_ enablePlugins(ScalaJSWeb))

lazy val sharedJS = shared.js
lazy val sharedJVM = shared.jvm

lazy val server = project.in(file("scala-server")).settings(
  commonSettings,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  name := "curried-in-space",
  WebKeys.packagePrefix in Assets := "assets/",
  managedClasspath in Runtime += (packageBin in Assets).value,
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-http" % "10.0.6",
    "com.vmunier" %% "scalajs-scripts" % "1.1.0"
  ),
  resourceDirectories in Runtime += (baseDirectory in client).value / "assets"
).enablePlugins(SbtWeb)
 .dependsOn(sharedJVM)


lazy val client = project.in(file("scala-client")).settings(
  commonSettings,
  name := "cis-client",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.2"
  ),
  /*jsDependencies ++= Seq(
    "org.webjars.npm" % "pixi.js" % "4.5.2" / "pixi.min.js",
    "org.webjars.npm" % "pixi-filters" % "1.0.6" / "filters.min.js" dependsOn "pixi.min.js",
    "org.webjars.npm" % "pixi.js" % "4.5.2" / "pixi-particles.min.js" dependsOn "pixi.min.js"
  ),
  skip in packageJSDependencies := false,*/
  artifactPath in (Compile, fastOptJS) := baseDirectory.value / "assets" / "js" / "client.js",
  artifactPath in (Compile, fullOptJS) := baseDirectory.value / "assets" / "js" / "client.js",
  scalaJSUseMainModuleInitializer := true
).enablePlugins(ScalaJSPlugin,ScalaJSWeb)
 .dependsOn(sharedJS)

onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value