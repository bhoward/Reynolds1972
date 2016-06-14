val sharedSettings = Seq(
  version := "0.1",
  organization := "bhoward",
  scalaVersion := "2.11.8"
)

lazy val interp = (project in file("."))
  .settings(sharedSettings: _*)
  .settings(
    name := "Reynolds1972",
    unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil,
    unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil,
    unmanagedResourceDirectories in Compile := Nil,
    unmanagedResourceDirectories in Test := Nil,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "0.3.7"
    ),
	EclipseKeys.withBundledScalaContainers := false,
	mainClass in assembly := Some("bhoward.demo.Main"),
	assemblyJarName in assembly := "Reynolds.jar"
  )
