name := "FunProcessing"

version := "1.0"

scalaVersion := "2.12.2"


libraryDependencies += "org.processing" % "core" % "3.3.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

val processingVersion = "3.3.4"
val joglVersion = "2.3.2"
val processingVideoVersion = "3.0.2"

libraryDependencies ++= Seq(
  //Failure to exclude brings in additional libraries which "confuse" search for default GL device, causes openGL exception "no default device"
  "org.processing" % "core" % processingVersion excludeAll(
    ExclusionRule(organization = "org.jogamp.jogl"),
    ExclusionRule(organization = "org.jogamp.gluegen")),

  "org.processing" % "net" % processingVersion,
  "org.processing" % "serial" % processingVersion,
  "org.processing" % "pdf" % processingVersion,
  "org.processing" % "video" % processingVideoVersion,
  "org.jogamp.gluegen" % "gluegen-rt-main" % joglVersion,
  "org.jogamp.jogl" % "jogl-all" % joglVersion,
  "org.jogamp.jogl" % "jogl-all-main" % joglVersion
)