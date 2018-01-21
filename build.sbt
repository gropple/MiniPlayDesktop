libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.144-R12"
libraryDependencies += "org.springframework.boot" % "spring-boot-starter-websocket" % "1.5.9.RELEASE"
libraryDependencies += "javax.websocket" % "javax.websocket-api" % "1.1" % "provided"
libraryDependencies += "org.apache.xmlgraphics" % "batik-parser" % "1.9.1"
libraryDependencies += "org.apache.xmlgraphics" % "batik-anim" % "1.9.1"
libraryDependencies += "org.apache.xmlgraphics" % "batik-bridge" % "1.9.1"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.10.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.10.0"
libraryDependencies += "com.jfoenix" % "jfoenix" % "9.0.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalafx" %% "scalafxml-core-sfx8" % "0.4"

lazy val miniplay = (project in file("."))
  .settings(
    name := "MiniPlay",
    scalaVersion := "2.12.3"
  )

