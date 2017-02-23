name := "parsing"

version := "1.0"

lazy val `parsing` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq( jdbc , cache , ws   , specs2 % Test, "com.typesafe.akka" %% "akka-http" % "10.0.3", ws )

libraryDependencies += "org.jsoup" % "jsoup" % "1.7.2"

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.12.1",
  "com.typesafe.akka" %% "akka-http" % "10.0.3"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"