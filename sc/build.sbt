
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

javaOptions in run +=  "-verbosegc"

javaOptions in run +=  "-XX:+PrintGCDetails"

javaOptions in run += "-verbosegc"

javaOptions in run += "-Xloggc:gc.log"
