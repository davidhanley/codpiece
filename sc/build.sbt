
libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

javaOptions in run +=  "-verbosegc"

javaOptions in run +=  "-XX:+PrintGCDetails"

javaOptions in run += "-verbosegc"

javaOptions in run += "-Xloggc:gc.log"


javaOptions in console +=  "-verbosegc"

javaOptions in console +=  "-XX:+PrintGCDetails"

javaOptions in console += "-verbosegc"

javaOptions in console += "-Xloggc:gc.log"

javaOptions in console += "-Xmx2G"

javaOptions in console += "-Xmn1G"

javaOptions in console += "-XX:MaxPermSize=256M"
