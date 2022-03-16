
set %JAVA_HOME%="C:\Program Files\Zulu\zulu-11"

javac -cp "../lib/commons-io-2.6.jar" -d ./build ./flixspt/sandbox/*.java

cd build 

jar cf flix-sandboxjava-1.1.jar ./flixspt/sandbox/*.class

