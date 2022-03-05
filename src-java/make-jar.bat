
set %JAVA_HOME%="C:\Program Files\Zulu\zulu-11"

javac -cp "../lib/commons-io-2.6.jar" -d ./build ./flixspt/sandbox/*.java ./flixspt/sandbox/marshal/*.java

cd build 

jar cf flix-sandboxjava-1.0.jar ./flixspt/sandbox/*.class ./flixspt/sandbox/marshal/*.class

