/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace System {
    pub opaque type ProcessBuilder = ##java.lang.ProcessBuilder

    pub opaque type Process = ##java.lang.Process
}


namespace System/Process {

    use Text.Charset.{Charset};

    use System.Process;
    use System.Process.{Process};
    use System.FilePath.{FilePath};
    use System.ProcessBuilder;
    use System.ProcessBuilder.{ProcessBuilder};

    type alias MarshalStringArray = ##flix.runtime.spt.sandbox.system.marshal.MarshalStringArray

    pub def newProcessBuilder (cmd: String, args: Array[String]): ProcessBuilder & Impure = 
        import new flix.runtime.spt.sandbox.system.marshal.MarshalStringArray() as newStringArray;
        import flix.runtime.spt.sandbox.system.marshal.MarshalStringArray.snoc(String);
        import flix.runtime.spt.sandbox.system.ProcessWrapper:create(String, MarshalStringArray) as create;
        let argArray = newStringArray();
        Array.foreach(x -> argArray.snoc(x), args);
        ProcessBuilder(create(cmd, argArray))

    pub def directory!(x: ProcessBuilder, path: System.FilePath): Unit & Impure =
        import java.lang.ProcessBuilder.directory(##java.io.File);
        import new java.io.File(String) as newFile;
        let ProcessBuilder(x1) = x;
        let name = ToString.toString(path);
        let f1 = newFile(name);
        directory(x1, f1);
        ()

    pub def getDirectory(x: ProcessBuilder): System.FilePath & Impure =
        import java.lang.ProcessBuilder.directory();
        import java.io.File.toPath();
        let ProcessBuilder(x1) = x;
        let p1 = x1.directory().toPath();
        FilePath(p1)




    pub def start(x: ProcessBuilder): Result[Process, String] & Impure =
        import java.lang.ProcessBuilder.start();
        try {
            let ProcessBuilder(x1) = x;
            let proc = x1.start();
            Ok(Process(proc))
        } catch {
            case _: ##java.io.IOException => Err("start")
        }
            
        

    pub def waitFor(x: Process): Result[Int32, String] & Impure =
        import java.lang.Process.waitFor();
        try {
            let Process(x1) = x;
            let retcode = x1.waitFor();
            Ok(retcode)
        } catch {
            case _: ##java.io.IOException => Err("waitFor")
        }

    pub def getInput(x:Process, cs: Text.Charset): Result[String, String] & Impure =
        import flix.runtime.spt.sandbox.system.ProcessWrapper:getInput(##java.lang.Process, ##java.nio.charset.Charset);
        try {
            let Process(x1) = x;
            let Charset(cs1) = cs;
            let str = getInput(x1, cs1);
            Ok(str)
        } catch {
            case _: ##java.lang.Exception => Err("getInput")
        }

    pub def getError(x:Process, cs: Text.Charset): Result[String, String] & Impure =
        import flix.runtime.spt.sandbox.system.ProcessWrapper:getError(##java.lang.Process, ##java.nio.charset.Charset);
        try {
            let Process(x1) = x;
            let Charset(cs1) = cs;
            let str = getError(x1, cs1);
            Ok(str)
        } catch {
            case _: ##java.lang.Exception => Err("getError")
        }

}