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


namespace FlixSandbox/System {
    pub opaque type ProcessBuilder = ##java.lang.ProcessBuilder

    pub opaque type Process = ##java.lang.Process
}


namespace FlixSandbox/System/Process {

    use FlixSandbox/Text.Charset.{Charset};

    use FlixSandbox/System.Process;
    use FlixSandbox/System.Process.{Process};
    use FlixSandbox/System.FilePath.{FilePath};
    use FlixSandbox/System.ProcessBuilder;
    use FlixSandbox/System.ProcessBuilder.{ProcessBuilder};


    pub def processBuilder(cmd: String, args: List[String]): ProcessBuilder & Impure =
        import new java.lang.ProcessBuilder(##java.util.List): ##java.lang.ProcessBuilder & Impure as new1;
        let strs = FlixSandbox/Marshal.marshalList(x1 -> x1 as ##java.lang.Object, cmd :: args);
        ProcessBuilder(new1(strs))


    pub def directory!(x: ProcessBuilder, path: FlixSandbox/System.FilePath): Unit & Impure =
        import java.lang.ProcessBuilder.directory(##java.io.File): ##java.lang.ProcessBuilder & Impure;
        import new java.io.File(String): ##java.io.File & Impure as newFile;
        let ProcessBuilder(x1) = x;
        let name = ToString.toString(path);
        let f1 = newFile(name);
        directory(x1, f1);
        ()

    pub def getDirectory(x: ProcessBuilder): FlixSandbox/System.FilePath & Impure =
        import java.lang.ProcessBuilder.directory(): ##java.io.File & Impure;
        import java.io.File.toPath(): ##java.nio.file.Path & Impure;
        let ProcessBuilder(x1) = x;
        let p1 = directory(x1) |> toPath;
        FilePath(p1)

    pub def start(x: ProcessBuilder): Result[Process, String] & Impure =
        import java.lang.ProcessBuilder.start(): ##java.lang.Process & Impure;
        try {
            let ProcessBuilder(x1) = x;
            let proc = start(x1);
            Ok(Process(proc))
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
        }

    pub def waitFor(x: Process): Result[Int32, String] & Impure =
        import java.lang.Process.waitFor(): Int32 & Impure;
        try {
            let Process(x1) = x;
            let retcode = waitFor(x1);
            Ok(retcode)
        } catch {
            case _: ##java.io.IOException => Err("waitFor")
        }


    pub def getInput(x: Process, c: FlixSandbox/Text.Charset): Result[String, String] & Impure =
        import java.lang.Process.getInputStream(): ##java.io.InputStream & Impure;
        import java.io.InputStream.readAllBytes(): Array[Int8] & Impure;
        import new java.lang.String(Array[Int8], ##java.nio.charset.Charset): String & Impure as new1;
        try {
            let Process(x1) = x;
            let Charset(c1) = c;
            let strm = getInputStream(x1);
            let bytes = readAllBytes(strm);
            let s1 = new1(bytes, c1);
            Ok(s1)
        } catch {
            case _: ##java.lang.Exception => Err("getInput")
        }



    pub def getError(x: Process, c: FlixSandbox/Text.Charset): Result[String, String] & Impure =        
        import java.lang.Process.getErrorStream(): ##java.io.InputStream & Impure;
        import java.io.InputStream.readAllBytes(): Array[Int8] & Impure;
        import new java.lang.String(Array[Int8], ##java.nio.charset.Charset): String & Impure as new1;
        try {
            let Process(x1) = x;
            let Charset(c1) = c;
            let strm = getErrorStream(x1);
            let bytes = readAllBytes(strm);
            let s1 = new1(bytes, c1);
            Ok(s1)
        } catch {
            case _: ##java.lang.Exception => Err("getError")
        }



    pub type alias ProcessArgs =
        { cmd :: String
        , args :: List[String]
        , charset :: FlixSandbox/Text.Charset
        }

    /// Return with an Exit Code
    pub def processWithExitCode(args: ProcessArgs): Result[{exitCode :: Int32, processResponse :: String}, String] & Impure =
        use Result.flatMap;
        let pb          = processBuilder(args.cmd, args.args);
        let* proc       = start(pb);
        let* respGood   = getInput(proc, args.charset);
        let* respBad    = getError(proc, args.charset);
        let* code       = waitFor(proc);
        Ok({exitCode = code, processResponse = if (code == 0) respGood else respBad})


    pub def process(args: ProcessArgs): Result[{processResponse :: String}, String] & Impure =
        use Result.flatMap;
        let pb          = processBuilder(args.cmd, args.args);
        let* proc       = start(pb);
        let* respGood   = getInput(proc, args.charset);
        let* respBad    = getError(proc, args.charset);
        let* code       = waitFor(proc);
        if (code == 0) 
            Ok({processResponse = respGood})
        else 
            Err(respBad)



}
