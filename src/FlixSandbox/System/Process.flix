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

    pub enum Process(##java.lang.Process)
}


namespace FlixSandbox/System/Process {

    use FlixSandbox/Text.Charset.{Charset};

    use FlixSandbox/System.Process;
    use FlixSandbox/System.Process.{Process};
    use FlixSandbox/System.FilePath.{FilePath};
    use FlixSandbox/System.ProcessBuilder;
    use FlixSandbox/System.ProcessBuilder.{ProcessBuilder};


    pub def processBuilder(cmd: String, args: List[String]): ProcessBuilder[ka, Unit] =
        FlixSandbox/System/ProcessBuilder.command(cmd, args)


    pub def waitFor(p: Process): Result[Int32, String] & Impure =
        import java.lang.Process.waitFor(): Int32 & Impure;
        try {
            let Process(p1) = p;
            let retcode = waitFor(p1);
            Ok(retcode)
        } catch {
            case ex: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
        }


    pub def getInput(cs: FlixSandbox/Text.Charset, p: Process): Result[String, String] & Impure =
        import java.lang.Process.getInputStream(): ##java.io.InputStream & Impure;
        import java.io.InputStream.readAllBytes(): Array[Int8, Static] & Impure;
        import new java.lang.String(Array[Int8, Static], ##java.nio.charset.Charset): String & Impure as newString;
        try {
            let Process(p1) = p;
            let Charset(cs1) = cs;
            let strm = getInputStream(p1);
            let bytes = readAllBytes(strm);
            let s1 = newString(bytes, cs1);
            Ok(s1)
        } catch {
            case ex: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
        }


    pub def getError(cs: FlixSandbox/Text.Charset, p: Process): Result[String, String] & Impure =        
        import java.lang.Process.getErrorStream(): ##java.io.InputStream & Impure;
        import java.io.InputStream.readAllBytes(): Array[Int8, Static] & Impure;
        import new java.lang.String(Array[Int8, Static], ##java.nio.charset.Charset): String & Impure as newString;
        try {
            let Process(p1) = p;
            let Charset(cs1) = cs;
            let strm = getErrorStream(p1);
            let bytes = readAllBytes(strm);
            let s1 = newString(bytes, cs1);
            Ok(s1)
        } catch {
            case ex: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
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
        let* proc       = FlixSandbox/System/ProcessBuilder.start(pb);
        let* respGood   = getInput(args.charset, proc);
        let* respBad    = getError(args.charset, proc);
        let* code       = waitFor(proc);
        Ok({exitCode = code, processResponse = if (code == 0) respGood else respBad})


    pub def process(args: ProcessArgs): Result[{processResponse :: String}, String] & Impure =
        use Result.flatMap;
        let pb          = processBuilder(args.cmd, args.args);
        let* proc       = FlixSandbox/System/ProcessBuilder.start(pb);
        let* respGood   = getInput(args.charset, proc);
        let* respBad    = getError(args.charset, proc);
        let* code       = waitFor(proc);
        if (code == 0) 
            Ok({processResponse = respGood})
        else 
            Err(respBad)



}
