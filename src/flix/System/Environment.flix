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



namespace System/Environment {

    use System.FilePath;
    use System.FilePath.{FilePath};

    /// Get env var...
    pub def getEnvVar(name: String): Option[String] & Impure = try {
        import java.lang.System:getenv(String);
        let ans = getenv(name);
        if (not Object.isNull(ans))
            Some(ans)
        else
            None
    } catch {
        case _:##java.lang.Exception => None
    }

    pub def getEnvVars(): Option[Map[String, String]] & Impure =
        import new flix.runtime.spt.sandbox.system.GetEnvCursor() as newCursor;
        import flix.runtime.spt.sandbox.system.GetEnvCursor.hasNext() as ffiHasNext;
        import flix.runtime.spt.sandbox.system.GetEnvCursor.next() as ffiNext;
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get0();
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get1();
        try {
            let iter = newCursor();
            let nextF = Cursor.guardThunk(_ -> {
                let tss = ffiNext(iter);
                let k = get0(tss);
                let v = get1(tss);
                (k, v)
            }, "next failed");
            let cursorAlg =  {
                  hasNext = () -> ffiHasNext(iter)
                , next = nextF
                , finalize = () -> () as & Impure
            };
            Cursor.makeCursor(cursorAlg)
                |> Cursor.toMap
                |> Result.toOption
         } catch {
            case _: ##java.lang.Exception => None
        }


    pub def getOsName(): String =
        import java.lang.System:getProperty(String);
        getProperty("os.name") as & Pure

    pub def getOsArch(): String =
        import java.lang.System:getProperty(String);
        getProperty("os.arch") as & Pure

    pub def getOsVersion(): String =
        import java.lang.System:getProperty(String);
        getProperty("os.version") as & Pure

    pub def getPathSeparator(): String =
        import java.lang.System:getProperty(String);
        getProperty("path.separator") as & Pure


    pub def getUserName(): String =
        import java.lang.System:getProperty(String);
        getProperty("user.name") as & Pure

    pub def getUserDirectory(): FilePath & Impure =
        import java.lang.System:getProperty(String);
        // Assume "user.dir" can change during a programs lifetime
        let p1 = getProperty("user.dir");
        System/FilePath.new(p1)
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

    pub def getUserHome(): FilePath =
        import java.lang.System:getProperty(String);
        let p1 = getProperty("user.home") as & Pure;
        System/FilePath.new(p1)
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

}