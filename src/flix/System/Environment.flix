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

    pub def currentTimeMilliseconds(): Int64 & Impure =
        import java.lang.System:currentTimeMillis();
        currentTimeMillis()


    /// Get env var...
    pub def getEnvVar(name: String): Result[String, String] & Impure = try {
        import java.lang.System:getenv(String);
        let ans = getenv(name);
        if (not Object.isNull(ans))
            Ok(ans)
        else
            Err("getEnvVar - not found: ${name}")
    } catch {
        case _:##java.lang.Exception => Err("getEnvVar")
    }

    pub def getEnvVars(): Result[Map[String, String], String] & Impure =
        getEnvVarsIter()
            |> Iterator.toList
            |> Result.sequence
            |> Result.map(List.toMap)

    def getEnvVarsIter(): Iterator[Result[(String, String), String]] & Impure =
        import new flix.runtime.spt.sandbox.system.GetEnvIterator() as newGetEnvIterator;
        import flix.runtime.spt.sandbox.system.GetEnvIterator.hasNext();
        import flix.runtime.spt.sandbox.system.GetEnvIterator.next();
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get0();
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get1();
        let primIter = newGetEnvIterator();
        let iterDone = () -> if (hasNext(primIter)) false else true;
        let iterNext = () -> {
            try {
                let xy = next(primIter);
                let k = get0(xy);
                let v = get1(xy);
                Ok((k, v))
            } catch {
                case _: ##java.lang.Exception => Err("getEnvVarsIter[next]")
            }
        };
        Iterator(iterDone, iterNext)


    def getProperty(funName: String, propName: String): Result[String, String] & Impure =
        import java.lang.System:getProperty(String);
        try {
            let ans = getProperty(propName);
            if (not Object.isNull(ans))
                Ok(ans)
            else
                Err("${funName}: lookup is null")
        } catch {
            case _:##java.lang.Exception => Err("${funName} - system exception")
        }


    pub def getOsName(): Result[String, String] & Impure =
        getProperty("getOsName", "os.name")

    pub def getOsArch(): Result[String, String] & Impure =
        getProperty("getOsArch", "os.arch")

    pub def getOsVersion(): Result[String, String] & Impure =
        getProperty("getOsVersion", "os.version")

    pub def getPathSeparator(): Result[String, String] & Impure =
        getProperty("getPathSeparator", "path.separator")

    pub def getUserName(): Result[String, String] & Impure =
        getProperty("getUserName", "user.name")


    pub def getUserDirectory(): Result[FilePath, String] & Impure =
        getProperty("getUserDirectory", "user.dir")
            |> Result.map(System/FilePath.new)
            |> Result.map(System/FilePath.normalize)
            |> Result.flatMap(System/FilePath.toAbsolutePath)

    pub def getUserHome(): Result[FilePath, String] & Impure =
        getProperty("getUserHome", "user.home")
            |> Result.map(System/FilePath.new)
            |> Result.map(System/FilePath.normalize)
            |> Result.flatMap(System/FilePath.toAbsolutePath)

}
