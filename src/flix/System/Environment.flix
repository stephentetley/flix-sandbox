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

use System.FilePath;
use System.FilePath.{FilePath};

namespace System/Environment {

    /// Get env var...
    pub def getEnvVar(name: String): Result[String, String] & Impure = try {
        import java.lang.System:getenv(String);
        let ans = getenv(name);
        if (isNull(ans))
            Err("Not found")
        else
            Ok(ans)
    } catch {
        case _:##java.lang.Exception => Err("System exception")
    }

    pub def getEnvVars(): Result[Map[String, String], String] & Impure = try {
        import new flix.runtime.spt.sandbox.system.marshal.UnmarshalEnvVars() as newIter;
        import flix.runtime.spt.sandbox.system.marshal.UnmarshalEnvVars.hasNext();
        import flix.runtime.spt.sandbox.system.marshal.UnmarshalEnvVars.next();
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get0();
        import flix.runtime.spt.sandbox.system.marshal.TupleStringString.get1();
        let iter = newIter();
        let step = () ->
            if (iter.hasNext() == false)
                None
            else {
                let pss = iter.next();
                let k = pss.get0();
                let v = pss.get1();
                Some(k, v)
             };
        Ok(Map.unfoldWithIter(step))
    } catch {
        case _: ##java.lang.Exception => Err("getEnvVars")
    }


    pub def getOsName(): String & Impure =
        import java.lang.System:getProperty(String);
        getProperty("os.name")

    pub def getOsArch(): String & Impure =
        import java.lang.System:getProperty(String);
        getProperty("os.arch")

    pub def getOsVersion(): String & Impure =
        import java.lang.System:getProperty(String);
        getProperty("os.version")

    pub def getUserName(): String & Impure =
        import java.lang.System:getProperty(String);
        getProperty("user.name")

    pub def getUserDirectory(): FilePath & Impure =
        import java.lang.System:getProperty(String);
        let p1 = getProperty("user.dir");
        System/FilePath.new(p1)
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

    pub def getUserHome(): FilePath & Impure =
        import java.lang.System:getProperty(String);
        let p1 = getProperty("user.home");
        System/FilePath.new(p1)
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

}