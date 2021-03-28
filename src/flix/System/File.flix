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


namespace System/File {

    use System.FilePath;
    use System.FilePath.{FilePath};
    use Text.Charset.{Charset};

    pub def exists(path: FilePath): Result[Bool, String] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:exists(##java.nio.file.Path);
            let FilePath(path1) = path;
            Ok(exists(path1))
        } catch {
            case _: ##java.lang.SecurityException => Err("exists")
        }


    pub def delete(path: FilePath): Result[Unit, String] & Impure =
        try {
            import java.nio.file.Files:delete(##java.nio.file.Path);
            let FilePath(path1) = path;
            delete(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("delete")
        }

    pub def copy(source: FilePath, target: FilePath): Result[Unit, String] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:copy(##java.nio.file.Path, ##java.nio.file.Path);
            let FilePath(source1) = source;
            let FilePath(target1) = target;
            let _ = copy(source1, target1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("copy")
        }


    pub def deleteIfExists(path: FilePath): Result[Unit, String] & Impure =
        try {
            import java.nio.file.Files:deleteIfExists(##java.nio.file.Path);
            let FilePath(path1) = path;
            deleteIfExists(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("deleteIfExists")
        }

    /// TODO - test exception
    pub def readFile(path: FilePath, cs: Text.Charset): Result[String, String] & Impure =
        try {
            import java.nio.file.Files:readString(##java.nio.file.Path, ##java.nio.charset.Charset);
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(readString(path1, cs1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readFile")
        }

    pub def readFileWithBOM(path: FilePath, cs: Text.Charset): Result[String, String] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:readFileWithBOM(##java.nio.file.Path,
                                                                               ##java.nio.charset.Charset) as readWithBOM;
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(readWithBOM(path1, cs1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readFileWithBOM")
        }




    pub def writeFile(path: FilePath, cs: Text.Charset, content: String): Result[Unit, String] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:writeString(##java.nio.file.Path, ##java.nio.charset.Charset, String);
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            writeString(path1, cs1, content);
            Ok()
        }
        catch {
            case _: ##java.lang.Exception => Err("writeFile")
        }
}