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

use System.Error.{FileNotFoundException, IOException, Generic};
use System.FilePath;
use System.FilePath.{FilePath};
use Text.Charset.{Charset};

namespace System/File {

    pub def exists(path: FilePath): Result[Bool, System.Error] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:exists(##java.nio.file.Path);
            let FilePath(path1) = path;
            Ok(exists(path1))
        } catch {
            case _: ##java.lang.SecurityException => Err(IOException)
        }


    pub def delete(path: FilePath): Result[Unit, System.Error] & Impure =
        try {
            import java.nio.file.Files:delete(##java.nio.file.Path);
            let FilePath(path1) = path;
            delete(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err(IOException)
        }

    pub def deleteIfExists(path: FilePath): Result[Unit, System.Error] & Impure =
        try {
            import java.nio.file.Files:deleteIfExists(##java.nio.file.Path);
            let FilePath(path1) = path;
            deleteIfExists(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err(IOException)
        }

    /// TODO - test exception
    pub def readFile(path: FilePath, cs: Text.Charset): Result[String, System.Error] & Impure =
        try {
            import java.nio.file.Files:readString(##java.nio.file.Path, ##java.nio.charset.Charset);
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(readString(path1, cs1))
        }
        catch {
            case _: ##java.io.FileNotFoundException => Err(FileNotFoundException)
            case _: ##java.io.IOException => Err(IOException)
            case _: ##java.lang.Exception => Err(Generic("Exception"))
        }

    pub def readFileWithBOM(path: FilePath, cs: Text.Charset): Result[String, System.Error] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:readFileWithBOM(##java.nio.file.Path,
                                                                               ##java.nio.charset.Charset) as readWithBOM;
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(readWithBOM(path1, cs1))
        }
        catch {
            case _: ##java.io.FileNotFoundException => Err(FileNotFoundException)
            case _: ##java.io.IOException => Err(IOException)
            case _: ##java.lang.Exception => Err(Generic("Exception"))
        }




    pub def writeFile(path: FilePath, cs: Text.Charset, content: String): Result[Unit, System.Error] & Impure =
        try {
            import flix.runtime.spt.sandbox.system.FileWrapper:writeString(##java.nio.file.Path, ##java.nio.charset.Charset, String);
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            writeString(path1, cs1, content);
            Ok()
        }
        catch {
            case _: ##java.io.IOException => Err(IOException)
            case _: ##java.lang.Exception => Err(Generic("Exception"))
        }
}