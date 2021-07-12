/*
 * Copyright 2021 Stephen Tetley
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


namespace IOPrelude {

    use System.FilePath;
    use System.FilePath.{FilePath};
    use Text.Charset;
    use Text.Charset.{Charset};

    /// See: https://github.com/flix/flix/issues/2040

    pub enum WriteMode {
        case Append
        case Create
        case CreateNew
    }

    instance ToString[WriteMode] {
        pub def toString(x: WriteMode): String = match x {
            case Append => "Append"
            case Create => "Create"
            case CreateNew => "CreateNew"
        }
    }

    instance FromString[WriteMode] {
        pub def fromString(x: String): Option[WriteMode] = match x {
            case "Append" => Some(Append)
            case "Create" => Some(Create)
            case "CreateNew" => Some(CreateNew)
            case _ => None
        }
    }

    instance Eq[WriteMode] {
        pub def eq(x: WriteMode, y: WriteMode): Bool = match (x, y) {
            case (Append, Append) => true
            case (Create, Create) => true
            case (CreateNew, CreateNew) => true
            case (_, _) => false
        }
    }


    def writeModeToNative(x: WriteMode): ##java.nio.file.StandardOpenOption =
        import get java.nio.file.StandardOpenOption:APPEND      as getAPPEND;
        import get java.nio.file.StandardOpenOption:CREATE      as getCREATE;
        import get java.nio.file.StandardOpenOption:CREATE_NEW  as getCREATE_NEW;
        match x {
            case Append     => getAPPEND()
            case Create     => getCREATE()
            case CreateNew  => getCREATE_NEW()
        } as & Pure



    pub def fileExists(path: FilePath): Result[Bool, String] & Impure =
        import flix.runtime.spt.sandbox.IOPrelude:fileExistsFollowLinks(##java.nio.file.Path);
        try {
            let FilePath(p1) = path;
            Ok(fileExistsFollowLinks(p1))
        } catch {
            case _: ##java.lang.Exception => Err("isDirectory")
        }

    pub def isDirectory(path: FilePath): Result[Bool, String] & Impure =
        import flix.runtime.spt.sandbox.IOPrelude:isDirectoryFollowLinks(##java.nio.file.Path);
        try {
            let FilePath(p1) = path;
            Ok(isDirectoryFollowLinks(p1))
        } catch {
            case _: ##java.lang.Exception => Err("isDirectory")
        }

    pub def readFile(path: FilePath, cs: Text.Charset): Result[String, String] & Impure =
        import java.nio.file.Files:readString(##java.nio.file.Path, ##java.nio.charset.Charset);
        try {
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(readString(path1, cs1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readFile")
        }

    /// Write as UTF_8...
    pub def writeFile(path: FilePath, content: String): Result[Unit, String] & Impure =
        writeFileWith({path = path, charset = Text/Charset.utf_8(), mode = Create}, content)

    pub def writeFileWith(opts: {path: FilePath, charset: Text.Charset, mode: WriteMode},
                            content: String): Result[Unit, String] & Impure =
        import flix.runtime.spt.sandbox.IOPrelude:writeString(##java.nio.file.Path, ##java.nio.charset.Charset, ##java.nio.file.StandardOpenOption, String);
        try {
            let FilePath(path1) = opts.path;
            let Charset(cs1) = opts.charset;
            let mode1 = writeModeToNative(opts.mode);
            writeString(path1, cs1, mode1, content);
            Ok()
        }
        catch {
            case _: ##java.lang.Exception => Err("writeFile")
        }

    /// Note we are reading to a *signed* Int8.
    pub def readBytes(path: FilePath): Result[Array[Int8], String] & Impure =
        import java.nio.file.Files:readAllBytes(##java.nio.file.Path);
        try {
            let FilePath(path1) = path;
            Ok(readAllBytes(path1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readBytes")
        }

    pub def writeBytes(path: FilePath, bytes: Array[Int8]): Result[Unit, String] & Impure =
        writeBytesWith({path = path, mode = Create}, bytes)

    pub def writeBytesWith(opts: {path: FilePath, mode: WriteMode}, bytes: Array[Int8]): Result[Unit, String] & Impure =
        import flix.runtime.spt.sandbox.IOPrelude:writeBytes(##java.nio.file.Path, ##java.nio.file.StandardOpenOption, Array[Int8]);
        try {
            let FilePath(path1) = opts.path;
            let mode1 = writeModeToNative(opts.mode);
            writeBytes(path1, mode1, bytes);
            Ok()
        }
        catch {
            case _: ##java.lang.Exception => Err("writeBytes")
        }


}

