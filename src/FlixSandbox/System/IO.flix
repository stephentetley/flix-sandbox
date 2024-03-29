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


namespace FlixSandbox/System/IO {


    use Interop/System.FilePath;
    use Interop/System.FilePath.{FilePath};
    use Interop/Text.Charset;
    use Interop/Text.Charset.{Charset};

    /// See: https://github.com/flix/flix/issues/2040

    pub enum WriteMode with Eq, Order, ToString {
        case Append
        case Create
        case CreateNew
    }

    instance FromString[WriteMode] {
        pub def fromString(x: String): Option[WriteMode] = match x {
            case "Append" => Some(Append)
            case "Create" => Some(Create)
            case "CreateNew" => Some(CreateNew)
            case _ => None
        }
    }


    def writeModeToNative(x: WriteMode): ##java.nio.file.StandardOpenOption =
        import static get java.nio.file.StandardOpenOption.APPEND: ##java.nio.file.StandardOpenOption & Pure as getAPPEND;
        import static get java.nio.file.StandardOpenOption.CREATE: ##java.nio.file.StandardOpenOption & Pure as getCREATE;
        import static get java.nio.file.StandardOpenOption.CREATE_NEW: ##java.nio.file.StandardOpenOption & Pure  as getCREATE_NEW;
        match x {
            case Append     => getAPPEND()
            case Create     => getCREATE()
            case CreateNew  => getCREATE_NEW()
        }

    /// # Queries

    pub def fileExists(path: FilePath): Result[Bool, String] & Impure =
        import static flixspt.sandbox.IOWrapper.fileExistsFollowLinks(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(p1) = path;
            Ok(fileExistsFollowLinks(p1))
        } catch {
            case _: ##java.lang.Exception => Err("fileExists")
        }

    pub def isFile(path: FilePath): Result[Bool, String] & Impure =
        import static flixspt.sandbox.IOWrapper.isRegularFileFollowLinks(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(path1) = path;
            Ok(isRegularFileFollowLinks(path1))
        } catch {
            case _: ##java.lang.Exception => Err("isFile")
        }


    pub def isDirectory(path: FilePath): Result[Bool, String] & Impure =
        import static flixspt.sandbox.IOWrapper.isDirectoryFollowLinks(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(path1) = path;
            Ok(isDirectoryFollowLinks(path1))
        } catch {
            case _: ##java.lang.Exception => Err("isDirectory")
        }


    pub def isSymbolicLink(path: FilePath): Result[Bool, String] & Impure =
        import static java.nio.file.Files.isSymbolicLink(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(path1) = path;
            Ok(isSymbolicLink(path1))
        } catch {
            case _: ##java.lang.Exception => Err("isSymbolicLink")
        }

    pub def isHidden(path: FilePath): Result[Bool, String] & Impure =
        import static java.nio.file.Files.isHidden(##java.nio.file.Path): Bool & Impure;
        try {
            let FilePath(path1) = path;
            Ok(isHidden(path1))
        } catch {
            case _: ##java.lang.Exception => Err("isHidden")
        }


    /// #

    pub def readFile(cs: Charset, path: FilePath): Result[String, String] & Impure =
        import static java.nio.file.Files.readString(##java.nio.file.Path, ##java.nio.charset.Charset): String & Impure;
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
        writeFileWith({path = path, charset = Interop/Text/Charset.utf_8(), mode = Create}, content)

    pub def writeFileWith(opts: {path :: FilePath, charset :: Charset, mode:: WriteMode},
                            content: String): Result[Unit, String] & Impure =
        import static flixspt.sandbox.IOWrapper.writeString(##java.nio.file.Path, ##java.nio.charset.Charset, ##java.nio.file.StandardOpenOption, String): Unit & Impure;
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
    pub def readBytes(path: FilePath): Result[Array[Int8, Static], String] & Impure =
        import static java.nio.file.Files.readAllBytes(##java.nio.file.Path): Array[Int8, Static] & Impure;
        try {
            let FilePath(path1) = path;
            Ok(readAllBytes(path1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readBytes")
        }


    
    pub def writeBytes(path: FilePath, bytes: Array[Int8, Static]): Result[Unit, String] & Impure =
        writeBytesWith({path = path, mode = Create}, bytes)

    pub def writeBytesWith(opts: {path :: FilePath, mode :: WriteMode}, bytes: Array[Int8, Static]): Result[Unit, String] & Impure =
        import static flixspt.sandbox.IOWrapper.writeBytes(##java.nio.file.Path, ##java.nio.file.StandardOpenOption, Array[Int8, Static]): Unit & Impure;
        try {
            let FilePath(path1) = opts.path;
            let mode1 = writeModeToNative(opts.mode);
            writeBytes(path1, mode1, bytes);
            Ok()
        }
        catch {
            case _: ##java.lang.Exception => Err("writeBytes")
        }

    pub def foldableWriteBytesWith[t: Type -> Type](opts: {path :: FilePath, mode :: WriteMode},
                                                    bytes: t[Int8]): Result[Unit, String] & Impure with Foldable[t] =
        import static flixspt.sandbox.IOWrapper.openFileOutputStream(##java.nio.file.Path, ##java.nio.file.StandardOpenOption): ##java.io.OutputStream & Impure;
        import static flixspt.sandbox.IOWrapper.outputStreamWriteByte(##java.io.OutputStream, Int8): Unit & Impure;
        import java.io.OutputStream.close(): Unit & Impure;
        try {
            let FilePath(path1) = opts.path;
            let mode1 = writeModeToNative(opts.mode);
            let outstream = openFileOutputStream(path1, mode1);
            Foldable.foldLeft((_, x) -> outputStreamWriteByte(outstream, x), (), bytes);
            let _ = close(outstream);
            Ok()
        }
        catch {
            case _: ##java.lang.Exception => Err("foldableWriteBytesWith")
        }

    pub def readChunks(path: FilePath, chunkSize: Int32): Result[Iterator[Array[Int8, Static], Static], String] & Impure =
        import new flixspt.sandbox.SBCIterator(##java.nio.file.Path, Int32): ##flixspt.sandbox.SBCIterator & Impure as newIter;
        import flixspt.sandbox.SBCIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.SBCIterator.nextBufferSize(): Int32 & Impure;
        import flixspt.sandbox.SBCIterator.next(): Array[Int8, Static] & Impure;
        try {
            let FilePath(path1) = path;
            let primIter = newIter(path1, chunkSize);
            let iterDone = () -> not hasNext(primIter);
            let iterNext = () -> {
                let sz = nextBufferSize(primIter);
                let arr = next(primIter);
                if (sz < chunkSize)
                    Array.slice(0, sz, arr)
                else 
                    arr
            };
            Ok(Iterator(iterDone, iterNext))
        } catch {
            case _: ##java.lang.Exception => Err("readChunks")
        }

    pub def readLines(opts: {charset :: Charset}, path: FilePath): Result[List[String], String] & Impure =
        readLinesIterWith(opts, path) 
            |> Result.map(Iterator.toList)


    pub def readLinesIterWith(opts: {charset :: Charset}, path: FilePath): Result[Iterator[String, Static], String] & Impure =
        import static flixspt.sandbox.LineIterator.newFileslinesIterator(##java.nio.file.Path, ##java.nio.charset.Charset): ##flixspt.sandbox.LineIterator & Impure;
        import flixspt.sandbox.LineIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.LineIterator.next(): String & Impure;
        try {
            let FilePath(path1) = path;
            let Charset(charset1) = opts.charset;
            let primIter = newFileslinesIterator(path1, charset1);
            let done1 = () -> not hasNext(primIter);
            let next1 = () -> next(primIter);
            Ok(Iterator(done1, next1))
        } catch {
            case _: ##java.lang.Exception => Err("readLinesIterWith")
        }

    pub def contents(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter(path)
            |> Result.map(Iterator.toList)


    pub def contentsIter(path: FilePath): Result[Iterator[FilePath, Static], String] & Impure =
        import static flixspt.sandbox.PathIterator.newFileslistIterator(##java.nio.file.Path): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        try {
            let FilePath(path1) = path;
            let primIter = newFileslistIterator(path1);
            let done1 = () -> not hasNext(primIter);
            let next1 = () -> FilePath(next(primIter));
            Ok(Iterator(done1, next1))
        } catch {
            case _: ##java.lang.Exception => Err("readLinesIterWith")
        }

    pub def matches(glob: String, path: FilePath): Result[List[FilePath], String] & Impure =
        matchesIter(glob, path)
            |> Result.map(Iterator.toList)

    pub def matchesIter(glob: String, path: FilePath): Result[Iterator[FilePath, Static], String] & Impure =
        import static flixspt.sandbox.PathIterator.newFilesnewDirectoryStreamIterator(##java.nio.file.Path, String): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        try {
            let FilePath(path1) = path;
            let primIter = newFilesnewDirectoryStreamIterator(path1, glob);
            let done1 = () -> not hasNext(primIter);
            let next1 = () -> FilePath(next(primIter));
            Ok(Iterator(done1, next1))
        } catch {
            case _: ##java.lang.Exception => Err("matchesIter")
        }

}
