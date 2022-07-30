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


namespace FlixSandbox/System/Directory {


    use Interop/System.FilePath;
    use Interop/System.FilePath.{FilePath};

    pub def getCurrentDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getCurrentWorkingDirectory() as & Impure) {
            case None    => Err("Error - getCurrentDirectory")
            case Some(s) =>
                Interop/System/FilePath.filePath(s)
                    |> Interop/System/FilePath.normalize
                    |> Interop/System/FilePath.toAbsolutePath
        }

    pub def getTempDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getTemporaryDirectory() as & Impure) {
            case None    => Err("Error - getTempDirectory")
            case Some(s) =>
                Interop/System/FilePath.filePath(s)
                    |> Interop/System/FilePath.normalize
                    |> Interop/System/FilePath.toAbsolutePath
        }


    /// Use File.list
    pub def contents(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter(path)
            |> Result.map(Iterator.toList)

    /// Use File.listWithIter
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
            case ex: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err("contentsIter: ${getMessage(ex)}")
        }

    pub def contentsIter2(path: FilePath): Result[Iterator[FilePath, Static], String] & Impure = 
        import static java.nio.file.Files.list(##java.nio.file.Path): ##java.util.stream.Stream & Impure;
        try {
            let FilePath(path1) = path;
            let strm = Interop/Collections/Stream.new(Static, list(path1));
            let iter = Interop/Collections/Stream.iterator(strm);
            Ok(iter)
        } catch {
            case ex: ##java.lang.Exception =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err("contentsIter2: ${getMessage(ex)}")
        }

    pub def contents2(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter2(path)
            |> Result.map(Iterator.toList)



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

    pub def contentsAll(followLinks: Bool, path: FilePath): Result[List[FilePath], String] & Impure =
        contentsAllIter(followLinks, path)
            |> Result.map(Iterator.toList)

    /// Use `files.walk`...
    /// All levels - recursive, uses a Iterator
    pub def contentsAllIter(followLinks: Bool, path: FilePath): Result[Iterator[FilePath, Static], String] & Impure =
        import static flixspt.sandbox.PathIterator.newFileswalkIterator(##java.nio.file.Path, Bool): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        try {
            let FilePath(path1) = path;
            let primIter = newFileswalkIterator(path1, followLinks);
            let done1 = () -> not hasNext(primIter);
            let next1 = () -> FilePath(next(primIter));
            Ok(Iterator(done1, next1))
        } catch {
            case _: ##java.lang.Exception => Err("contentsAllIter")
        }

    /// Use File.mkdir...
    pub def createDirectory(path: FilePath): Unit & Impure =
        import static flixspt.sandbox.FilesWrapper.createDirectory(##java.nio.file.Path): Unit & Impure;
        let FilePath(path1) = path;
        createDirectory(path1)

    /// Use File.mkdirs...
    /// Create any "missing" directories along the way...
    pub def createDirectoryAll(path: FilePath): Unit & Impure =
        import static flixspt.sandbox.FilesWrapper.createDirectories(##java.nio.file.Path): Unit & Impure;
        let FilePath(path1) = path;
        createDirectories(path1)


    /// If path is a symbolic link the link is deleted.
    /// If `path` is a directory, the directory must be empty.
    /// If `path` does not exist, throw and error.
    pub def delete(path: FilePath): Result[Unit, String] & Impure =
        import static java.nio.file.Files.delete(##java.nio.file.Path): Unit & Impure;
        try {
            let FilePath(path1) = path;
            delete(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("delete")
        }

    /// As `delete` but returns `Ok()` if the file does not exist.
    pub def deleteIfExists(path: FilePath): Result[Unit, String] & Impure =
        import static java.nio.file.Files.deleteIfExists(##java.nio.file.Path): Unit & Impure;
        try {
            let FilePath(path1) = path;
            deleteIfExists(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("deleteIfExists")
        }

    /// Use Files.copy...
    pub def copy(source: FilePath, target: FilePath): Result[Unit, String] & Impure =
        import static flixspt.sandbox.FilesWrapper.copy(##java.nio.file.Path, ##java.nio.file.Path): ##java.nio.file.Path & Impure;
        try {
            let FilePath(source1) = source;
            let FilePath(target1) = target;
            let _ = copy(source1, target1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("copy")
        }

}
