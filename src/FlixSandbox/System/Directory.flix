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

    use FlixSandbox/Deprecated.Cursor;
    use FlixSandbox/Deprecated.Cursor.{Cursor};
    use FlixSandbox/System.FilePath;
    use FlixSandbox/System.FilePath.{FilePath};

    pub def getCurrentDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getCurrentWorkingDirectory() as & Impure) {
            case None    => Err("Error - getCurrentDirectory")
            case Some(s) =>
                FlixSandbox/System/FilePath.new(s)
                    |> FlixSandbox/System/FilePath.normalize
                    |> FlixSandbox/System/FilePath.toAbsolutePath
        }

    pub def getTempDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getTemporaryDirectory() as & Impure) {
            case None    => Err("Error - getTempDirectory")
            case Some(s) =>
                FlixSandbox/System/FilePath.new(s)
                    |> FlixSandbox/System/FilePath.normalize
                    |> FlixSandbox/System/FilePath.toAbsolutePath
        }


    pub def contents(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter(path)
            |> FlixSandbox/Deprecated/Cursor.toList!


    pub def contentsIter(path: FilePath): Cursor[String, FilePath] & Impure =
        import static flixspt.sandbox.PathIterator.newFileslistIterator(##java.nio.file.Path): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        let FilePath(path1) = path;
        let primIter = newFileslistIterator(path1);
        let iterNext = () -> {
            try {
                let p1 = next(primIter);
                Ok(FilePath(p1))
            } catch {
                case _: ##java.lang.Exception => Err("contentsIter[next]")
            }
        };
        let cursorAlg: FlixSandbox/Deprecated.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        FlixSandbox/Deprecated/Cursor.makeCursor(cursorAlg)

    //// Iterator<Path> iter1 = Files.list(dirpath).iterator();

    pub def contentsIter2(path: FilePath): Result[Iterator[FilePath], String] & Impure = 
        import static java.nio.file.Files.list(##java.nio.file.Path): ##java.util.stream.Stream & Impure;
        try {
            let FilePath(path1) = path;
            let strm = list(path1);
            let downcast = x1 -> FilePath(x1 as ##java.nio.file.Path);
            let iter = FlixSandbox/Marshal.unmarshalStream(downcast, strm);
            Ok(iter)
        } catch {
            case _: ##java.lang.Exception => Err("contentsIter2")
        }

    pub def contents2(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter2(path)
            |> Result.map(Iterator.toList)



    pub def matches(glob: String, path: FilePath): Result[List[FilePath], String] & Impure =
        matchesIter(glob, path)
            |> FlixSandbox/Deprecated/Cursor.toList!

    pub def matchesIter(glob: String, path: FilePath): Cursor[String, FilePath] & Impure =
        import static flixspt.sandbox.PathIterator.newFilesnewDirectoryStreamIterator(##java.nio.file.Path, String): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        let FilePath(path1) = path;
        let primIter = newFilesnewDirectoryStreamIterator(path1, glob);
        let iterNext = () -> {
            try {
                let p1 = next(primIter);
                Ok(FilePath(p1))
            } catch {
                case _: ##java.lang.Exception => Err("matchesIter[next]")
            }
        };
        let cursorAlg: FlixSandbox/Deprecated.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        FlixSandbox/Deprecated/Cursor.makeCursor(cursorAlg)

    pub def contentsAll(followLinks: Bool, path: FilePath): Result[List[FilePath], String] & Impure =
        contentsAllIter(followLinks, path)
            |> FlixSandbox/Deprecated/Cursor.toList!

    /// All levels - recursive, uses a Iterator
    pub def contentsAllIter(followLinks: Bool, path: FilePath): Cursor[String, FilePath] & Impure =
        import static flixspt.sandbox.PathIterator.newFileswalkIterator(##java.nio.file.Path, Bool): ##flixspt.sandbox.PathIterator & Impure;
        import flixspt.sandbox.PathIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.PathIterator.next(): ##java.nio.file.Path & Impure;
        let FilePath(path1) = path;
        let primIter = newFileswalkIterator(path1, followLinks);
        let iterNext = () -> {
            try {
                let p1 = next(primIter);
                Ok(FilePath(p1))
            } catch {
                case _: ##java.lang.Exception => Err("contentsAllIter[next]")
            }
        };
        let cursorAlg: FlixSandbox/Deprecated.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        FlixSandbox/Deprecated/Cursor.makeCursor(cursorAlg)


    pub def createDirectory(path: FilePath): Unit & Impure =
        import static flixspt.sandbox.FilesWrapper.createDirectory(##java.nio.file.Path): Unit & Impure;
        let FilePath(path1) = path;
        createDirectory(path1)

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
