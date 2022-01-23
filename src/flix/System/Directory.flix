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


namespace System/Directory {

    use X.Cursor;
    use X.Cursor.{Cursor};
    use System.FilePath;
    use System.FilePath.{FilePath};

    pub def getCurrentDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getCurrentWorkingDirectory() as & Impure) {
            case None    => Err("Error - getCurrentDirectory")
            case Some(s) =>
                System/FilePath.new(s)
                    |> System/FilePath.normalize
                    |> System/FilePath.toAbsolutePath
        }

    pub def getTempDirectory(): Result[FilePath, String] & Impure =
        match (Environment.getTemporaryDirectory() as & Impure) {
            case None    => Err("Error - getTempDirectory")
            case Some(s) =>
                System/FilePath.new(s)
                    |> System/FilePath.normalize
                    |> System/FilePath.toAbsolutePath
        }


    pub def contents(path: FilePath): Result[List[FilePath], String] & Impure =
        contentsIter(path)
            |> X/Cursor.toList!


    pub def contentsIter(path: FilePath): Cursor[String, FilePath] & Impure =
        import static flix.runtime.spt.sandbox.system.PathIterator.newFileslistIterator(##java.nio.file.Path);
        import flix.runtime.spt.sandbox.system.PathIterator.hasNext();
        import flix.runtime.spt.sandbox.system.PathIterator.next();
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
        let cursorAlg: X.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        X/Cursor.makeCursor(cursorAlg)

    pub def matches(glob: String, path: FilePath): Result[List[FilePath], String] & Impure =
        matchesIter(glob, path)
            |> X/Cursor.toList!

    pub def matchesIter(glob: String, path: FilePath): Cursor[String, FilePath] & Impure =
        import static flix.runtime.spt.sandbox.system.PathIterator.newFilesnewDirectoryStreamIterator(##java.nio.file.Path, String);
        import flix.runtime.spt.sandbox.system.PathIterator.hasNext();
        import flix.runtime.spt.sandbox.system.PathIterator.next();
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
        let cursorAlg: X.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        X/Cursor.makeCursor(cursorAlg)

    pub def contentsAll(followLinks: Bool, path: FilePath): Result[List[FilePath], String] & Impure =
        contentsAllIter(followLinks, path)
            |> X/Cursor.toList!

    /// All levels - recursive, uses a Iterator
    pub def contentsAllIter(followLinks: Bool, path: FilePath): Cursor[String, FilePath] & Impure =
        import static flix.runtime.spt.sandbox.system.PathIterator.newFileswalkIterator(##java.nio.file.Path, Bool);
        import flix.runtime.spt.sandbox.system.PathIterator.hasNext();
        import flix.runtime.spt.sandbox.system.PathIterator.next();
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
        let cursorAlg: X.CursorAlg[String, FilePath] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        X/Cursor.makeCursor(cursorAlg)


    pub def createDirectory(path: FilePath): Unit & Impure =
        import static flix.runtime.spt.sandbox.system.FilesWrapper.createDirectory(##java.nio.file.Path);
        let FilePath(path1) = path;
        createDirectory(path1)

    /// Create any "missing" directories along the way...
    pub def createDirectoryAll(path: FilePath): Unit & Impure =
        import static flix.runtime.spt.sandbox.system.FilesWrapper.createDirectories(##java.nio.file.Path);
        let FilePath(path1) = path;
        createDirectories(path1)


    /// If path is a symbolic link the link is deleted.
    /// If `path` is a directory, the directory must be empty.
    /// If `path` does not exist, throw and error.
    pub def delete(path: FilePath): Result[Unit, String] & Impure =
        import static java.nio.file.Files.delete(##java.nio.file.Path);
        try {
            let FilePath(path1) = path;
            delete(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("delete")
        }

    /// As `delete` but returns `Ok()` if the file does not exist.
    pub def deleteIfExists(path: FilePath): Result[Unit, String] & Impure =
        import static java.nio.file.Files.deleteIfExists(##java.nio.file.Path);
        try {
            let FilePath(path1) = path;
            deleteIfExists(path1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("deleteIfExists")
        }


    pub def copy(source: FilePath, target: FilePath): Result[Unit, String] & Impure =
        import static flix.runtime.spt.sandbox.system.FilesWrapper.copy(##java.nio.file.Path, ##java.nio.file.Path);
        try {
            let FilePath(source1) = source;
            let FilePath(target1) = target;
            let _ = copy(source1, target1);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("copy")
        }

}
