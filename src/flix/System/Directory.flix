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


    use System.FilePath;
    use System.FilePath.{FilePath};

    pub def getCurrentDirectory(): Result[FilePath, String] & Impure =
        System/Environment.getUserDirectory()

    pub def getTempDirectory(): Result[FilePath, String] & Impure =
        import java.lang.System:getProperty(String);
        let p1 = getProperty("java.io.tmpdir");
        if (Object.isNull(p1))
            Err("getTempDirectory")
        else
            System/FilePath.new(p1)
                |> System/FilePath.normalize
                |> System/FilePath.toAbsolutePath


    /// One level - not recursive, returns a Cursor
    pub def contents(path: FilePath): Cursor[String, FilePath] & Impure =
        import flix.runtime.spt.sandbox.system.FilesPathCursor:createFilesListCursor(##java.nio.file.Path) as newCursor;
        import flix.runtime.spt.sandbox.system.FilesPathCursor.hasNext() as ffiHasNext;
        import flix.runtime.spt.sandbox.system.FilesPathCursor.next() as ffiNext;
        try {
            let FilePath(path1) = path;
            let iter = newCursor(path1);
            let cursorAlg =  {
                  hasNext = () -> ffiHasNext(iter)
                , next = Cursor.guardThunk(_ -> FilePath(ffiNext(iter)), "next failed")
                , finalize = () -> () as & Impure
            };
            Cursor.makeCursor(cursorAlg)
        } catch {
            case _: ##java.lang.Exception => Cursor.failure("Directory.contents")
        }

    /// All levels - recursive, uses a Cursor
    pub def contentsAll(path: FilePath, followLinks: Bool): Cursor[String, FilePath] & Impure =
        import flix.runtime.spt.sandbox.system.FilesPathCursor:createFilesWalkCursor(##java.nio.file.Path, Bool) as newCursor;
        import flix.runtime.spt.sandbox.system.FilesPathCursor.hasNext() as ffiHasNext;
        import flix.runtime.spt.sandbox.system.FilesPathCursor.next() as ffiNext;
        try {
            let FilePath(path1) = path;
            let iter = newCursor(path1, followLinks);
            let cursorAlg =  {
                  hasNext = () -> ffiHasNext(iter)
                , next = Cursor.guardThunk(_ -> FilePath(ffiNext(iter)), "next failed")
                , finalize = () -> () as & Impure
            };
            Cursor.makeCursor(cursorAlg)
        } catch {
            case _: ##java.lang.Exception => Cursor.failure("Directory.contentsAll")
        }

    pub def doesPathExist(path: FilePath): Bool & Impure =
        import flix.runtime.spt.sandbox.system.DirectoryWrapper:existsFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.existsFollowLinks()

    pub def isDirectory(path: FilePath): Bool & Impure =
        import flix.runtime.spt.sandbox.system.DirectoryWrapper:isDirectoryFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isDirectoryFollowLinks()

    pub def isFile(path: FilePath): Bool & Impure =
        import flix.runtime.spt.sandbox.system.DirectoryWrapper:isRegularFileFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isRegularFileFollowLinks()

    pub def isSymbolicLink(path: FilePath): Bool & Impure =
        import java.nio.file.Files:isSymbolicLink(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isSymbolicLink()

    pub def isHidden(path: FilePath): Bool & Impure =
        import java.nio.file.Files:isHidden(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isHidden()

    pub def createDirectory(path: FilePath): Unit & Impure =
        import flix.runtime.spt.sandbox.system.DirectoryWrapper:createDirectory(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.createDirectory()

    /// Create any "missing" directories along the way...
    pub def createDirectoryAll(path: FilePath): Unit & Impure =
        import flix.runtime.spt.sandbox.system.DirectoryWrapper:createDirectories(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.createDirectories()


}