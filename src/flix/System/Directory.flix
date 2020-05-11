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

namespace System/Directory {

    pub def getCurrentDirectory(): FilePath & Impure =
        System/FilePath.new(".")
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

    pub def getTempDirectory(): FilePath & Impure =
        import java.lang.System:getProperty(String);
        let p1 = getProperty("java.io.tmpdir");
        System/FilePath.new(p1)
            |> System/FilePath.normalize
            |> System/FilePath.toAbsolutePath

    /// One level - not recursive
    /// TODO - return a Stream
    pub def contents(path: FilePath): Result[List[FilePath], String] & Impure = try {
        import new flix.runtime.library.system.marshal.UnmarshalFilesList(##java.nio.file.Path) as newIter;
        import flix.runtime.library.system.marshal.UnmarshalFilesList.hasNext();
        import flix.runtime.library.system.marshal.UnmarshalFilesList.next();
        let FilePath(p1) = path;
        let iter = newIter(p1);
        let step = () ->
            if (iter.hasNext() == false)
                None
            else {
                let x1 = iter.next();
                Some(FilePath(x1))
             };
        Ok(System/Internal/Utils.unfoldListWithIterator(step))
    } catch {
        case _: ##java.lang.Exception => Err("contents")
    }

    /// All levels - recursive
    /// TODO - return a Stream
    pub def contentsAll(path: FilePath, followLinks: Bool): Result[List[FilePath], String] & Impure = try {
        import new flix.runtime.library.system.marshal.UnmarshalFilesWalk(##java.nio.file.Path, Bool) as newIter;
        import flix.runtime.library.system.marshal.UnmarshalFilesWalk.hasNext();
        import flix.runtime.library.system.marshal.UnmarshalFilesWalk.next();
        let FilePath(p1) = path;
        let iter = newIter(p1, followLinks);
        let step = () ->
            if (iter.hasNext() == false)
                None
            else {
                let x1 = iter.next();
                Some(FilePath(x1))
             };
        Ok(System/Internal/Utils.unfoldListWithIterator(step))
    } catch {
        case _: ##java.lang.Exception => Err("contentsAll")
    }

    pub def doesPathExist(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:existsFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.existsFollowLinks()

    pub def isDirectory(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:isDirectoryFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isDirectoryFollowLinks()

    pub def isFile(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:isRegularFileFollowLinks(##java.nio.file.Path);
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
        import flix.runtime.library.system.DirectoryWrapper:createDirectory(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.createDirectory()

    /// Create any "missing" directories along the way...
    pub def createDirectoryAll(path: FilePath): Unit & Impure =
        import flix.runtime.library.system.DirectoryWrapper:createDirectories(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.createDirectories()


}