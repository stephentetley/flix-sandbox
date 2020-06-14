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

namespace System {

    ///
    /// Represents a FilePath.
    ///
    pub opaque type FilePath = ##java.nio.file.Path

}

namespace System/FilePath {

    /// Note - current set of operations should be pure?

    ///
    /// Returns a new FilePath.
    ///
    pub def new(s: String): FilePath =
        import flix.runtime.spt.sandbox.system.FilePathWrapper:get1(String) as get1;
        FilePath(get1(s)) as & Pure

    ///
    /// Returns `true` if `x` == `y`.
    ///
    pub def equals(x: FilePath, y: FilePath): Bool =
        import java.nio.file.Path.equals(##java.lang.Object);
        let FilePath(x1) = x;
        let FilePath(y1) = y;
        x1.equals(y1 as ##java.lang.Object) as & Pure

    pub def toString(path: FilePath): String =
        import java.nio.file.Path.toString();
        let FilePath(path1) = path;
        path1.toString() as & Pure

    pub def normalize(path: FilePath): FilePath =
        import java.nio.file.Path.normalize();
        let FilePath(p1) = path;
        FilePath(p1.normalize()) as & Pure

    pub def toAbsolutePath(path: FilePath): FilePath =
        import java.nio.file.Path.toAbsolutePath();
        let FilePath(p1) = path;
        FilePath(p1.toAbsolutePath()) as & Pure

    pub def isAbsolute(path: FilePath): Bool =
        import java.nio.file.Path.isAbsolute();
        let FilePath(path1) = path;
        path1.isAbsolute() as & Pure

    pub def isRelative(path: FilePath): Bool = !(isAbsolute(path))

    pub def getRoot(path: FilePath): Option[FilePath] =
        import java.nio.file.Path.getRoot();
        let FilePath(path1) = path;
        let root = path1.getRoot() as & Pure;
        if (Object.isNull(root))
            None
        else
            Some(FilePath(root))

    pub def getParent(path: FilePath): Option[FilePath] =
        import java.nio.file.Path.getParent();
        let FilePath(path1) = path;
        let parent = path1.getParent() as & Pure;
        if (Object.isNull(parent))
            None
        else
            Some(FilePath(parent))

    pub def getFileName(path: FilePath): Option[FilePath] =
        import java.nio.file.Path.getFileName();
        let FilePath(path1) = path;
        let name = path1.getFileName() as & Pure;
        if (Object.isNull(name))
            None
        else
            Some(FilePath(name))

    pub def append(path: FilePath, other: FilePath): FilePath =
        import java.nio.file.Path.resolve(##java.nio.file.Path);
        let FilePath(p1) = path;
        let FilePath(p2) = other;
        let ans = FilePath(p1.resolve(p2)) as & Pure;
        normalize(ans)

    pub def getNameCount(path: FilePath): Int32 =
        import java.nio.file.Path.getNameCount();
        let FilePath(path1) = path;
        path1.getNameCount() as & Pure

    pub def getNameAt(ix: Int32, path: FilePath): Option[FilePath] =
        import java.nio.file.Path.getName(Int32);
        try {
            let FilePath(path1) = path;
            let name1 = path1.getName(ix) as & Pure;
            Some(FilePath(name1))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def hasExtension(path: FilePath): Bool =
        match getFileName(path) |> Option.map(toString) {
            case None => false
            case Some(str) => String.isInfixOf(".", str)
        }


    pub def getExtension(path: FilePath): Option[String] =
        use Option.flatMap;
        let* s = getFileName(path) |> Option.map(toString);
        let* ix = String.indexOfRight(".", s);
        Some(String.sliceRight(s, ix+1))

    ///
    /// Replaces the extension.
    /// If the filepath has multiple extenstion only the rightmost extension
    /// is replaced.
    ///
    pub def putExtension(path: FilePath, ext: String): FilePath =
        match getExtension(path) {
            case Some(ext0) => {
                let str = toString(path);
                let lhs = String.dropRight(String.length(ext0), str);
                new(lhs + ext)
            }
            case None => path
        }

    ///
    /// Drop only the rightmost extension if the filepath has mutliple
    /// extensions, i.e. "archive.tar.gz" becomes "archive.tar".
    ///
    pub def dropExtension(path: FilePath): FilePath = match getExtension(path) {
        case Some(ext) => {
            let str = toString(path);
            let lhs = String.dropRight(String.length(ext) + 1, str);
            new(lhs)
        }
        case None => path
    }

    pub def splitExtension(path: FilePath): (FilePath, Option[String]) =
        (dropExtension(path), getExtension(path))

    /// Suffixes the extension `ext` even if the path already has an extension.
    /// e.g. addExtension("archive.tar", "gz") == "archive.tar.gz"
    pub def addExtension(path: FilePath, ext: String): FilePath =
        if (String.isEmpty(ext))
            path
        else {
            let str = toString(path);
            new(str + "." + ext)
        }

    pub def getExtensions(path: FilePath): Option[String] =
        use Option.flatMap;
        let* s = getFileName(path) |> Option.map(toString);
        let* ix = String.indexOfLeft(".", s);
        Some(String.sliceRight(s, ix+1))

    ///
    /// Drop all extensions if the filepath has mutliple extensions,
    /// i.e. "archive.tar.gz" becomes "archive".
    ///
    pub def dropExtensions(path: FilePath): FilePath = match getExtensions(path) {
            case Some(ext) => {
                let str = toString(path);
                let lhs = String.dropRight(String.length(ext) + 1, str);
                new(lhs)
            }
            case None => path
        }

    pub def putExtensions(path: FilePath, ext: String): FilePath =
        match getExtensions(path) {
            case Some(ext0) => {
                let str = toString(path);
                let lhs = String.dropRight(String.length(ext0), str);
                new(lhs + ext)
            }
            case None => path
        }

    pub def splitExtensions(path: FilePath): (FilePath, Option[String]) =
        (dropExtensions(path), getExtensions(path))

    pub def stripExtension(path: FilePath, ext: String ): Option[FilePath] =
        use Option.flatMap;
        let* ext1 = getExtension(path);
        if (ext1 == ext)
            Some(dropExtension(path))
        else
            None

    pub def stripExtensions(path: FilePath, exts: String ): Option[FilePath] =
        use Option.flatMap;
        let* exts1 = getExtensions(path);
        if (exts1 == exts)
            Some(dropExtensions(path))
        else
            None

 }