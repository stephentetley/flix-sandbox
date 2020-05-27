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

    ///
    /// Returns a new FilePath.
    ///
    pub def new(s: String): FilePath & Impure =
        import java.nio.file.Paths:get(String, Array[String]) as get1;
        FilePath(get1(s, []))

    pub def toString(path: FilePath): String & Impure =
        import java.nio.file.Path.toString();
        let FilePath(path1) = path;
        path1.toString()

    pub def normalize(path: FilePath): FilePath & Impure =
        import java.nio.file.Path.normalize();
        let FilePath(p1) = path;
        FilePath(p1.normalize())

    pub def toAbsolutePath(path: FilePath): FilePath & Impure =
        import java.nio.file.Path.toAbsolutePath();
        let FilePath(p1) = path;
        FilePath(p1.toAbsolutePath())

    pub def isAbsolute(path: FilePath): Bool & Impure =
        import java.nio.file.Path.isAbsolute();
        let FilePath(path1) = path;
        path1.isAbsolute()

    pub def isRelative(path: FilePath): Bool & Impure = !(isAbsolute(path))

    pub def getRoot(path: FilePath): Option[FilePath] & Impure =
        import java.nio.file.Path.getRoot();
        let FilePath(path1) = path;
        let root = path1.getRoot();
        if (Object.isNull(root))
            None
        else
            Some(FilePath(root))

    pub def getParent(path: FilePath): Option[FilePath] & Impure =
        import java.nio.file.Path.getParent();
        let FilePath(path1) = path;
        let parent = path1.getParent();
        if (Object.isNull(parent))
            None
        else
            Some(FilePath(parent))

    pub def getFileName(path: FilePath): Option[FilePath] & Impure =
        import java.nio.file.Path.getFileName();
        let FilePath(path1) = path;
        let name = path1.getFileName();
        if (Object.isNull(name))
            None
        else
            Some(FilePath(name))

    pub def append(path: FilePath, other: FilePath): FilePath & Impure =
        import java.nio.file.Path.resolve(##java.nio.file.Path);
        let FilePath(path1) = path;
        let FilePath(other1) = other;
        FilePath(resolve(path1, other1)) |> normalize

    pub def getNameCount(path: FilePath): Int32 & Impure =
        import java.nio.file.Path.getNameCount();
        let FilePath(path1) = path;
        path1.getNameCount()

    pub def getNameAt(ix: Int32, path: FilePath): Option[FilePath] & Impure =
        try {
            import java.nio.file.Path.getName(Int32);
            let FilePath(path1) = path;
            path1.getName(ix) |> FilePath |> Some
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def getExtension(path: FilePath): Option[String] & Impure =
        match getFileName(path) |> Option.map(toString) {
            case None => None
            case Some(s) => match String.indexOfRight(s, ".") {
                case None => None
                case Some(ix) => Some(String.sliceRight(s, ix+1))
            }
        }


 }