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

///
/// Represents a FilePath.
///
opaque type FilePath = ##java.nio.file.Path

namespace FilePath {

    ///
    /// Returns a new FilePath.
    ///
    pub def new(s: String): FilePath & Impure =
        import flix.runtime.library.SystemPrimitives:pathOf1(String);
        FilePath(pathOf1(s))

    pub def toString(path: FilePath): String & Impure =
        import flix.runtime.library.SystemPrimitives:pathToString(##java.nio.file.Path);
        let FilePath(path1) = path;
        path1.pathToString()

    pub def normalize(path: FilePath): FilePath & Impure =
        import flix.runtime.library.SystemPrimitives:pathNormalize(##java.nio.file.Path);
        let FilePath(path1) = path;
        FilePath(path1.pathNormalize())

    pub def isAbsolute(path: FilePath): Bool & Impure =
        import flix.runtime.library.SystemPrimitives:pathIsAbsolute(##java.nio.file.Path);
        let FilePath(path1) = path;
        path1.pathIsAbsolute()

    pub def isRelative(path: FilePath): Bool & Impure = !(isAbsolute(path))

    pub def getRoot(path: FilePath): Option[FilePath] & Impure =
        import flix.runtime.library.SystemPrimitives:pathGetRoot(##java.nio.file.Path);
        let FilePath(path1) = path;
        let root = path1.pathGetRoot();
        if (isNull(root)) None else Some(FilePath(root))

    pub def getParent(path: FilePath): Option[FilePath] & Impure =
        import flix.runtime.library.SystemPrimitives:pathGetParent(##java.nio.file.Path);
        let FilePath(path1) = path;
        let parent = path1.pathGetParent();
        if (isNull(parent)) None else Some(FilePath(parent))

    pub def getFileName(path: FilePath): Option[FilePath] & Impure =
        import flix.runtime.library.SystemPrimitives:pathGetFileName(##java.nio.file.Path);
        let FilePath(path1) = path;
        let name = path1.pathGetFileName();
        if (isNull(name)) None else Some(FilePath(name))

    pub def append(path: FilePath, other: FilePath): FilePath & Impure =
        import flix.runtime.library.SystemPrimitives:pathResolve(##java.nio.file.Path, ##java.nio.file.Path);
        let FilePath(path1) = path;
        let FilePath(other1) = other;
        FilePath(pathResolve(path1, other1))

    pub def getNameCount(path: FilePath): Int32 & Impure =
        import flix.runtime.library.SystemPrimitives:pathGetNameCount(##java.nio.file.Path);
        let FilePath(path1) = path;
        path1.pathGetNameCount()

    pub def getName(index: Int32, path: FilePath): Option[FilePath] & Impure =
        try {
            import flix.runtime.library.SystemPrimitives:pathGetName(##java.nio.file.Path, Int32);
            let FilePath(path1) = path;
            path1.pathGetName(index) |> FilePath |> Some
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def getExtension(path: FilePath): Option[String] & Impure =
        match getFileName(path) |> Option.map(toString) {
            case None => None
            case Some(s) => match String.indexOfRight(s, ".") {
                case None => None
                case Some(ix) => Some(String.slice(ix+1, String.length(s), s))
            }
        }


 }