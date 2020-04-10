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

    pub def doesDirectoryExist(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:isDirectoryFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isDirectoryFollowLinks()

    pub def doesFileExist(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:isRegularFileFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.isRegularFileFollowLinks()

    pub def doesPathExist(path: FilePath): Bool & Impure =
        import flix.runtime.library.system.DirectoryWrapper:existsFollowLinks(##java.nio.file.Path);
        let FilePath(p1) = path;
        p1.existsFollowLinks()

}