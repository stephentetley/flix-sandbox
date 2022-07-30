/*
 * Copyright 2022 Stephen Tetley
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

/// Functions not covered by stdlib/File

namespace FlixSandbox/System/Extras {

    pub def globList(glob: {glob :: String}, f: String): Result[List[String], String] & Impure =
        globListWithIter(glob, f) |> Result.map(Iterator.toList)



    pub def globListWithIter(glob: {glob :: String}, f: String): Result[Iterator[String, Static], String] & Impure =
        try {
            import new java.io.File(String): ##java.io.File & Impure as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path & Impure;
            import static java.nio.file.Files.newDirectoryStream(##java.nio.file.Path, String): ##java.nio.file.DirectoryStream & Impure;
            import java.nio.file.DirectoryStream.iterator(): ##java.util.Iterator & Impure;
            import java.nio.file.Path.toString(): String & Pure as pathToString;

            let javaFile = newFile(f);
            let javaPath = toPath(javaFile);
            let javaStream = newDirectoryStream(javaPath, glob.glob);
            let javaIter = iterator(javaStream);
            let iter = Interop/Collections/RawIterator.new(Static, javaIter) 
                            |> Interop/Collections/RawIterator.iteratorWith(obj -> pathToString(obj as ##java.nio.file.Path));
            Ok(iter)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
        }


    pub def regexList(regex: {regex :: String}, f: String): Result[List[String], String] & Impure =
        regexListWithIter(regex, f) |> Result.map(Iterator.toList)

    pub def regexListWithIter(regex: {regex :: String}, f: String): Result[Iterator[String, Static], String] & Impure =
        try {
            import new java.io.File(String): ##java.io.File & Impure as newFile;
            import java.io.File.toPath(): ##java.nio.file.Path & Impure;
            import static java.nio.file.Files.newDirectoryStream(##java.nio.file.Path): ##java.nio.file.DirectoryStream & Impure;
            import java.nio.file.DirectoryStream.iterator(): ##java.util.Iterator & Impure;
            import java.nio.file.Path.toString(): String & Pure as pathToString;
            import static java.nio.file.FileSystems.getDefault(): ##java.nio.file.FileSystem & Impure;
            import java.nio.file.FileSystem.getPathMatcher(String): ##java.nio.file.PathMatcher & Impure;
            import java.nio.file.PathMatcher.matches(##java.nio.file.Path): Bool & Impure;
            let javaFile = newFile(f);
            let javaPath = toPath(javaFile) as & Pure;
            let javaStream = newDirectoryStream(javaPath);
            let javaIter = iterator(javaStream);
            let currentFS = getDefault();
            let matcher = getPathMatcher(currentFS, "regex:${regex.regex}");            
            let test = path -> matches(matcher, path) as & Pure;
            let iter = Interop/Collections/RawIterator.new(Static, javaIter) 
                            |> Interop/Collections/RawIterator.iteratorWith(obj -> obj as ##java.nio.file.Path);
            let iter1 = iter |> Iterator.filterL(test) |>  Iterator.mapL(pathToString);
            Ok(iter1)
        } catch {
            case ex: ##java.io.IOException =>
                import java.lang.Throwable.getMessage(): String & Impure;
                Err(getMessage(ex))
        }

}
