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

package flixspt.sandbox;

import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;

/**
 * A wrapper class for java.nio.file.Path, java.nio.file.Paths and related functions.
 * `Paths.get` is a varargs function which causes problems if we pass an array from from Flix.
 * This class that provides concrete, non-overloaded methods.
 */
public class FilePathWrapper {

    /// Wrapper function for `Paths.get` as it is "varargs" in Java.
    public static Path get1(String path) {
        return Paths.get(path);
    }

    /// Wrapper function for `getPathMatcher` / `matches`.
    public static boolean globMatches(Path path, String glob) throws Exception {
        FileSystem fs = FileSystems.getDefault();
        PathMatcher m1 = fs.getPathMatcher("glob:" + glob);
        return m1.matches(path);
    }

    /// Wrapper function for `getPathMatcher` / `matches`.
    public static boolean regexMatches(Path path, String pattern) throws Exception {
        FileSystem fs = FileSystems.getDefault();
        PathMatcher m1 = fs.getPathMatcher("regex:" + pattern);
        return m1.matches(path);
    }

}
