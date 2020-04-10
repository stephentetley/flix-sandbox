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

package flix.runtime.library.system;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * A wrapper class for java.nio.file.Path related functions.
 * java.nio.file.Path is an interface which is taxing for Flix's
 * current FFI.
 * This class that provides concrete, non-overloaded methods.
 */
public class FilePathWrapper {

    /// This is a "vargs" in Java
    public static Path get1(String path) {
        return Paths.get(path);
    }

    // Stub function for `toString` as it is an interface method.
    public static String toString(Path path) {
        return path.toString();
    }

    // Stub function for `normalize` as it is an interface method.
    public static Path normalize(Path path) {
        return path.normalize();
    }

    // Stub function for `toAbsolutePath` as it is an interface method.
    public static Path toAbsolutePath(Path path) {
        return path.toAbsolutePath();
    }

    // Stub function for `isAbsolute` as it is an interface method.
    public static boolean isAbsolute(Path path) {
        return path.isAbsolute();
    }

    // Stub function for `getRoot` as it is an interface method.
    // Note - may return null.
    public static Path getRoot(Path path) {
        return path.getRoot();
    }

    // Stub function for `getParent` as it is an interface method.
    // Note - may return null.
    public static Path getParent(Path path) {
        return path.getParent();
    }

    // Stub function for `getFileName` as it is an interface method.
    // Note - may return null.
    public static Path getFileName(Path path) {
        return path.getFileName();
    }

    // Stub function for `resolve` as it is an interface method.
    public static Path resolve(Path path, Path other) {
        return path.resolve(other);
    }

    // Stub function for `getNameCount` as it is an interface method.
    public static int getNameCount(Path path) {
        return path.getNameCount();
    }

    // Stub function for `getName` as it is an interface method.
    // Note - may throw an exception.
    public static Path getName(Path path, int index) throws Exception {
        return path.getName(index);
    }

}
