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

package flix.runtime.library;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * A wrapper class for System related functions that provides concrete, non-overloaded methods.
 */
public class SystemWrapper {

    /// This is a "vargs" in Java
    public static Path pathsGet1(String path) {
        return Paths.get(path);
    }

    // Stub function for `toString` as it is an interface method.
    public static String pathToString(Path path) {
        return path.toString();
    }

    // Stub function for `normalize` as it is an interface method.
    public static Path pathNormalize(Path path) {
        return path.normalize();
    }

    // Stub function for `isAbsolute` as it is an interface method.
    public static boolean pathIsAbsolute(Path path) {
        return path.isAbsolute();
    }

    // Stub function for `getRoot` as it is an interface method.
    // Note - may return null.
    public static Path pathGetRoot(Path path) {
        return path.getRoot();
    }

    // Stub function for `getParent` as it is an interface method.
    // Note - may return null.
    public static Path pathGetParent(Path path) {
        return path.getParent();
    }

    // Stub function for `getFileName` as it is an interface method.
    // Note - may return null.
    public static Path pathGetFileName(Path path) {
        return path.getFileName();
    }

    // Stub function for `resolve` as it is an interface method.
    public static Path pathResolve(Path path, Path other) {
        return path.resolve(other);
    }

    // Stub function for `getNameCount` as it is an interface method.
    public static int pathGetNameCount(Path path) {
        return path.getNameCount();
    }

    // Stub function for `getNameCount` as it is an interface method.
    // Note - may throw an exception.
    public static Path pathGetName(Path path, int index) throws Exception {
        return path.getName(index);
    }

    public static Path currentDirectory() throws Exception {
        Path currentDir = Paths.get(".");
        return currentDir.normalize().toAbsolutePath();
    }

    public static void writeString(Path path, Charset charset, String content) throws Exception {
        final CharSequence content2 = content;
        Files.writeString(path, content2, charset);
        return;
    }



    public static String readStringUTF16le(Path path) throws Exception {
        try {
            Charset cs = Charset.forName("UTF-16LE");
            String s = Files.readString(path, cs);
            return s;
        } catch (IOException ex) {
            ex.printStackTrace();
            throw new IOException();
        }
    }
}
