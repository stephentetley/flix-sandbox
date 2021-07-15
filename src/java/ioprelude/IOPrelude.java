/*
 * Copyright 2021 Stephen Tetley
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

package flix.runtime.spt.sandbox.ioprelude;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * A wrapper class for Directory related functions.
 * This class that provides concrete, non-overloaded methods.
 */

public class IOPrelude {

    // Wrapper function for `exists` as it is a "varags" method
    // with FollowLinks == true.
    public static boolean fileExistsFollowLinks(Path path) throws Exception {
        return Files.exists(path);
    }

    // Wrapper function for `isDirectory` as it is a "varags" method
    // with FollowLinks == true.
    public static boolean isDirectoryFollowLinks(Path path) throws Exception {
        return Files.isDirectory(path);
    }

    /// Wrapper function for `Files.writeString` as it is "varargs" in Java and uses the CharSequence interface.
    public static void writeString(Path path, Charset charset, StandardOpenOption opt, String content) throws Exception {
        final CharSequence content2 = content;
        Files.writeString(path, content2, charset, opt);
    }

    public static void writeBytes(Path path, StandardOpenOption opt, byte[] bytes)  throws Exception {
        Files.write(path, bytes, opt);
    }

    public static InputStream openFileInputStream(Path path) throws Exception {
        return Files.newInputStream(path);
    }

    public static OutputStream openFileOutputStream(Path path, StandardOpenOption opt) throws Exception {
        return Files.newOutputStream(path, opt);
    }

    public static void outputStreamWriteByte(OutputStream strm, byte i) throws Exception {
        byte[] arr1 = {i};
        strm.write(arr1);
    }


}
