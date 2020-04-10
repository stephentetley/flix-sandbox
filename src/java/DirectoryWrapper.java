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

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * A wrapper class for Directory related functions.
 * This class that provides concrete, non-overloaded methods.
 */
public class DirectoryWrapper {

    // Stub function for `exists` as it is a "varags" method
    // with FollowLinks == true.
    public static boolean existsFollowLinks(Path path) throws Exception {
        return Files.exists(path);
    }

    // Stub function for `isDirectory` as it is a "varags" method
    // with FollowLinks == true.
    public static boolean isDirectoryFollowLinks(Path path) throws Exception {
        return Files.isDirectory(path);
    }

    // Stub function for `isRegularFile` as it is a "varags" method
    // with FollowLinks == true.
    public static boolean isRegularFileFollowLinks(Path path) throws Exception {
        return Files.isRegularFile(path);
    }

    // Stub function for `createDirectory` as it is a "varags" method
    // with no FileAttributes.
    public static void createDirectory(Path path) throws Exception {
        Files.createDirectory(path);
    }

    // Stub function for `createDirectories` as it is a "varags" method
    // with no FileAttributes.
    public static void createDirectories(Path path) throws Exception {
        Files.createDirectories(path);
    }

}
