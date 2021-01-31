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

package flix.runtime.spt.sandbox.system;

import java.nio.file.Path;
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



}
