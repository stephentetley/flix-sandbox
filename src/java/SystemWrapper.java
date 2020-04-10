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
 * A wrapper class for System related functions that provides concrete, non-overloaded methods.
 * This is currently a "ragbag" and will be removed at some point.
 */
public class SystemWrapper {


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
