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

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.input.BOMInputStream;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * A wrapper class for File related functions.
 * This class that provides concrete, non-overloaded methods.
 */
public class FileWrapper {


    /// Wrapper function for `Files.exists` as it is "varargs" in Java.
    public static boolean exists(Path path) throws Exception {
        return Files.exists(path, LinkOption.NOFOLLOW_LINKS);
    }

    /// Wrapper function for `Files.copy` as it is "varargs" in Java.
    public static Path copy(Path source, Path target) throws Exception {
        return Files.copy(source, target, LinkOption.NOFOLLOW_LINKS, StandardCopyOption.REPLACE_EXISTING);
    }

    /// Wrapper function for `Files.writeString` as it is "varargs" in Java and uses the CharSequence interface.
    public static void writeString(Path path, Charset charset, String content) throws Exception {
        final CharSequence content2 = content;
        Files.writeString(path, content2, charset);
    }

    /// Reads the File with the Charset identified by the BOM
    public static String readFileWithBOM(Path path) throws Exception {
        File file = path.toFile();
        FileInputStream instream = new FileInputStream(file);
        BOMInputStream bomInstream = new BOMInputStream(instream, false);
        String csname = bomInstream.getBOMCharsetName();
        if (csname == null) csname = "UTF-16";
        Charset charset = Charset.forName(csname);
        Reader reader = new InputStreamReader(bomInstream, charset);
        String answer = IOUtils.toString(reader);
        reader.close();
        return answer;
    }

}
