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
public class BOMWrapper {


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
