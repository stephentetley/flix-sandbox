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

package flix.runtime.spt.sandbox.system;


import org.apache.commons.io.input.BOMInputStream;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.nio.file.Path;

public class BufferedFileReaderWrapper {

    /// Reads the File with the Charset identified by the BOM
    public static BufferedReader createWithBOM(Path path) throws Exception {
        InputStream instream = new FileInputStream(path.toFile());
        BOMInputStream bomInstream = new BOMInputStream(instream);
        String csname = bomInstream.getBOMCharsetName();
        if (csname == null) csname = "UTF-16";
        Charset charset = Charset.forName(csname);
        BufferedReader br = new BufferedReader(new InputStreamReader(bomInstream, charset));
        return br;
    }

}
