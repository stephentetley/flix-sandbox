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

/// BOM has extra Jar dependencies Apache Commons `commons-io`
/// so it should provide extra functionality for existing modules / types
/// (which shouldn't depend it it).

namespace System/File/BOM {

    use System.FilePath;
    use System.FilePath.{FilePath};
    use System/File.BufferedReader;
    use System/File.BufferedReader.{BufferedReader};
    use Text.Charset.{Charset};


    /// Reads the File with the Charset identified by the BOM
    pub def readFileWithBOM(path: FilePath): Result[String, String] & Impure =
        import static flixspt.sandbox.BOMWrapper.readFileWithBOM(##java.nio.file.Path): String & Impure;
        try {
            let FilePath(path1) = path;
            Ok(readFileWithBOM(path1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readFileWithBOM")
        }

    pub def openBufferedReaderWithBOM(path: System.FilePath): Result[BufferedReader, String] & Impure =
        import static flixspt.sandbox.BOMWrapper.openBufferedReaderWithBOM(##java.nio.file.Path): ##java.io.BufferedReader & Impure;
        try {
            let FilePath(path1) = path;
            Ok(BufferedReader(openBufferedReaderWithBOM(path1)))
        }
        catch {
            case _: ##java.lang.Exception => Err("BufferedReader:newUsingBOM")
        }

}
