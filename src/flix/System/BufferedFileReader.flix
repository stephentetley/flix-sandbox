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

namespace System {

    ///
    /// Represents a FilePath.
    ///
    pub opaque type BufferedFileReader = ##java.io.BufferedReader


}

namespace System/BufferedFileReader {

    use System.BufferedFileReader;
    use System.BufferedFileReader.{BufferedFileReader};
    use System.FilePath.{FilePath};
    use Text.Charset.{Charset};

    pub def new(path: System.FilePath, cs: Text.Charset): Result[BufferedFileReader, String] & Impure =
        import java.nio.file.Files:newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset);
        try {
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(BufferedFileReader(newBufferedReader(path1, cs1)))
        }
        catch {
            case _: ##java.lang.Exception => Err("BufferedFileReader:new")
        }

    ///
    /// Read a single Char as an Int32.
    /// Returns -1 if the end of the stream has been reached.
    ///
    pub def read(x: BufferedFileReader): Result[Int32, String] & Impure =
        import java.io.BufferedReader.read();
        try {
            let BufferedFileReader(x1) = x;
            Ok(read(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("read")
        }

    pub def readLine(x: BufferedFileReader): Result[String, String] & Impure =
        import java.io.BufferedReader.readLine();
        try {
            let BufferedFileReader(x1) = x;
            Ok(readLine(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readLine")
        }

    pub def close(x: BufferedFileReader): Result[Unit, String] & Impure =
        import java.io.Reader.close();
        try {
            let BufferedFileReader(x1) = x;
            Ok(close(x1 as ##java.io.Reader))
        }
        catch {
            case _: ##java.lang.Exception => Err("close")
        }




}