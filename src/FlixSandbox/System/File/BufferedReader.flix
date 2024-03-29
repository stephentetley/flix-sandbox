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

/// For reading text files...

namespace FlixSandbox/System/File {

    ///
    /// Represents a FilePath.
    ///
    pub enum BufferedReader(##java.io.BufferedReader)

}

namespace FlixSandbox/System/File/BufferedReader {


    use FlixSandbox/System/File.BufferedReader;
    use FlixSandbox/System/File.BufferedReader.{BufferedReader};
    use Interop/System.FilePath;
    use Interop/System.FilePath.{FilePath};
    
    use Interop/Text.Charset;
    use Interop/Text.Charset.{Charset};

    pub def bufferedReader(path: FilePath, cs: Charset): Result[BufferedReader, String] & Impure =
        import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset): ##java.io.BufferedReader & Impure;
        try {
            let FilePath(path1) = path;
            let Charset(cs1) = cs;
            Ok(BufferedReader(newBufferedReader(path1, cs1)))
        }
        catch {
            case _: ##java.lang.Exception => Err("BufferedReader:new")
        }



    ///
    /// Read a single Char as an Int32.
    /// Returns -1 if the end of the stream has been reached.
    ///
    pub def read(x: BufferedReader): Result[Int32, String] & Impure =
        import java.io.BufferedReader.read(): Int32 & Impure;
        try {
            let BufferedReader(x1) = x;
            Ok(read(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("read")
        }

    pub def readChar(x: BufferedReader): Result[Char, String] & Impure =
        match read(x) {
            case Ok(i) if i >= 0 => Ok(Char.fromInt32(i))
            case _ => Err("readChar")
        }

    pub def readLine(x: BufferedReader): Result[String, String] & Impure =
        import java.io.BufferedReader.readLine(): String & Impure;
        try {
            let BufferedReader(x1) = x;
            Ok(readLine(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readLine")
        }

    pub def close(x: BufferedReader): Result[Unit, String] & Impure =
        import java.io.Reader.close(): Unit & Impure;
        try {
            let BufferedReader(x1) = x;
            Ok(close(x1 as ##java.io.Reader))
        }
        catch {
            case _: ##java.lang.Exception => Err("close")
        }


    ///
    /// Assumes the garbage collector closes the BufferedReader when done...
    ///
    pub def lines(x: BufferedReader): Iterator[String, Static] & Impure =
        import static flixspt.sandbox.LineIterator.newBufferedReaderlinesIterator(##java.io.BufferedReader): ##flixspt.sandbox.LineIterator & Impure;
        import flixspt.sandbox.LineIterator.hasNext(): Bool & Impure;
        import flixspt.sandbox.LineIterator.next(): String & Impure;
        let BufferedReader(x1) = x;
        let primIter = newBufferedReaderlinesIterator(x1);
        let done1 = () -> not hasNext(primIter);
        let next1 = () -> next(primIter);
        Iterator(done1, next1)


}
