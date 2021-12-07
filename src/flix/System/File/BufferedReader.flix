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

namespace System/File {

    ///
    /// Represents a FilePath.
    ///
    pub opaque type BufferedReader = ##java.io.BufferedReader

}

namespace System/File/BufferedReader {

    use X.Cursor;
    use X.Cursor.{Cursor};
    use System/File.BufferedReader;
    use System/File.BufferedReader.{BufferedReader};
    use System.FilePath;
    use System.FilePath.{FilePath};
    use Text.Charset.{Charset};

    pub def new(path: System.FilePath, cs: Text.Charset): Result[BufferedReader, String] & Impure =
        import static java.nio.file.Files.newBufferedReader(##java.nio.file.Path, ##java.nio.charset.Charset);
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
        import java.io.BufferedReader.read();
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
        import java.io.BufferedReader.readLine();
        try {
            let BufferedReader(x1) = x;
            Ok(readLine(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readLine")
        }

    pub def close(x: BufferedReader): Result[Unit, String] & Impure =
        import java.io.Reader.close();
        try {
            let BufferedReader(x1) = x;
            Ok(close(x1 as ##java.io.Reader))
        }
        catch {
            case _: ##java.lang.Exception => Err("close")
        }

    ///
    /// Closes the BufferedReader when done.
    ///
    pub def lines(x: BufferedReader): Cursor[String, String] & Impure =
        import static flix.runtime.spt.sandbox.system.LineIterator.newBufferedReaderlinesIterator(##java.io.BufferedReader);
        import flix.runtime.spt.sandbox.system.LineIterator.hasNext();
        import flix.runtime.spt.sandbox.system.LineIterator.next();
        let BufferedReader(x1) = x;
        let primIter = newBufferedReaderlinesIterator(x1);
        let iterNext = () -> {
            try {
                let s1 = next(primIter);
                Ok(s1)
            } catch {
                case _: ##java.lang.Exception => Err("lines[next]")
            }
        };
        let cursorAlg: X.CursorAlg[String, String] =  {
              hasNext = () -> hasNext(primIter)
            , next = iterNext
            , finalize = () -> () as & Impure
        };
        X/Cursor.makeCursor(cursorAlg)

}
