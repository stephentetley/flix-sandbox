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

/// For reading binary files...

namespace FlixSandbox/System/File {

    ///
    /// Represents a FileInputStream.
    ///
    pub opaque type FileInputStream = ##java.io.FileInputStream

}

namespace FlixSandbox/System/File/FileInputStream {

    use FlixSandbox/System/File.FileInputStream;
    use FlixSandbox/System/File.FileInputStream.{FileInputStream};
    use FlixSandbox/System.FilePath.{FilePath};

    pub def fileInputStream(path: FlixSandbox/System.FilePath): Result[FileInputStream, String] & Impure =
        import new java.io.FileInputStream(##java.io.File): ##java.io.FileInputStream & Impure as newFileInputStream;
        import java.nio.file.Path.toFile(): ##java.io.File & Impure;
        try {
            let FilePath(path1) = path;
            let file1 = toFile(path1);
            Ok(FileInputStream(newFileInputStream(file1)))
        }
        catch {
            case _: ##java.lang.Exception => Err("FileInputStream:new")
        }


    ///
    /// Read a single byte as an Int32.
    /// Returns -1 if the end of the stream has been reached.
    ///
    pub def read(x: FileInputStream): Result[Int32, String] & Impure =
        import java.io.FileInputStream.read(): Int32 & Impure;
        try {
            let FileInputStream(x1) = x;
            Ok(read(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("read")
        }


    pub def close(x: FileInputStream): Result[Unit, String] & Impure =
        import java.io.FileInputStream.close(): Unit & Impure;
        try {
            let FileInputStream(x1) = x;
            Ok(close(x1))
        }
        catch {
            case _: ##java.lang.Exception => Err("close")
        }

}
