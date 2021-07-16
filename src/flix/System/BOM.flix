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

/// Deprecated

namespace System/BOM {

    use System.FilePath;
    use System.FilePath.{FilePath};
    use Text.Charset.{Charset};


    /// Reads the File with the Charset identified by the BOM
    pub def readFileWithBOM(path: FilePath): Result[String, String] & Impure =
        import flix.runtime.spt.sandbox.system.BOMWrapper:readFileWithBOM(##java.nio.file.Path);
        try {
            let FilePath(path1) = path;
            Ok(readFileWithBOM(path1))
        }
        catch {
            case _: ##java.lang.Exception => Err("readFileWithBOM")
        }

}
