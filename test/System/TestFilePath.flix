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

namespace System/TestFilePath {


/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool =
    let p1 = System/FilePath.new("src.flix");
    ToString.toString(p1) == "src.flix"



/////////////////////////////////////////////////////////////////////////////
// normalize                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def normalize01(): Bool =
    let p1 = System/FilePath.new("c:/root/../root2") |> System/FilePath.normalize;
    let p2 = System/FilePath.new("c:/root2");
    p1 == p2

@test
def normalize02(): Bool =
    let p1 = System/FilePath.new("c:/root/child1/..") |> System/FilePath.normalize;
    let p2 = System/FilePath.new("c:/root/");
    p1 == p2

/////////////////////////////////////////////////////////////////////////////
// toAbsolutePath                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def toAbsolutePath01(): Bool =
    let p1 = System/FilePath.new("c:/root/child1.txt") |> System/FilePath.toAbsolutePath;
    let p2 = System/FilePath.new("c:/root/child1.txt");
    p1 == Ok(p2)

/////////////////////////////////////////////////////////////////////////////
// getExtension                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def getExtension01(): Bool =
    use System/FilePath.{getExtension};
    let p1 = System/FilePath.new("c:/root/child1.txt");
    getExtension(p1) == Some("txt")

@test
def getExtension02(): Bool =
    use System/FilePath.{getExtension};
    let p1 = System/FilePath.new("file.txt");
    getExtension(p1) == Some("txt")

@test
def getExtension03(): Bool =
    use System/FilePath.{getExtension};
    let p1 = System/FilePath.new("c:/root");
    getExtension(p1) == None

@test
def getExtension04(): Bool =
    use System/FilePath.{getExtension};
    let p1 = System/FilePath.new("c:/root/file.001.txt");
    getExtension(p1) == Some("txt")

/////////////////////////////////////////////////////////////////////////////
// putExtension                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def putExtension01(): Bool =
    use System/FilePath.{putExtension, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let p2 = putExtension(p1, "md");
    getFileName(p2) |> Option.map(ToString.toString) == Some("file1.md")

/////////////////////////////////////////////////////////////////////////////
// dropExtension                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def dropExtension01(): Bool =
    use System/FilePath.{dropExtension, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let p2 = dropExtension(p1);
    getFileName(p2) |> Option.map(ToString.toString) == Some("file1")

/////////////////////////////////////////////////////////////////////////////
// addExtension                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def addExtension01(): Bool =
    use System/FilePath.{addExtension, getFileName};
    let p1 = System/FilePath.new("archive.tar");
    let p2 = addExtension(p1, "gz");
    getFileName(p2) |> Option.map(ToString.toString) == Some("archive.tar.gz")

/////////////////////////////////////////////////////////////////////////////
// dropExtensions                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def dropExtensions01(): Bool =
    use System/FilePath.{dropExtensions, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let p2 = dropExtensions(p1);
    getFileName(p2) |> Option.map(ToString.toString) == Some("file1")

@test
def dropExtensions02(): Bool =
    use System/FilePath.{dropExtensions, getFileName};
    let p1 = System/FilePath.new("c:/root/archive.tar.gz");
    let p2 = dropExtensions(p1);
    getFileName(p2) |> Option.map(ToString.toString) == Some("archive")

/////////////////////////////////////////////////////////////////////////////
// stripExtension                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def stripExtension01(): Bool =
    use System/FilePath.{stripExtension, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let ans = stripExtension(p1, "txt");
    ans |> Option.flatMap(getFileName) |> Option.map(ToString.toString) == Some("file1")

@test
def stripExtension02(): Bool =
    use System/FilePath.{stripExtension, getFileName};
    let p1 = System/FilePath.new("c:/root/archive.tar.gz");
    let ans = stripExtension(p1, "gz");
    ans |> Option.flatMap(getFileName) |> Option.map(ToString.toString) == Some("archive.tar")

@test
def stripExtension03(): Bool =
    use System/FilePath.{stripExtension, getFileName};
    let p1 = System/FilePath.new("c:/root/file1");
    let ans = stripExtension(p1, "txt");
    ans == None

/////////////////////////////////////////////////////////////////////////////
// stripExtensions                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def stripExtensions01(): Bool =
    use System/FilePath.{stripExtensions, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let ans = stripExtensions(p1, "txt");
    ans |> Option.flatMap(getFileName) |> Option.map(ToString.toString) == Some("file1")

@test
def stripExtensions02(): Bool =
    use System/FilePath.{stripExtensions, getFileName};
    let p1 = System/FilePath.new("c:/root/archive.tar.gz");
    let ans = stripExtensions(p1, "tar.gz");
    ans |> Option.flatMap(getFileName) |> Option.map(ToString.toString) == Some("archive")

@test
def stripExtensions03(): Bool =
    use System/FilePath.{stripExtensions, getFileName};
    let p1 = System/FilePath.new("c:/root/file1");
    let ans = stripExtensions(p1, "txt");
    ans == None

/////////////////////////////////////////////////////////////////////////////
// putBaseName                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def putBaseName01(): Bool =
    use System/FilePath.{putBaseName, getFileName};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let p2 = putBaseName(p1, "file2");
    p2 |> getFileName |> Option.map(ToString.toString) == Some("file2.txt")

/////////////////////////////////////////////////////////////////////////////
// split                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def split01(): Bool =
    use System/FilePath.{split};
    let p1 = System/FilePath.new("c:/root/file1.txt");
    let xs = split(p1);
    List.length(xs) == 3


@test
def isGlobMatch01(): Bool = 
    let p1 = System/FilePath.new("file1.txt");
    System/FilePath.isGlobMatch("*.txt", p1) == Ok(true)

}
