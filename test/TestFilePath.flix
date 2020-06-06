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

namespace TestNormalize {


/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def toString01(): Bool & Impure =
    let p1 = System/FilePath.new("src.flix");
    System/FilePath.toString(p1) == "src.flix"



/////////////////////////////////////////////////////////////////////////////
// normalize                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def normalize01(): Bool & Impure =
    let p1 = System/FilePath.new("c:/root/../root2") |> System/FilePath.normalize;
    let p2 = System/FilePath.new("c:/root2");
    System/FilePath.equals(p1, p2)

@test
def normalize02(): Bool & Impure =
    let p1 = System/FilePath.new("c:/root/child1/..") |> System/FilePath.normalize;
    let p2 = System/FilePath.new("c:/root/");
    System/FilePath.equals(p1, p2)

/////////////////////////////////////////////////////////////////////////////
// toAbsolutePath                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def toAbsolutePath01(): Bool & Impure =
    let p1 = System/FilePath.new("c:/root/child1.txt") |> System/FilePath.toAbsolutePath;
    let p2 = System/FilePath.new("c:/root/child1.txt");
    System/FilePath.equals(p1, p2)

}