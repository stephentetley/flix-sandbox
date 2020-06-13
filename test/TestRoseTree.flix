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

use Data.RoseTree;
use Data.RoseTree.{RoseTree};

namespace TestRoseTree {


/////////////////////////////////////////////////////////////////////////////
// drawTree                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def drawTree01(): String =
    use Data/RoseTree.{drawTree};
    let t1 = RoseTree("a", Nil);
    drawTree(t1)

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def flatten01(): Bool =
    use Data/RoseTree.{flatten};
    let t1 = RoseTree("a1", Nil);
    flatten(t1) == "a1" :: Nil

@test
def flatten02(): Bool =
    use Data/RoseTree.{flatten};
    let t1 = RoseTree("a1", RoseTree("b2", Nil) :: Nil);
    flatten(t1) == "a1" :: "b2" :: Nil

@test
def flatten03(): Bool =
    use Data/RoseTree.{flatten};
    let t1 = RoseTree("a1", RoseTree("b2", Nil) :: RoseTree("c2", Nil) :: Nil);
    flatten(t1) == "a1" :: "b2" :: "c2" :: Nil

@test
def flatten04(): Bool =
    use Data/RoseTree.{flatten};
    let t1 = RoseTree("a1", RoseTree("b2", RoseTree("c3", Nil) :: Nil) :: RoseTree("d2", Nil) :: Nil);
    flatten(t1) == "a1" :: "b2" :: "c3" :: "d2" :: Nil

/////////////////////////////////////////////////////////////////////////////
// foldRoseTree                                                            //
/////////////////////////////////////////////////////////////////////////////

def sum(xs: List[Int32]): Int32 = List.foldRight((x,y) -> x + y, 0, xs)

@test
def foldRoseTree01(): Bool =
    use Data/RoseTree.{foldRoseTree};
    let t1 = RoseTree(1, Nil);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 1

@test
def foldRoseTree02(): Bool =
    use Data/RoseTree.{foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, Nil) :: Nil);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 3

@test
def foldRoseTree03(): Bool =
    use Data/RoseTree.{foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, Nil) :: RoseTree(3, Nil) :: Nil);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 6

@test
def foldRoseTree04(): Bool =
    use Data/RoseTree.{foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, RoseTree(3, Nil) :: Nil) :: RoseTree(4, Nil) :: Nil);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 10

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01(): Bool =
    use Data/RoseTree.{map, foldRoseTree};
    let t1 = RoseTree(1, Nil) |> map(x -> x + 1);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 2

@test
def map02(): Bool =
    use Data/RoseTree.{map, foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, Nil) :: Nil) |> map(x -> x + 1);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 5


@test
def map03(): Bool =
    use Data/RoseTree.{map, foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, Nil) :: RoseTree(3, Nil) :: Nil) |> map(x -> x + 1);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 9

@test
def map04(): Bool =
    use Data/RoseTree.{map, foldRoseTree};
    let t1 = RoseTree(1, RoseTree(2, RoseTree(3, Nil) :: Nil) :: RoseTree(4, Nil) :: Nil) |> map(x -> x + 1);
    foldRoseTree((x,xs) -> x + sum(xs), t1) == 14

}