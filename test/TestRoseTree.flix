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
// flatten                                                                //
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


}