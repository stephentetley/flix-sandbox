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

namespace TestDList {

use Data.DList;

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def empty01(): Bool =
    use Data/DList.{toList, empty};
    let xs = empty(): DList[Int32];
    toList(xs) == Nil

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def singleton01(): Bool =
    use Data/DList.{toList, singleton};
    let xs = singleton(1);
    toList(xs) == 1 :: Nil

@test
def singleton02(): Bool =
    use Data/DList.{toList, singleton};
    let xs: DList[Option[Int32]] = singleton(None);
    toList(xs) == None :: Nil

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def concat01(): Bool =
    use Data/DList.{toList, concat};
    let xs = concat(Nil): DList[Int32];
    toList(xs) == Nil

@test
def concat02(): Bool =
    use Data/DList.{toList, concat, singleton};
    let xs = concat(singleton(1) :: Nil);
    toList(xs) == 1 :: Nil

@test
def concat03(): Bool =
    use Data/DList.{toList, concat, singleton};
    let xs = concat(singleton(1) :: singleton(2) :: Nil);
    toList(xs) == 1 :: 2 :: Nil

@test
def concat04(): Bool =
    use Data/DList.{toList, concat, singleton, fromList};
    let xs = concat(fromList(1 :: 2 :: Nil) :: singleton(3) :: Nil);
    toList(xs) == 1 :: 2 :: 3 :: Nil

}
