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

namespace TestChain {

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def empty01(): Bool =
    use Chain.{toList, empty};
    let xs = empty();
    toList(xs) == Nil

/////////////////////////////////////////////////////////////////////////////
// singleton                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def singleton01(): Bool =
    use Chain.{toList, singleton};
    let xs = singleton(1);
    toList(xs) == 1 :: Nil

@test
def singleton02(): Bool =
    use Chain.{toList, singleton};
    let xs: Chain[Option[Int32]] = singleton(None);
    toList(xs) == None :: Nil

/////////////////////////////////////////////////////////////////////////////
// head                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def head01(): Bool = Chain.head(Chain.fromList(Nil)) == None

@test
def head02(): Bool = Chain.head(Chain.fromList(1 :: Nil)) == Some(1)

@test
def head03(): Bool = Chain.head(Chain.fromList(2 :: 1 :: Nil)) == Some(2)

@test
def head04(): Bool = Chain.head(Chain.fromList(3 :: 2 :: 1 :: Nil)) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// last                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def last01(): Bool = Chain.last(Chain.empty()) == None

@test
def last02(): Bool = Chain.last(Chain.fromList(1 :: Nil)) == Some(1)

@test
def last03(): Bool = Chain.last(Chain.fromList(1 :: 2 :: Nil)) == Some(2)

@test
def last04(): Bool = Chain.last(Chain.fromList(1 :: 2 :: 3 :: Nil)) == Some(3)

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def length01(): Bool = Chain.length(Chain.empty()) == 0

@test
def length02(): Bool = Chain.length(Chain.fromList(1 :: Nil)) == 1

@test
def length03(): Bool = Chain.length(Chain.fromList(1 :: 2 :: Nil)) == 2

@test
def length04(): Bool = Chain.length(Chain.fromList(1 :: 2 :: 3 :: Nil)) == 3

/////////////////////////////////////////////////////////////////////////////
// append                                                                  //
/////////////////////////////////////////////////////////////////////////////
@test
def append01(): Bool = Chain.toList(Chain.append(Chain.empty(), Chain.empty())) == Nil

@test
def append02(): Bool = Chain.toList(Chain.append(Chain.empty(), Chain.singleton(1))) == 1 :: Nil

@test
def append03(): Bool =
    Chain.toList(Chain.append(Chain.empty(), Chain.fromList(1 :: 2 :: Nil))) == 1 :: 2 :: Nil

@test
def append04(): Bool = Chain.toList(Chain.append(Chain.singleton(1), Chain.empty()))== 1 :: Nil

@test
def append05(): Bool =
    Chain.toList(Chain.append(Chain.fromList(1 :: 2 :: Nil), Chain.empty())) == 1 :: 2 :: Nil

@test
def append06(): Bool =
    Chain.toList(Chain.append(Chain.singleton(1), Chain.singleton(2))) == 1 :: 2 :: Nil

@test
def append07(): Bool =
    Chain.toList(Chain.append(Chain.fromList(1 :: 2 :: Nil), Chain.fromList(3 :: Nil))) == 1 :: 2 :: 3 :: Nil

@test
def append08(): Bool =
    Chain.toList(Chain.append(Chain.fromList(1 :: Nil), Chain.fromList(2 :: 3 :: Nil))) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// memberOf                                                                //
/////////////////////////////////////////////////////////////////////////////
@test
def memberOf01(): Bool = Chain.memberOf(0, Chain.empty()) == false

@test
def memberOf02(): Bool = Chain.memberOf(0, Chain.fromList(1 :: Nil)) == false

@test
def memberOf03(): Bool = Chain.memberOf(0, Chain.fromList(0 :: Nil)) == true

@test
def memberOf04(): Bool = Chain.memberOf(0, Chain.fromList(1 :: 2 :: Nil)) == false

@test
def memberOf05(): Bool = Chain.memberOf(1, Chain.fromList(1 :: 2 :: Nil)) == true

@test
def memberOf06(): Bool = Chain.memberOf(2, Chain.fromList(1 :: 2 :: Nil)) == true

@test
def memberOf07(): Bool = Chain.memberOf(3, Chain.fromList(1 :: 2 :: Nil)) == false

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def concat01(): Bool =
    use Chain.{toList, concat};
    let xs = concat(Nil);
    toList(xs) == Nil

@test
def concat02(): Bool =
    use Chain.{toList, concat, singleton};
    let xs = concat(singleton(1) :: Nil);
    toList(xs) == 1 :: Nil

@test
def concat03(): Bool =
    use Chain.{toList, concat, singleton};
    let xs = concat(singleton(1) :: singleton(2) :: Nil);
    toList(xs) == 1 :: 2 :: Nil

@test
def concat04(): Bool =
    use Chain.{toList, concat, singleton, fromList};
    let xs = concat(fromList(1 :: 2 :: Nil) :: singleton(3) :: Nil);
    toList(xs) == 1 :: 2 :: 3 :: Nil

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight01(): Bool =
    use Chain.{repeat, foldRight};
    let xs = repeat(1, 1000000);
    foldRight((a,b) -> Int32.max(a,b), 0, xs) == 1

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft01(): Bool =
    use Chain.{repeat, foldLeft};
    let xs = repeat(1, 1000000);
    foldLeft((a,b) -> Int32.max(a,b), 0, xs) == 1

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def map01(): Bool =
    use Chain.{toList, map, empty};
    let xs = map(x -> x+1, empty(): Chain[Int32]);
    toList(xs) == Nil

@test
def map02(): Bool =
    use Chain.{toList, map, singleton};
    let xs = map(x -> x+1, singleton(0));
    toList(xs) == 1 :: Nil

@test
def map03(): Bool =
    use Chain.{toList, map, cons, singleton};
    let xs = map(x -> x+1, cons(1, singleton(0)));
    toList(xs) == 2 :: 1 :: Nil

}