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

namespace TestChain {


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        let xs: Chain[Int32] = Chain.empty();
        Chain.toList(xs) == Nil

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
// flatten                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def flatten01(): Bool =
    let xs: Chain[Chain[Int32]] = Chain.flatten(Chain.empty());
    Chain.toList(xs) == Nil

@test
def flatten02(): Bool =
    let xs = Chain.flatten(Chain.singleton(Chain.singleton(1)));
    Chain.toList(xs) == 1 :: Nil

@test
def flatten03(): Bool =
    let xs = Chain.flatten(Chain.fromList(Chain.singleton(1) :: Chain.singleton(2) :: Nil));
    Chain.toList(xs) == 1 :: 2 :: Nil

@test
def flatten04(): Bool =
    let xs = Chain.flatten(Chain.fromList(Chain.fromList(1 :: 2 :: Nil) :: Chain.singleton(3) :: Nil));
    Chain.toList(xs) == 1 :: 2 :: 3 :: Nil

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