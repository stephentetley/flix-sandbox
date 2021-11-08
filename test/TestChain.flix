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
    // point                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def point01(): Bool =
        let xs = Chain.point(1);
        Chain.toList(xs) == 1 :: Nil

    @test
    def point02(): Bool =
        let xs: Chain[Option[Int32]] = Chain.point(None);
        Chain.toList(xs) == None :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Chain.ap(Chain.empty(), Chain.empty()) == Chain.empty() : Chain[Int32]

    @test
    def ap02(): Bool = Chain.ap(Chain.point(x -> x+1), Chain.empty()) == Chain.empty()

    @test
    def ap03(): Bool = Chain.ap(Chain.empty(), Chain.point(5)) == Chain.empty() : Chain[Int32]

    @test
    def ap04(): Bool = Chain.ap(Chain.point(x -> x+1), Chain.point(5)) == Chain.point(6)

    @test
    def ap05(): Bool = Chain.ap(Chain.point(x -> x+1), Chain.fromList(0 :: 5 :: Nil)) == Chain.fromList(1 :: 6 :: Nil)

    @test
    def ap06(): Bool = Chain.ap(Chain.fromList((x -> x+1) :: (x -> x*2) :: Nil), Chain.fromList(0 :: 4 :: Nil)) == Chain.fromList(1 :: 5 :: 0 :: 8 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = Chain.isEmpty(Chain.empty()) == true

    @test
    def isEmpty02(): Bool = Chain.isEmpty(Chain.point(1)) == false

    @test
    def isEmpty03(): Bool = Chain.isEmpty(Chain.fromList(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = Chain.cons(1, Chain.empty()) == Chain.point(1)

    @test
    def cons02(): Bool = Chain.cons(1, Chain.point(2)) == Chain.fromList(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def snoc01(): Bool = Chain.snoc(Chain.empty(), 1) == Chain.point(1)

    @test
    def snoc02(): Bool = Chain.snoc(Chain.point(1), 2) == Chain.fromList(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = Chain.head(Chain.empty(): Chain[Unit]) == None

    @test
    def head02(): Bool = Chain.head(Chain.point(1)) == Some(1)

    @test
    def head03(): Bool = Chain.head(Chain.fromList(2 :: 1 :: Nil)) == Some(2)

    @test
    def head04(): Bool = Chain.head(Chain.fromList(3 :: 2 :: 1 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = Chain.last(Chain.empty(): Chain[Unit]) == None

    @test
    def last02(): Bool = Chain.last(Chain.point(1)) == Some(1)

    @test
    def last03(): Bool = Chain.last(Chain.fromList(1 :: 2 :: Nil)) == Some(2)

    @test
    def last04(): Bool = Chain.last(Chain.fromList(1 :: 2 :: 3 :: Nil)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = Chain.init(Chain.empty(): Chain[Unit]) == None

    @test
    def init02(): Bool = Chain.init(Chain.point(0)) == Some(Chain.empty())

    @test
    def init03(): Bool = Chain.init(Chain.fromList(0 :: 1 :: Nil)) == Some(Chain.point(0))

    @test
    def init04(): Bool = Chain.init(Chain.fromList(2 :: 1 :: 0 :: Nil)) == Some(Chain.fromList(2 :: 1 :: Nil))

    @test
    def init05(): Bool = Chain.init(Chain.fromList('a' :: 'b' :: 'c' :: 'd' :: Nil)) == Some(Chain.fromList('a' :: 'b' :: 'c' :: Nil))

    @test
    def init06(): Bool = Chain.init(Chain.fromList("test1" :: "test2" :: "test3" :: Nil)) == Some(Chain.fromList("test1" :: "test2" :: Nil))

    @test
    def init07(): Bool = Chain.init(Chain.fromList(2 :: 1 :: 0 :: -1 :: Nil)) == Some(Chain.fromList(2 :: 1 :: 0 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = Chain.length(Chain.empty()) == 0

    @test
    def length02(): Bool = Chain.length(Chain.point(1)) == 1

    @test
    def length03(): Bool = Chain.length(Chain.fromList(1 :: 2 :: Nil)) == 2

    @test
    def length04(): Bool = Chain.length(Chain.fromList(1 :: 2 :: 3 :: Nil)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Chain.append(Chain.empty(): Chain[Unit], Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def append02(): Bool = Chain.append(Chain.empty(), Chain.point(1)) == Chain.point(1)

    @test
    def append03(): Bool = Chain.append(Chain.empty(), Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def append04(): Bool = Chain.append(Chain.point(1), Chain.empty()) == Chain.point(1)

    @test
    def append05(): Bool = Chain.append(Chain.fromList(1 :: 2 :: Nil), Chain.empty()) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def append06(): Bool = Chain.append(Chain.point(1), Chain.point(2)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def append07(): Bool = Chain.append(Chain.fromList(1 :: 2 :: Nil), Chain.point(3)) == Chain.fromList(1 :: 2 :: 3 :: Nil)

    @test
    def append08(): Bool = Chain.append(Chain.point(1), Chain.fromList(2 :: 3 :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewLeft01(): Bool = Chain.viewLeft(Chain.empty()) == Chain/ViewLeft.NoneLeft

    @test
    def viewLeft02(): Bool = Chain.viewLeft(Chain.point(1)) == Chain/ViewLeft.SomeLeft(1, Chain.empty())

    @test
    def viewLeft03(): Bool = Chain.viewLeft(Chain.fromList(1 :: 2 :: Nil)) == Chain/ViewLeft.SomeLeft(1, Chain.point(2))

    @test
    def viewLeft04(): Bool = Chain.viewLeft(Chain.fromList(1 :: 2 :: 3 :: Nil)) == Chain/ViewLeft.SomeLeft(1, Chain.fromList(2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewRight01(): Bool = Chain.viewRight(Chain.empty()) == Chain/ViewRight.NoneRight

    @test
    def viewRight02(): Bool = Chain.viewRight(Chain.point(1)) == Chain/ViewRight.SomeRight(Chain.empty(), 1)

    @test
    def viewRight03(): Bool = Chain.viewRight(Chain.fromList(1 :: 2 :: Nil)) == Chain/ViewRight.SomeRight(Chain.point(1), 2)

    @test
    def viewRight04(): Bool = Chain.viewRight(Chain.fromList(1 :: 2 :: 3 :: Nil)) == Chain/ViewRight.SomeRight(Chain.fromList(1 :: 2 :: Nil), 3)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = Chain.memberOf(0, Chain.empty()) == false

    @test
    def memberOf02(): Bool = Chain.memberOf(0, Chain.point(1)) == false

    @test
    def memberOf03(): Bool = Chain.memberOf(0, Chain.point(0)) == true

    @test
    def memberOf04(): Bool = Chain.memberOf(0, Chain.fromList(1 :: 2 :: Nil)) == false

    @test
    def memberOf05(): Bool = Chain.memberOf(1, Chain.fromList(1 :: 2 :: Nil)) == true

    @test
    def memberOf06(): Bool = Chain.memberOf(2, Chain.fromList(1 :: 2 :: Nil)) == true

    @test
    def memberOf07(): Bool = Chain.memberOf(3, Chain.fromList(1 :: 2 :: Nil)) == false

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = Chain.indexOf(0, Chain.empty()) == -1

    @test
    def indexOf02(): Bool = Chain.indexOf(0, Chain.point(1)) == -1

    @test
    def indexOf03(): Bool = Chain.indexOf(1, Chain.point(1)) == 0

    @test
    def indexOf04(): Bool = Chain.indexOf(0, Chain.fromList(1 :: 2 :: Nil)) == -1

    @test
    def indexOf05(): Bool = Chain.indexOf(1, Chain.fromList(1 :: 2 :: Nil)) == 0

    @test
    def indexOf06(): Bool = Chain.indexOf(2, Chain.fromList(1 :: 2 :: Nil)) == 1

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = Chain.find(i -> i > 2, Chain.empty()) == None

    @test
    def find02(): Bool = Chain.find(i -> i > 2, Chain.point(1)) == None

    @test
    def find03(): Bool = Chain.find(i -> i > 2, Chain.point(3)) == Some(3)

    @test
    def find04(): Bool = Chain.find(i -> i > 2, Chain.fromList(1 :: 2 :: Nil)) == None

    @test
    def find05(): Bool = Chain.find(i -> i > 2, Chain.fromList(6 :: -6 :: Nil)) == Some(6)

    @test
    def find06(): Bool = Chain.find(i -> i > 2, Chain.fromList(-6 :: 6 :: Nil)) == Some(6)

    @test
    def find07(): Bool = Chain.find(i -> i > 2, Chain.fromList(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = Chain.findLeft(i -> i > 2, Chain.empty()) == None

    @test
    def findLeft02(): Bool = Chain.findLeft(i -> i > 2, Chain.point(1)) == None

    @test
    def findLeft03(): Bool = Chain.findLeft(i -> i > 2, Chain.point(3)) == Some(3)

    @test
    def findLeft04(): Bool = Chain.findLeft(i -> i > 2, Chain.fromList(1 :: 2 :: Nil)) == None

    @test
    def findLeft05(): Bool = Chain.findLeft(i -> i > 2, Chain.fromList(6 :: -6 :: Nil)) == Some(6)

    @test
    def findLeft06(): Bool = Chain.findLeft(i -> i > 2, Chain.fromList(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findLeft07(): Bool = Chain.findLeft(i -> i > 2, Chain.fromList(6 :: 7 :: Nil)) == Some(6)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = Chain.findRight(i -> i > 2, Chain.empty()) == None

    @test
    def findRight02(): Bool = Chain.findRight(i -> i > 2, Chain.point(1)) == None

    @test
    def findRight03(): Bool = Chain.findRight(i -> i > 2, Chain.point(3)) == Some(3)

    @test
    def findRight04(): Bool = Chain.findRight(i -> i > 2, Chain.fromList(1 :: 2 :: Nil)) == None

    @test
    def findRight05(): Bool = Chain.findRight(i -> i > 2, Chain.fromList(6 :: -6 :: Nil)) == Some(6)

    @test
    def findRight06(): Bool = Chain.findRight(i -> i > 2, Chain.fromList(-6 :: 6 :: Nil)) == Some(6)

    @test
    def findRight07(): Bool = Chain.findRight(i -> i > 2, Chain.fromList(6 :: 7 :: Nil)) == Some(7)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = Chain.range(1, 0) == Chain.empty()

    @test
    def range02(): Bool = Chain.range(1, 1) == Chain.empty()

    @test
    def range03(): Bool = Chain.range(1, 2) == Chain.point(1)

    @test
    def range04(): Bool = Chain.range(1, 3) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def range05(): Bool = Chain.range(1, 4) == Chain.fromList(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = Chain.repeat(-1, 1) == Chain.empty()
    
    @test
    def repeat02(): Bool = Chain.repeat(0, 1) == Chain.empty()
    
    @test
    def repeat03(): Bool = Chain.repeat(1, 1) == Chain.point(1)
    
    @test
    def repeat04(): Bool = Chain.repeat(2, 1) == Chain.fromList(1 :: 1 :: Nil)
    
    @test
    def repeat05(): Bool = Chain.repeat(3, 1) == Chain.fromList(1 :: 1 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        let xs: Chain[Chain[Int32]] = Chain.flatten(Chain.empty());
        Chain.toList(xs) == Nil

    @test
    def flatten02(): Bool =
        let xs = Chain.flatten(Chain.point(Chain.point(1)));
        Chain.toList(xs) == 1 :: Nil

    @test
    def flatten03(): Bool =
        let xs = Chain.flatten(Chain.fromList(Chain.point(1) :: Chain.point(2) :: Nil));
        Chain.toList(xs) == 1 :: 2 :: Nil

    @test
    def flatten04(): Bool =
        let xs = Chain.flatten(Chain.fromList(Chain.fromList(1 :: 2 :: Nil) :: Chain.point(3) :: Nil));
        Chain.toList(xs) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        use Chain.{repeat, foldRight};
        let xs = repeat(1000000, 1);
        foldRight((a,b) -> Int32.max(a,b), 0, xs) == 1

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        use Chain.{repeat, foldLeft};
        let xs = repeat(1000000, 1);
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
        use Chain.{toList, map, point};
        let xs = map(x -> x+1, point(0));
        toList(xs) == 1 :: Nil


    @test
    def map03(): Bool =
        use Chain.{toList, map, cons, point};
        let xs = map(x -> x+1, cons(1, point(0)));
        toList(xs) == 2 :: 1 :: Nil

}
