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
    // scan                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def scan01(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.point(1)

    @test
    def scan02(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.point(false)) == Chain.fromList(1 :: 3 :: Nil)

    @test
    def scan03(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.point(true)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def scan04(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: false :: Nil)) == Chain.fromList(1 :: 3 :: 5 :: Nil)

    @test
    def scan05(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: true :: Nil)) == Chain.fromList(1 :: 3 :: 4 :: Nil)

    @test
    def scan06(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: false :: Nil)) == Chain.fromList(1 :: 2 :: 4 :: Nil)

    @test
    def scan07(): Bool = Chain.scan((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: true :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanLeft01(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.point(1)

    @test
    def scanLeft02(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.point(false)) == Chain.fromList(1 :: 3 :: Nil)

    @test
    def scanLeft03(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.point(true)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def scanLeft04(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: false :: Nil)) == Chain.fromList(1 :: 3 :: 5 :: Nil)

    @test
    def scanLeft05(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: true :: Nil)) == Chain.fromList(1 :: 3 :: 4 :: Nil)

    @test
    def scanLeft06(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: false :: Nil)) == Chain.fromList(1 :: 2 :: 4 :: Nil)

    @test
    def scanLeft07(): Bool = Chain.scanLeft((i, b) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: true :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // scanRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def scanRight01(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.empty()) == Chain.point(1)

    @test
    def scanRight02(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.point(false)) == Chain.fromList(3 :: 1 :: Nil)

    @test
    def scanRight03(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.point(true)) == Chain.fromList(2 :: 1 :: Nil)

    @test
    def scanRight04(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: false :: Nil)) == Chain.fromList(5 :: 3 :: 1 :: Nil)

    @test
    def scanRight05(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.fromList(false :: true :: Nil)) == Chain.fromList(4 :: 2 :: 1 :: Nil)

    @test
    def scanRight06(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: false :: Nil)) == Chain.fromList(4 :: 3 :: 1 :: Nil)

    @test
    def scanRight07(): Bool = Chain.scanRight((b, i) -> if (b) i+1 else i+2, 1, Chain.fromList(true :: true :: Nil)) == Chain.fromList(3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Chain.map(i -> i > 2, Chain.empty()) == Chain.empty()

    @test
    def map02(): Bool = Chain.map(i -> i > 2, Chain.point(1)) == Chain.point(false)

    @test
    def map03(): Bool = Chain.map(i -> i > 2, Chain.point(3)) == Chain.point(true)

    @test
    def map04(): Bool = Chain.map(i -> i > 2, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(false :: false :: Nil)

    @test
    def map05(): Bool = Chain.map(i -> i > 2, Chain.fromList(1 :: 8 :: Nil)) == Chain.fromList(false :: true :: Nil)

    @test
    def map06(): Bool = Chain.map(i -> i > 2, Chain.fromList(8 :: 1 :: Nil)) == Chain.fromList(true :: false :: Nil)

    @test
    def map07(): Bool = Chain.map(i -> i > 2, Chain.fromList(7 :: 8 :: Nil)) == Chain.fromList(true :: true :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.empty()) == Chain.empty()
    
    @test
    def mapWithIndex02(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.point(1)) == Chain.point(false)
    
    @test
    def mapWithIndex03(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.point(3)) == Chain.point(true)
    
    @test
    def mapWithIndex04(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(false :: true :: Nil)
    
    @test
    def mapWithIndex05(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.fromList(1 :: 8 :: Nil)) == Chain.fromList(false :: false :: Nil)
    
    @test
    def mapWithIndex06(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.fromList(8 :: 1 :: Nil)) == Chain.fromList(true :: true :: Nil)
    
    @test
    def mapWithIndex07(): Bool = Chain.mapWithIndex((e, i) -> if (i < 1) e > 2 else e <= 2, Chain.fromList(7 :: 8 :: Nil)) == Chain.fromList(true :: false :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    
    @test
    def flatMap01(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.empty()) == Chain.empty()
    
    @test
    def flatMap02(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.point(0)) == Chain.empty()
    
    @test
    def flatMap03(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.point(1)) == Chain.point(1)
    
    @test
    def flatMap04(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.point(2)) == Chain.fromList(2 :: 2 :: Nil)
    
    @test
    def flatMap05(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: 2 :: Nil)
    
    @test
    def flatMap06(): Bool = Chain.flatMap(i -> Chain.repeat(i, i), Chain.fromList(2 :: 3 :: Nil)) == Chain.fromList(2 :: 2 :: 3 :: 3 :: 3 :: Nil)
    
    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = Chain.reverse(Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def reverse02(): Bool = Chain.reverse(Chain.point(1)) == Chain.point(1)
    
    @test
    def reverse03(): Bool = Chain.reverse(Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(2 :: 1 :: Nil)
    
    @test
    def reverse04(): Bool = Chain.reverse(Chain.fromList(1 :: 1 :: Nil)) == Chain.fromList(1 :: 1 :: Nil)
    
    @test
    def reverse05(): Bool = Chain.reverse(Chain.fromList(1 :: 2 :: 3 :: Nil)) == Chain.fromList(3 :: 2 :: 1 :: Nil)
    
    @test
    def reverse06(): Bool = Chain.reverse(Chain.fromList(1 :: 2 :: 3 :: 4 :: Nil)) == Chain.fromList(4 :: 3 :: 2 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = Chain.intersperse(11, Chain.empty()) == Chain.empty()

    @test
    def intersperse02(): Bool = Chain.intersperse(11, Chain.point(1)) == Chain.point(1)

    @test
    def intersperse03(): Bool = Chain.intersperse(11, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 11 :: 2 :: Nil)

    @test
    def intersperse04(): Bool = Chain.intersperse(11, Chain.fromList(1 :: 2 :: 3 :: Nil)) == Chain.fromList(1 :: 11 :: 2 :: 11 :: 3 :: Nil)

    @test
    def intersperse05(): Bool = Chain.intersperse(11, Chain.fromList(1 :: 2 :: 3 :: 4 :: Nil)) == Chain.fromList(1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = Chain.count(i -> i > 3, Chain.empty()) == 0

    @test
    def count02(): Bool = Chain.count(i -> i > 3, Chain.point(1)) == 0

    @test
    def count03(): Bool = Chain.count(i -> i > 3, Chain.point(4)) == 1

    @test
    def count04(): Bool = Chain.count(i -> i > 3, Chain.fromList(1 :: 2 :: Nil)) == 0

    @test
    def count05(): Bool = Chain.count(i -> i > 3, Chain.fromList(1 :: 8 :: Nil)) == 1

    @test
    def count06(): Bool = Chain.count(i -> i > 3, Chain.fromList(8 :: 1 :: Nil)) == 1

    @test
    def count07(): Bool = Chain.count(i -> i > 3, Chain.fromList(6 :: 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = Chain.flatten(Chain.empty(): Chain[Chain[Unit]]) == Chain.empty()
    
    @test
    def flatten02(): Bool = Chain.flatten(Chain.point(Chain.empty())): Chain[Unit] == Chain.empty()
    
    @test
    def flatten03(): Bool = Chain.flatten(Chain.point(Chain.point(1))) == Chain.point(1)
    
    @test
    def flatten04(): Bool = Chain.flatten(Chain.point(Chain.fromList(1 :: 2 :: Nil))) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def flatten05(): Bool = Chain.flatten(Chain.fromList(Chain.empty() :: Chain.empty() :: Nil)): Chain[Unit] == Chain.empty()
    
    @test
    def flatten06(): Bool = Chain.flatten(Chain.fromList(Chain.point(1) :: Chain.empty() :: Nil)) == Chain.point(1)
    
    @test
    def flatten07(): Bool = Chain.flatten(Chain.fromList(Chain.empty() :: Chain.point(1) :: Nil)) == Chain.point(1)
    
    @test
    def flatten08(): Bool = Chain.flatten(Chain.fromList(Chain.point(1) :: Chain.point(2) :: Nil)) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def flatten09(): Bool = Chain.flatten(Chain.fromList(Chain.fromList(1 :: 2 :: Nil) :: Chain.fromList(3 :: 4 :: 5 :: Nil) :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: Nil)
    
    @test
    def flatten10(): Bool = Chain.flatten(Chain.fromList(Chain.point(1) :: Chain.fromList(2 :: 3 :: Nil) :: Chain.point(4) :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Chain.exists(i -> i > 3, Chain.empty()) == false

    @test
    def exists02(): Bool = Chain.exists(i -> i > 3, Chain.point(1)) == false

    @test
    def exists03(): Bool = Chain.exists(i -> i > 3, Chain.point(5)) == true

    @test
    def exists04(): Bool = Chain.exists(i -> i > 3, Chain.fromList(1 :: 2 :: Nil)) == false

    @test
    def exists05(): Bool = Chain.exists(i -> i > 3, Chain.fromList(1 :: 6 :: Nil)) == true

    @test
    def exists06(): Bool = Chain.exists(i -> i > 3, Chain.fromList(6 :: 1 :: Nil)) == true

    @test
    def exists07(): Bool = Chain.exists(i -> i > 3, Chain.fromList(16 :: 6 :: Nil)) == true

    @test
    def exists08(): Bool = Chain.exists(i -> i > 3, Chain.fromList(1 :: -9 :: 3 :: Nil)) == false

    @test
    def exists09(): Bool = Chain.exists(i -> i > 3, Chain.fromList(1 :: 9 :: 3 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = Chain.forall(i -> i > 3, Chain.empty()) == true
    
    @test
    def forall02(): Bool = Chain.forall(i -> i > 3, Chain.point(1)) == false
    
    @test
    def forall03(): Bool = Chain.forall(i -> i > 3, Chain.point(5)) == true
    
    @test
    def forall04(): Bool = Chain.forall(i -> i > 3, Chain.fromList(1 :: 2 :: Nil)) == false
    
    @test
    def forall05(): Bool = Chain.forall(i -> i > 3, Chain.fromList(1 :: 6 :: Nil)) == false
    
    @test
    def forall06(): Bool = Chain.forall(i -> i > 3, Chain.fromList(6 :: 1 :: Nil)) == false
    
    @test
    def forall07(): Bool = Chain.forall(i -> i > 3, Chain.fromList(16 :: 6 :: Nil)) == true
    
    @test
    def forall08(): Bool = Chain.forall(i -> i > 3, Chain.fromList(1 :: -9 :: 3 :: Nil)) == false
    
    @test
    def forall09(): Bool = Chain.forall(i -> i > 3, Chain.fromList(1 :: 9 :: 3 :: Nil)) == false
    
    @test
    def forall10(): Bool = Chain.forall(i -> i > 3, Chain.fromList(11 :: 9 :: 31 :: Nil)) == true

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = Chain.filter(i -> i > 3, Chain.empty()) == Chain.empty()
    
    @test
    def filter02(): Bool = Chain.filter(i -> i > 3, Chain.point(2)) == Chain.empty()
    
    @test
    def filter03(): Bool = Chain.filter(i -> i > 3, Chain.point(4)) == Chain.point(4)
    
    @test
    def filter04(): Bool = Chain.filter(i -> i > 3, Chain.fromList(1 :: 3 :: Nil)) == Chain.empty()
    
    @test
    def filter05(): Bool = Chain.filter(i -> i > 3, Chain.fromList(1 :: 8 :: Nil)) == Chain.point(8)
    
    @test
    def filter06(): Bool = Chain.filter(i -> i > 3, Chain.fromList(8 :: 1 :: Nil)) == Chain.point(8)
    
    @test
    def filter07(): Bool = Chain.filter(i -> i > 3, Chain.fromList(8 :: 9 :: Nil)) == Chain.fromList(8 :: 9 :: Nil)
    
    @test
    def filter08(): Bool = Chain.filter(i -> i > 3, Chain.fromList(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) == Chain.fromList(4 :: 11 :: 17 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain.empty()) == 100

    @test
    def foldLeft02(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain.point(1)) == 198

    @test
    def foldLeft03(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain.fromList(1 :: 2 :: Nil)) == 196

    @test
    def foldLeft04(): Bool = Chain.foldLeft((i, e) -> (i - e)*(e rem 2 + 1), 100, Chain.fromList(1 :: 2 :: 3 :: Nil)) == 386

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = Chain.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain.empty()) == 100

    @test
    def foldRight02(): Bool = Chain.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain.point(1)) == 198

    @test
    def foldRight03(): Bool = Chain.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain.fromList(1 :: 2 :: Nil)) == 194

    @test
    def foldRight04(): Bool = Chain.foldRight((e, i) -> (i - e)*(e rem 2 + 1), 100, Chain.fromList(1 :: 2 :: 3 :: Nil)) == 382

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropLeft01(): Bool = Chain.dropLeft(-1, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def dropLeft02(): Bool = Chain.dropLeft(0, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def dropLeft03(): Bool = Chain.dropLeft(1, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def dropLeft04(): Bool = Chain.dropLeft(-1, Chain.point(1)) == Chain.point(1)
    
    @test
    def dropLeft05(): Bool = Chain.dropLeft(0, Chain.point(1)) == Chain.point(1)
    
    @test
    def dropLeft06(): Bool = Chain.dropLeft(1, Chain.point(1)) == Chain.empty()
    
    @test
    def dropLeft07(): Bool = Chain.dropLeft(2, Chain.point(1)) == Chain.empty()
    
    @test
    def dropLeft08(): Bool = Chain.dropLeft(0, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def dropLeft09(): Bool = Chain.dropLeft(1, Chain.fromList(1 :: 2 :: Nil)) == Chain.point(2)
    
    @test
    def dropLeft10(): Bool = Chain.dropLeft(2, Chain.fromList(1 :: 2 :: Nil)) == Chain.empty()
    
    @test
    def dropLeft11(): Bool = Chain.dropLeft(2, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(3 :: 4 :: 5 :: 6 :: Nil)
    
    @test
    def dropLeft12(): Bool = Chain.dropLeft(4, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(5 :: 6 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropRight01(): Bool = Chain.dropRight(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def dropRight02(): Bool = Chain.dropRight(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def dropRight03(): Bool = Chain.dropRight(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def dropRight04(): Bool = Chain.dropRight(-1, Chain.point(1)) == Chain.point(1)

    @test
    def dropRight05(): Bool = Chain.dropRight(0, Chain.point(1)) == Chain.point(1)

    @test
    def dropRight06(): Bool = Chain.dropRight(1, Chain.point(1)) == Chain.empty()

    @test
    def dropRight07(): Bool = Chain.dropRight(2, Chain.point(1)) == Chain.empty()

    @test
    def dropRight08(): Bool = Chain.dropRight(0, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def dropRight09(): Bool = Chain.dropRight(1, Chain.fromList(1 :: 2 :: Nil)) == Chain.point(1)

    @test
    def dropRight10(): Bool = Chain.dropRight(2, Chain.fromList(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def dropRight11(): Bool = Chain.dropRight(2, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: 4 :: Nil)

    @test
    def dropRight12(): Bool = Chain.dropRight(4, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.empty()) == Chain.empty()
    
    @test
    def dropWhileLeft02(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.point(1)) == Chain.point(1)
    
    @test
    def dropWhileLeft03(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.point(4)) == Chain.empty()
    
    @test
    def dropWhileLeft04(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def dropWhileLeft05(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.fromList(1 :: 5 :: Nil)) == Chain.fromList(1 :: 5 :: Nil)
    
    @test
    def dropWhileLeft06(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.fromList(5 :: 1 :: Nil)) == Chain.point(1)
    
    @test
    def dropWhileLeft07(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.fromList(5 :: 8 :: Nil)) == Chain.empty()
    
    @test
    def dropWhileLeft08(): Bool = Chain.dropWhileLeft(i -> i > 3, Chain.fromList(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            Chain.fromList(-3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.empty()) == Chain.empty()

    @test
    def dropWhileRight02(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.point(1)) == Chain.point(1)

    @test
    def dropWhileRight03(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.point(4)) == Chain.empty()

    @test
    def dropWhileRight04(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def dropWhileRight05(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.fromList(1 :: 5 :: Nil)) == Chain.point(1)

    @test
    def dropWhileRight06(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.fromList(5 :: 1 :: Nil)) == Chain.fromList(5 :: 1 :: Nil)

    @test
    def dropWhileRight07(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.fromList(5 :: 8 :: Nil)) == Chain.empty()

    @test
    def dropWhileRight08(): Bool = Chain.dropWhileRight(i -> i > 3, Chain.fromList(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil)) ==
                            Chain.fromList(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeLeft01(): Bool = Chain.takeLeft(-1, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def takeLeft02(): Bool = Chain.takeLeft(0, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def takeLeft03(): Bool = Chain.takeLeft(1, Chain.empty(): Chain[Unit]) == Chain.empty()
    
    @test
    def takeLeft04(): Bool = Chain.takeLeft(-1, Chain.point(1)) == Chain.empty()
    
    @test
    def takeLeft05(): Bool = Chain.takeLeft(0, Chain.point(1)) == Chain.empty()
    
    @test
    def takeLeft06(): Bool = Chain.takeLeft(1, Chain.point(1)) == Chain.point(1)
    
    @test
    def takeLeft07(): Bool = Chain.takeLeft(2, Chain.point(1)) == Chain.point(1)
    
    @test
    def takeLeft08(): Bool = Chain.takeLeft(0, Chain.fromList(1 :: 2 :: Nil)) == Chain.empty()
    
    @test
    def takeLeft09(): Bool = Chain.takeLeft(1, Chain.fromList(1 :: 2 :: Nil)) == Chain.point(1)
    
    @test
    def takeLeft10(): Bool = Chain.takeLeft(2, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def takeLeft11(): Bool = Chain.takeLeft(2, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)
    
    @test
    def takeLeft12(): Bool = Chain.takeLeft(4, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(1 :: 2 :: 3 :: 4 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeRight01(): Bool = Chain.takeRight(-1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def takeRight02(): Bool = Chain.takeRight(0, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def takeRight03(): Bool = Chain.takeRight(1, Chain.empty(): Chain[Unit]) == Chain.empty()

    @test
    def takeRight04(): Bool = Chain.takeRight(-1, Chain.point(1)) == Chain.empty()

    @test
    def takeRight05(): Bool = Chain.takeRight(0, Chain.point(1)) == Chain.empty()

    @test
    def takeRight06(): Bool = Chain.takeRight(1, Chain.point(1)) == Chain.point(1)

    @test
    def takeRight07(): Bool = Chain.takeRight(2, Chain.point(1)) == Chain.point(1)

    @test
    def takeRight08(): Bool = Chain.takeRight(0, Chain.fromList(1 :: 2 :: Nil)) == Chain.empty()

    @test
    def takeRight09(): Bool = Chain.takeRight(1, Chain.fromList(1 :: 2 :: Nil)) == Chain.point(2)

    @test
    def takeRight10(): Bool = Chain.takeRight(2, Chain.fromList(1 :: 2 :: Nil)) == Chain.fromList(1 :: 2 :: Nil)

    @test
    def takeRight11(): Bool = Chain.takeRight(2, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(5 :: 6 :: Nil)

    @test
    def takeRight12(): Bool = Chain.takeRight(4, Chain.fromList(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil)) == Chain.fromList(3 :: 4 :: 5 :: 6 :: Nil)

}
