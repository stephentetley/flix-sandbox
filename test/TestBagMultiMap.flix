/*
 * Copyright 2022 Stephen Tetley
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

namespace TestBagMultiMap {

    use BagMultiMap.{singleton};

    def toBagMultiMap(l: List[(k, v)]): BagMultiMap[k, v] with Order[k], Order[v] = 
        List.foldLeft((acc, kv) -> let (k, v) = kv; BagMultiMap.insert(k, v, acc), BagMultiMap.empty(), l)

    def isEven(i: Int32): Bool = i rem 2 == 0
    
    def isOdd(i: Int32): Bool = not isEven(i)

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool = BagMultiMap.isEmpty(BagMultiMap.empty())

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = 
        BagMultiMap.singleton(1, 'a') |> BagMultiMap.toList == (1, 'a') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool = 
        BagMultiMap.fromMap(Map#{}: Map[Int32, Char]) |> BagMultiMap.toList == Nil

    @test
    def fromMap02(): Bool = 
        BagMultiMap.fromMap(Map#{1 => 'a'}) |> BagMultiMap.toList == (1, 'a') :: Nil

    @test
    def fromMap03(): Bool = 
        BagMultiMap.fromMap(Map#{1 => 'a', 2 => 'b'}) |> BagMultiMap.toList == (1, 'a') :: (2, 'b') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = 
        BagMultiMap.isEmpty(BagMultiMap.empty()) == true
    
    @test
    def isEmpty02(): Bool = 
        BagMultiMap.isEmpty(singleton(1, 'a')) == false

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = 
        BagMultiMap.get(1, BagMultiMap.empty(): BagMultiMap[Int32, Char]) == Nil
    
    @test
    def get02(): Bool = 
        BagMultiMap.get(1, singleton(1, 'a')) == 'a' :: Nil

    @test
    def get03(): Bool = 
        BagMultiMap.get(1, toBagMultiMap((1, 'a') :: (2, 'b') :: Nil)) == 'a' :: Nil

    @test
    def get04(): Bool = 
        BagMultiMap.get(1, toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) |> List.sort == 'a' :: 'c' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = 
        not BagMultiMap.memberOf(1, BagMultiMap.empty(): BagMultiMap[Int32, Char])

    @test
    def memberOf02(): Bool = BagMultiMap.memberOf(1, singleton(1, 'a'))

    @test
    def memberOf03(): Bool = not BagMultiMap.memberOf(1, singleton(2, 'b'))

    @test
    def memberOf04(): Bool = BagMultiMap.memberOf(1, toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def keysOf01(): Bool = BagMultiMap.keysOf(BagMultiMap.empty(): BagMultiMap[Int32, Char]) == Set#{}

    @test
    def keysOf02(): Bool = BagMultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @test
    def keysOf03(): Bool = BagMultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @test
    def keysOf04(): Bool = BagMultiMap.keysOf(toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) == Set#{1, 2}


    /////////////////////////////////////////////////////////////////////////////
    // valuesOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def valuesOf01(): Bool = BagMultiMap.valuesOf(BagMultiMap.empty(): BagMultiMap[Int32, Char]) == Nil

    @test
    def valuesOf02(): Bool = BagMultiMap.valuesOf(singleton(1, 'a')) |> List.sort == 'a' :: Nil

    @test
    def valuesOf03(): Bool = BagMultiMap.valuesOf(singleton(2, 'b')) |> List.sort == 'b' :: Nil

    @test
    def valuesOf04(): Bool = 
        BagMultiMap.valuesOf(toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) 
            |> List.sort == 'a' :: 'b' :: 'c' :: Nil

    @test
    def valuesOf05(): Bool = 
        BagMultiMap.valuesOf(toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: (3, 'a') :: Nil)) 
            |> List.sort == 'a' :: 'a' :: 'b' :: 'c' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // insert                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def insert01(): Bool = 
        BagMultiMap.insert(1, 'a', BagMultiMap.empty()) |> BagMultiMap.toList |> List.sort == (1, 'a') :: Nil

    @test
    def insert02(): Bool = 
        BagMultiMap.empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b') 
            |> BagMultiMap.toList |> List.sort == (1, 'a') :: (2, 'b') :: Nil

    @test
    def insert03(): Bool = 
        BagMultiMap.empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b') |> BagMultiMap.insert(1, 'c') 
            |> BagMultiMap.toList |> List.sort == (1, 'a') :: (1, 'c') :: (2, 'b') :: Nil

    @test
    def insert04(): Bool = 
        BagMultiMap.empty() |> BagMultiMap.insert(1, 'a') |> BagMultiMap.insert(2, 'b') |> BagMultiMap.insert(1, 'c') |> BagMultiMap.insert(3, 'b') 
            |> BagMultiMap.toList |> List.sort == (1, 'a') :: (1, 'c') :: (2, 'b') :: (3, 'b') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjust                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjust01(): Bool = BagMultiMap.adjust(v -> 2*v, 1, BagMultiMap.empty()) == BagMultiMap.empty()

    @test
    def adjust02(): Bool = 
        BagMultiMap.adjust(v -> 2*v, 1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 8) :: Nil

    @test
    def adjust03(): Bool = 
        BagMultiMap.adjust(v -> 2*v, 3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def adjust04(): Bool = 
        BagMultiMap.adjust(v -> 2*v, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 8) :: (1, 10) :: (2, 4) :: Nil

    @test
    def adjust05(): Bool = 
        BagMultiMap.adjust(v -> 2*v, 2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 8) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // adjustWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def adjustWithKey01(): Bool = BagMultiMap.adjustWithKey((k, v) -> k+v, 1, BagMultiMap.empty()) == BagMultiMap.empty()

    @test
    def adjustWithKey02(): Bool = 
        BagMultiMap.adjustWithKey((k, v) -> k+v, 1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 5) :: Nil

    @test
    def adjustWithKey03(): Bool = 
        BagMultiMap.adjustWithKey((k, v) -> k+v, 3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def adjustWithKey04(): Bool = 
        BagMultiMap.adjustWithKey((k, v) -> k+v, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 5) :: (1, 6) :: (2, 4) :: Nil

    @test
    def adjustWithKey05(): Bool = 
        BagMultiMap.adjustWithKey((k, v) -> k+v, 2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 6) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def update01(): Bool = BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, BagMultiMap.empty()) == BagMultiMap.empty()

    @test
    def update02(): Bool = 
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 8) :: Nil

    @test
    def update03(): Bool = 
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def update04(): Bool & Impure = 
        println("update04");
        println(BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5)));
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 5) :: (1, 8) :: (2, 4) :: Nil

    @test
    def update05(): Bool = 
        BagMultiMap.update(v -> if (isEven(v)) Some(2*v) else None, 2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 8) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // updateWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def updateWithKey01(): Bool = BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, BagMultiMap.empty()) == BagMultiMap.empty()

    @test
    def updateWithKey02(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def updateWithKey03(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 5) :: Nil

    @test
    def updateWithKey04(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def updateWithKey05(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def updateWithKey06(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 4) :: Nil

    @test
    def updateWithKey07(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 5) :: (1, 6) :: (2, 4) :: Nil

    @test
    def updateWithKey08(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isEven(k)) Some(k+v) else None, 2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 6) :: Nil

    @test
    def updateWithKey09(): Bool = 
        BagMultiMap.updateWithKey((k, v) -> if (isOdd(k)) Some(k+v) else None, 2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: (2, 4) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // remove                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def remove01(): Bool = BagMultiMap.remove(1, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == BagMultiMap.empty()

    @test
    def remove02(): Bool = 
        BagMultiMap.remove(1, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == Nil

    @test
    def remove03(): Bool = 
        BagMultiMap.remove(3, BagMultiMap.singleton(1, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def remove04(): Bool = 
        BagMultiMap.remove(1, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (2, 4) :: Nil

    @test
    def remove05(): Bool = 
        BagMultiMap.remove(2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 5) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = BagMultiMap.find((k, v) -> k == v, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def find02(): Bool = BagMultiMap.find((k, v) -> k == v, BagMultiMap.singleton(1, 2)) == None

    @test
    def find03(): Bool = BagMultiMap.find((k, v) -> k == v, BagMultiMap.singleton(1, 1)) == Some((1, 1))

    @test
    def find04(): Bool = 
        BagMultiMap.find((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5)) == None

    @test
    def find05(): Bool = 
        BagMultiMap.find((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(3, 3)) == Some((2,2))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = BagMultiMap.findLeft((k, v) -> k == v, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def findLeft02(): Bool = BagMultiMap.findLeft((k, v) -> k == v, BagMultiMap.singleton(1, 2)) == None

    @test
    def findLeft03(): Bool = BagMultiMap.findLeft((k, v) -> k == v, BagMultiMap.singleton(1, 1)) == Some((1, 1))

    @test
    def findLeft04(): Bool = 
        BagMultiMap.findLeft((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5)) == None

    @test
    def findLeft05(): Bool = 
        BagMultiMap.findLeft((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(3, 3)) == Some((2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = BagMultiMap.findRight((k, v) -> k == v, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def findRight02(): Bool = BagMultiMap.findRight((k, v) -> k == v, BagMultiMap.singleton(1, 2)) == None

    @test
    def findRight03(): Bool = BagMultiMap.findRight((k, v) -> k == v, BagMultiMap.singleton(1, 1)) == Some((1, 1))

    @test
    def findRight04(): Bool = 
        BagMultiMap.findRight((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4) |> BagMultiMap.insert(1, 5)) == None

    @test
    def findRight05(): Bool = 
        BagMultiMap.findRight((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(3, 3)) == Some((3, 3))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = 
        BagMultiMap.filter(isEven, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == BagMultiMap.empty()

    @test
    def filter02(): Bool = 
        BagMultiMap.filter(isEven, BagMultiMap.singleton(1, 2))
            |> BagMultiMap.toList |> List.sort == (1, 2) :: Nil

    @test
    def filter03(): Bool = 
        BagMultiMap.filter(isEven, BagMultiMap.singleton(1, 1)) == BagMultiMap.empty()

    @test
    def filter04(): Bool = 
        BagMultiMap.filter(isEven, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4))
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (2, 4) :: Nil

    @test
    def filter05(): Bool = 
        BagMultiMap.filter(isEven, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 3)) 
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (2, 2) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filterWithKey                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterWithKey01(): Bool = 
        BagMultiMap.filterWithKey((k, v) -> k == v, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == BagMultiMap.empty()

    @test
    def filterWithKey02(): Bool = 
        BagMultiMap.filterWithKey((k, v) -> k == v, BagMultiMap.singleton(1, 2)) == BagMultiMap.empty()

    @test
    def filterWithKey03(): Bool = 
        BagMultiMap.filterWithKey((k, v) -> k == v, BagMultiMap.singleton(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 1) :: Nil

    @test
    def filterWithKey04(): Bool = 
        BagMultiMap.filterWithKey((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4)) == BagMultiMap.empty()

    @test
    def filterWithKey05(): Bool = 
        BagMultiMap.filterWithKey((k, v) -> k == v, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 1) :: (2, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = 
        BagMultiMap.map(v -> v*3, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == BagMultiMap.empty()

    @test
    def map02(): Bool = 
        BagMultiMap.map(v -> v*3, BagMultiMap.singleton(1, 2))
            |> BagMultiMap.toList |> List.sort == (1, 6) :: Nil

    @test
    def map03(): Bool = 
        BagMultiMap.map(v -> v*3, BagMultiMap.singleton(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 3) :: Nil

    @test
    def map04(): Bool = 
        BagMultiMap.map(v -> v*3, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4))
            |> BagMultiMap.toList |> List.sort == (1, 12) :: (2, 12) :: Nil

    @test
    def map05(): Bool = 
        BagMultiMap.map(v -> v*3, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 3) :: (1, 12) :: (2, 6) :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // mapWithKey                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithKey01(): Bool = 
        BagMultiMap.mapWithKey((k, v) -> v*3+k, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == BagMultiMap.empty()

    @test
    def mapWithKey02(): Bool = 
        BagMultiMap.mapWithKey((k, v) -> v*3+k, BagMultiMap.singleton(1, 2))
            |> BagMultiMap.toList |> List.sort == (1, 7) :: Nil

    @test
    def mapWithKey03(): Bool = 
        BagMultiMap.mapWithKey((k, v) -> v*3+k, BagMultiMap.singleton(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 4) :: Nil

    @test
    def mapWithKey04(): Bool = 
        BagMultiMap.mapWithKey((k, v) -> v*3+k, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4))
            |> BagMultiMap.toList |> List.sort == (1, 13) :: (2, 14) :: Nil

    @test
    def mapWithKey05(): Bool = 
        BagMultiMap.mapWithKey((k, v) -> v*3+k, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) 
            |> BagMultiMap.toList |> List.sort == (1, 4) :: (1, 13) :: (2, 8) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldWithKey                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldWithKey01(): Bool = 
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def foldWithKey02(): Bool = 
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, BagMultiMap.singleton(1, 2)) == 3

    @test
    def foldWithKey03(): Bool = 
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, BagMultiMap.singleton(1, 1)) == 2

    @test
    def foldWithKey04(): Bool = 
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4)) == 11

    @test
    def foldWithKey05(): Bool = 
        BagMultiMap.foldWithKey((acc, k, v) -> acc + k + v, 0, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 11

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = 
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeft02(): Bool = 
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, BagMultiMap.singleton(1, 2)) == 2 :: Nil

    @test
    def foldLeft03(): Bool = 
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, BagMultiMap.singleton(1, 1)) == 1 :: Nil

    @test
    def foldLeft04(): Bool = 
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == 5 :: 4 :: Nil

    @test
    def foldLeft05(): Bool = 
        BagMultiMap.foldLeft((acc, v) -> v :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 2 :: 4 :: 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftWithKey                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeftWithKey01(): Bool = 
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldLeftWithKey02(): Bool = 
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, BagMultiMap.singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldLeftWithKey03(): Bool = 
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, BagMultiMap.singleton(1, 1)) == (1, 1) :: Nil

    @test
    def foldLeftWithKey04(): Bool = 
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == (2, 5) :: (1, 4) :: Nil

    @test
    def foldLeftWithKey05(): Bool = 
        BagMultiMap.foldLeftWithKey((acc, k, v) -> (k, v) :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == (2, 2) :: (1, 4) :: (1, 1) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = 
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldRight02(): Bool = 
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, BagMultiMap.singleton(1, 2)) == 2 :: Nil

    @test
    def foldRight03(): Bool = 
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, BagMultiMap.singleton(1, 1)) == 1 :: Nil

    @test
    def foldRight04(): Bool = 
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == 4 :: 5 :: Nil

    @test
    def foldRight05(): Bool = 
        BagMultiMap.foldRight((v, acc) -> v :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 1 :: 4 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithKey                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithKey01(): Bool = 
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == Nil

    @test
    def foldRightWithKey02(): Bool = 
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, BagMultiMap.singleton(1, 2)) == (1, 2) :: Nil

    @test
    def foldRightWithKey03(): Bool = 
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, BagMultiMap.singleton(1, 1)) == (1, 1) :: Nil

    @test
    def foldRightWithKey04(): Bool = 
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == (1, 4) :: (2, 5) :: Nil

    @test
    def foldRightWithKey05(): Bool = 
        BagMultiMap.foldRightWithKey((k, v, acc) -> (k, v) :: acc, Nil, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == (1, 1) :: (1, 4) :: (2, 2) :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = 
        BagMultiMap.reduceLeft((v1, v2) -> v1 - v2, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceLeft02(): Bool = 
        BagMultiMap.reduceLeft((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 2)) == Some(2)

    @test
    def reduceLeft03(): Bool = 
        BagMultiMap.reduceLeft((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 1)) == Some(1)

    @test
    def reduceLeft04(): Bool = 
        BagMultiMap.reduceLeft((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == Some(-1)

    @test
    def reduceLeft05(): Bool =           
        BagMultiMap.reduceLeft((v1, v2) -> v1 - v2, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == Some(-5)



    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftWithKey                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeftWithKey01(): Bool = 
        BagMultiMap.reduceLeftWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceLeftWithKey02(): Bool = 
        BagMultiMap.reduceLeftWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.singleton(1, 2)) == Some((1, 2))

    @test
    def reduceLeftWithKey03(): Bool = 
        BagMultiMap.reduceLeftWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.singleton(1, 1)) == Some((1, 1))

    @test
    def reduceLeftWithKey04(): Bool = 
        BagMultiMap.reduceLeftWithKey((k1, v1, _, v2) -> (k1, v1 - v2), 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == Some((1, -1))

    @test
    def reduceLeftWithKey05(): Bool =           
        BagMultiMap.reduceLeftWithKey((k1, v1, _, v2) -> (k1, v1 - v2), 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == Some((1, -5))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = 
        BagMultiMap.reduceRight((v1, v2) -> v1 - v2, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceRight02(): Bool = 
        BagMultiMap.reduceRight((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 2)) == Some(2)

    @test
    def reduceRight03(): Bool = 
        BagMultiMap.reduceRight((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 1)) == Some(1)

    @test
    def reduceRight04(): Bool = 
        BagMultiMap.reduceRight((v1, v2) -> v1 - v2, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == Some(1)

    @test
    def reduceRight05(): Bool =           
        BagMultiMap.reduceRight((v1, v2) -> v1 - v2, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == Some(-3)

    /////////////////////////////////////////////////////////////////////////////
    // reduceRightWithKey                                                      //
    /////////////////////////////////////////////////////////////////////////////
/*
    @test
    def reduceRightWithKey01(): Bool = 
        BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == None

    @test
    def reduceRightWithKey02(): Bool = 
        BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.singleton(1, 2)) == Some((1, 2))

    @test
    def reduceRightWithKey03(): Bool = 
        BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), BagMultiMap.singleton(1, 1)) == Some((1, 1))

    @test
    def reduceRightWithKey04(): Bool = 
        BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 5)) == Some((1, -1))
*/
    @test
    def reduceRightWithKey05(): Bool & Impure =
        println("reduceRightWithKey05()");
        println(BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)));
        BagMultiMap.reduceRightWithKey((k1, v1, _, v2) -> (k1, v1 - v2), 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == Some((2, -3))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = 
        BagMultiMap.count((k, v) -> v > k, BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def count02(): Bool = 
        BagMultiMap.count((k, v) -> v > k, BagMultiMap.singleton(1, 2)) == 1

    @test
    def count03(): Bool = 
        BagMultiMap.count((k, v) -> v > k, BagMultiMap.singleton(1, 1)) == 0

    @test
    def count04(): Bool = 
        BagMultiMap.count((k, v) -> v > k, BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4)) == 2

    @test
    def count05(): Bool = 
        BagMultiMap.count((k, v) -> v > k, 
            BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 1

    /////////////////////////////////////////////////////////////////////////////
    // sumKeys                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumKeys01(): Bool = BagMultiMap.sumKeys(BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def sumKeys02(): Bool = BagMultiMap.sumKeys(BagMultiMap.singleton(1, 2)) == 1

    @test
    def sumKeys03(): Bool = BagMultiMap.sumKeys(BagMultiMap.singleton(1, 1)) == 1

    @test
    def sumKeys04(): Bool = BagMultiMap.sumKeys(BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4)) == 3

    @test
    def sumKeys05(): Bool = 
        BagMultiMap.sumKeys(BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 4

    /////////////////////////////////////////////////////////////////////////////
    // sumValues                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumValues01(): Bool = BagMultiMap.sumValues(BagMultiMap.empty(): BagMultiMap[Int32, Int32]) == 0

    @test
    def sumValues02(): Bool = BagMultiMap.sumValues(BagMultiMap.singleton(1, 2)) == 2

    @test
    def sumValues03(): Bool = BagMultiMap.sumValues(BagMultiMap.singleton(1, 1)) == 1

    @test
    def sumValues04(): Bool = BagMultiMap.sumValues(BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 4)) == 8

    @test
    def sumValues05(): Bool = 
        BagMultiMap.sumValues(BagMultiMap.singleton(1, 4) |> BagMultiMap.insert(2, 2) |> BagMultiMap.insert(1, 1)) == 7

}
