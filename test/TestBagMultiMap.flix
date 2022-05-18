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

}
