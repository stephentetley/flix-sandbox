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
        BagMultiMap.singleton(1, 'a') |> BagMultiMap.toAscList == (1, 'a') :: Nil

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
    def keysOf01(): Bool = 
        BagMultiMap.keysOf(BagMultiMap.empty(): BagMultiMap[Int32, Char]) == Set#{}

    @test
    def keysOf02(): Bool = BagMultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @test
    def keysOf03(): Bool = BagMultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @test
    def keysOf04(): Bool = BagMultiMap.keysOf(toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) == Set#{1, 2}
}
