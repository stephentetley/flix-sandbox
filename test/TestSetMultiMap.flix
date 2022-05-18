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

namespace TestSetMultiMap {

    use SetMultiMap.{singleton};

    def toBagMultiMap(l: List[(k, v)]): SetMultiMap[k, v] with Order[k], Order[v] = 
        List.foldLeft((acc, kv) -> let (k, v) = kv; SetMultiMap.insert(k, v, acc), SetMultiMap.empty(), l)


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool = SetMultiMap.isEmpty(SetMultiMap.empty())

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = 
        SetMultiMap.singleton(1, 'a') |> SetMultiMap.toList |> List.sort == (1, 'a') :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // fromMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fromMap01(): Bool = 
        SetMultiMap.fromMap(Map#{}: Map[Int32, Char]) |> SetMultiMap.toList == Nil

    @test
    def fromMap02(): Bool = 
        SetMultiMap.fromMap(Map#{1 => 'a'}) |> SetMultiMap.toList == (1, 'a') :: Nil

    @test
    def fromMap03(): Bool = 
        SetMultiMap.fromMap(Map#{1 => 'a', 2 => 'b'}) |> SetMultiMap.toList == (1, 'a') :: (2, 'b') :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = 
        SetMultiMap.isEmpty(SetMultiMap.empty()) == true
    
    @test
    def isEmpty02(): Bool = 
        SetMultiMap.isEmpty(singleton(1, 'a')) == false

    /////////////////////////////////////////////////////////////////////////////
    // get                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def get01(): Bool = 
        SetMultiMap.get(1, SetMultiMap.empty(): SetMultiMap[Int32, Char]) == Set#{}
    
    @test
    def get02(): Bool = 
        SetMultiMap.get(1, singleton(1, 'a')) == Set#{'a'}

    @test
    def get03(): Bool = 
        SetMultiMap.get(1, toBagMultiMap((1, 'a') :: (2, 'b') :: Nil)) == Set#{'a'}

    @test
    def get04(): Bool = 
        SetMultiMap.get(1, toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) == Set#{'a', 'c'}

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = 
        not SetMultiMap.memberOf(1, SetMultiMap.empty(): SetMultiMap[Int32, Char])

    @test
    def memberOf02(): Bool = SetMultiMap.memberOf(1, singleton(1, 'a'))

    @test
    def memberOf03(): Bool = not SetMultiMap.memberOf(1, singleton(2, 'b'))

    @test
    def memberOf04(): Bool = SetMultiMap.memberOf(1, toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // keysOf                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def keysOf01(): Bool = SetMultiMap.keysOf(SetMultiMap.empty(): SetMultiMap[Int32, Char]) == Set#{}

    @test
    def keysOf02(): Bool = SetMultiMap.keysOf(singleton(1, 'a')) == Set#{1}

    @test
    def keysOf03(): Bool = SetMultiMap.keysOf(singleton(2, 'b')) == Set#{2}

    @test
    def keysOf04(): Bool = SetMultiMap.keysOf(toBagMultiMap((1, 'a') :: (2, 'b') :: (1, 'c') :: Nil)) == Set#{1, 2}

}
