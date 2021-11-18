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

namespace TestFunctor {

    use FunctorExtras.{ignore, <&>, <&, &>};

    /////////////////////////////////////////////////////////////////////////////
    // ignore                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ignore01(): Bool = ignore(None) == None

    @test
    def ignore02(): Bool = ignore(Some(1)) == Some(())

    @test
    def ignore03(): Bool = ignore(Nil) == Nil

    @test
    def ignore04(): Bool = ignore(1 :: Nil) == () :: Nil

    @test
    def ignore05(): Bool = ignore(1 :: 2 :: Nil) == () :: () :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // mapSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapSym01(): Bool = ((i -> 2*i) <&> None) == None

    @test
    def mapSym02(): Bool = ((i -> 2*i) <&> Some(1)) == Some(2)

    @test
    def mapSym03(): Bool = ((i -> 2*i) <&> Nil) == Nil

    @test
    def mapSym04(): Bool = ((i -> 2*i) <&> 1 :: Nil) == 2 :: Nil

    @test
    def mapSym05(): Bool = ((i -> 2*i) <&> 1 :: 2 :: Nil) == 2 :: 4 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // asSym                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def asSym01(): Bool = ('A' <& None) == None

    @test
    def asSym02(): Bool = ('A' <& Some(1)) == Some('A')

    @test
    def asSym03(): Bool = ('A' <& Nil) == Nil

    @test
    def asSym04(): Bool = ('A' <& 1 :: Nil) == 'A' :: Nil

    @test
    def asSym05(): Bool = ('A' <& 1 :: 2 :: Nil) == 'A' :: 'A' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // asSymFlipped                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def asSymFlipped01(): Bool = (None &> 'A') == None

    @test
    def asSymFlipped02(): Bool = (Some(1) &> 'A') == Some('A')

    @test
    def asSymFlipped03(): Bool = (Nil &> 'A') == Nil

    @test
    def asSymFlipped04(): Bool = ((1 :: Nil) &> 'A') == 'A' :: Nil

    @test
    def asSymFlipped05(): Bool = ((1 :: 2 :: Nil) &> 'A') == 'A' :: 'A' :: Nil

}