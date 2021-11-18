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

namespace TestApplicative {

    use ApplicativeExtras.{seqLeft, seqRight};

    /////////////////////////////////////////////////////////////////////////////
    // seqLeft                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqLeft01(): Bool = seqLeft(None, Some(1)) == None

    @test
    def seqLeft02(): Bool = seqLeft(Some(1), None) == None

    @test
    def seqLeft03(): Bool = seqLeft(Some('A'), Some(1)) == Some('A')

    @test
    def seqLeft04(): Bool = seqLeft(Nil, Nil) == Nil

    @test
    def seqLeft05(): Bool = seqLeft(1 :: Nil, Nil) == Nil

    @test
    def seqLeft06(): Bool = seqLeft(1 :: Nil, 2 :: Nil) == 1 :: Nil

    @test
    def seqLeft07(): Bool = seqLeft(1 :: Nil, 2 :: 3 :: Nil) == 1 :: 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // seqRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqRight01(): Bool = seqRight(None, Some(1)) == None

    @test
    def seqRight02(): Bool = seqRight(Some(1), None) == None

    @test
    def seqRight03(): Bool = seqRight(Some('A'), Some(1)) == Some(1)

    @test
    def seqRight04(): Bool = seqRight(Nil, Nil) == Nil

    @test
    def seqRight05(): Bool = seqRight(1 :: Nil, Nil) == Nil

    @test
    def seqRight06(): Bool = seqRight(1 :: Nil, 2 :: Nil) == 2 :: Nil

    @test
    def seqRight07(): Bool = seqRight(1 :: Nil, 2 :: 3 :: Nil) == 2 :: 3 :: Nil

}
