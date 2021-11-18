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

namespace TestIdentity {

    use Functor.{map};
    use ApplicativeExtras.{seqLeft, seqRight};

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Functor.map(i -> 2*i, Identity(1)) == Identity(2)

    @test
    def map02(): Bool = Functor.map(i -> 2*i, Identity(2)) == Identity(4)

    /////////////////////////////////////////////////////////////////////////////
    // seqLeft                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqLeft01(): Bool = seqLeft(Identity(1), Identity('A')) == Identity(1)

    @test
    def seqLeft02(): Bool = seqLeft(Identity(1), Identity(2)) == Identity(1)


    /////////////////////////////////////////////////////////////////////////////
    // seqRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def seqRight01(): Bool = seqRight(Identity(1), Identity('A')) == Identity('A')

    @test
    def seqRight02(): Bool = seqRight(Identity(1), Identity(2)) == Identity(2)

}
