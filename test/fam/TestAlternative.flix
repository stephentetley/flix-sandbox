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

namespace TestAlternative {

    use Alternative.{zero, alt};
    use Alternative.{<|>, optional};

    /////////////////////////////////////////////////////////////////////////////
    // zero                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zero01(): Bool = Alternative.zero() : Option[Unit] == None


    /////////////////////////////////////////////////////////////////////////////
    // alt                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def alt01(): Bool = alt(None, None) == None

    @test
    def alt02(): Bool = alt(Some(1), None) == Some(1)

    @test
    def alt03(): Bool = alt(None, Some(1)) == Some(1)

    @test
    def alt04(): Bool = alt(Some(1), Some(2)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // altSym                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def altSym01(): Bool = (None <|> None) == None

    @test
    def altSym02(): Bool = (Some(1) <|> None) == Some(1)

    @test
    def altSym03(): Bool = (None <|> Some(1)) == Some(1)

    @test
    def altSym04(): Bool = (Some(1) <|> Some(2)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // optional                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def optional01(): Bool = optional(None) == Some(None)

}
