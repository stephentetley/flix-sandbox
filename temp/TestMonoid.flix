/*
 *  Copyright 2020 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace TestMonoid {

use Monoid.{empty, combine};

/////////////////////////////////////////////////////////////////////////////
// Unit                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def unitEmpty01(): Bool = empty() == ()

@test
def unitCombine01(): Bool = combine((), ()) == ()

/////////////////////////////////////////////////////////////////////////////
// String                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def stringEmpty01(): Bool = empty() == ""

@test
def stringCombine01(): Bool = combine("", "") == ""

@test
def stringCombine02(): Bool = combine("a", "") == "a"

@test
def stringCombine03(): Bool = combine("", "b") == "b"

@test
def stringCombine04(): Bool = combine("a", "b") == "ab"

/////////////////////////////////////////////////////////////////////////////
// Tuple 2                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def tuple2Empty01(): Bool = empty() == ("", "")

@test
def tuple2Combine01(): Bool = combine(("", ""), ("", "")) == ("", "")

@test
def tuple2Combine02(): Bool = combine(("a", "b"), ("", "")) == ("a", "b")

@test
def tuple2Combine03(): Bool = combine(("", ""), ("x", "y")) == ("x", "y")

@test
def tuple2Combine04(): Bool = combine(("a", "b"), ("x", "y")) == ("ax", "by")

/////////////////////////////////////////////////////////////////////////////
// Tuple 2                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def tuple3Empty01(): Bool = empty() == ("", "", "")

@test
def tuple3Combine01(): Bool = combine(("", "", ""), ("", "", "")) == ("", "", "")

@test
def tuple3Combine02(): Bool = combine(("a", "b", "c"), ("", "", "")) == ("a", "b", "c")

@test
def tuple3Combine03(): Bool = combine(("", "", ""), ("x", "y", "z")) == ("x", "y", "z")

@test
def tuple3Combine04(): Bool = combine(("a", "b", "c"), ("x", "y", "z")) == ("ax", "by", "cz")

}