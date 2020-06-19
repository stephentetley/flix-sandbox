/*
 * Copyright 2020 Stephen Tetley
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

namespace TestMatcher {


/////////////////////////////////////////////////////////////////////////////
// return                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def return01(): Bool & Impure =
    use Text/Regex/Matcher.{runMatcher, return};
    runMatcher(return(3), ".*", "aaaaa") == Ok(3)

/////////////////////////////////////////////////////////////////////////////
// getMatch                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def getMatch01(): Bool & Impure =
    use Text/Regex/Matcher.{runMatcher, getMatch};
    runMatcher(getMatch(), ".*", "aa") == Ok("aa")


@test
def getMatch02(): Bool & Impure =
    use Text/Regex/Matcher.{runMatcher, flatMap, return, getMatch};
    let p1 = {
        let* _ = getMatch();
        let* b = getMatch();
        return(b)
    };
    runMatcher(p1, "\w+", "aa bb") == Ok("bb")
}