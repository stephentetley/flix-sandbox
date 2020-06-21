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

namespace TestRegex {


/////////////////////////////////////////////////////////////////////////////
// matches                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def matches01(): Bool =
    use Result.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".*");
        Ok("".matches(p1))
    };
    step() == Ok(true)

@test
def matches02(): Bool =
    use Result.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".*");
        Ok("a".matches(p1))
    };
    step() == Ok(true)

@test
def matches03(): Bool =
    use Result.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".+");
        Ok("".matches(p1))
    };
    step() == Ok(false)

@test
def matches04(): Bool =
    use Result.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");    // 1+ digits
        Ok("aaa".matches(p1))
    };
    step() == Ok(false)

@test
def matches05(): Bool =
    use Result.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("1234a".matches(p1))
    };
    step() == Ok(false)

/////////////////////////////////////////////////////////////////////////////
// contains                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def contains01(): Bool =
    use Result.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("".contains(p1))
    };
    step() == Ok(false)

@test
def contains02(): Bool =
    use Result.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("123".contains(p1))
    };
    step() == Ok(true)

@test
def contains03(): Bool =
    use Result.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("123abc".contains(p1))
    };
    step() == Ok(true)

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def startsWith01(): Bool =
    use Result.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("".startsWith(p1))
    };
    step() == Ok(false)

@test
def startsWith02(): Bool =
    use Result.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("123".startsWith(p1))
    };
    step() == Ok(true)

@test
def startsWith03(): Bool =
    use Result.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Ok("123abc".startsWith(p1))
    };
    step() == Ok(true)

/////////////////////////////////////////////////////////////////////////////
// evalMatches                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def evalMatches01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalMatches};
    let eval = Text/Regex/MatchEvaluator.getNamedCapture("num");
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("(?<num>\d+)");
        "123".evalMatches(p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// evalContains                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def evalContains01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalContains};
    let eval = Text/Regex/MatchEvaluator.getNamedCapture("num");
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("(?<num>\d+)");
        "aa 123 bb".evalContains(p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// evalStartsWith                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def evalStartsWith01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalStartsWith};
    let eval = Text/Regex/MatchEvaluator.getNamedCapture("num");
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("(?<num>\d+)");
        "123 bb".evalStartsWith(p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// results                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def results01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{results};
    use Text/Regex/MatchEvaluator.{value};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\p{Alpha}+");
        let strm = results(p1, value(), "aa bb cc");
        Ok(strm)
    };
    Result.map(Data/Stream.toList, step()) == Ok("aa" :: "bb" :: "cc" :: Nil)


}