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
    use Option.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".*");
        Some("".matches(p1))
    };
    step() == Some(true)

@test
def matches02(): Bool =
    use Option.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".*");
        Some(matches("a", p1))
    };
    step() == Some(true)

@test
def matches03(): Bool =
    use Option.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile(".+");
        Some(matches("", p1))
    };
    step() == Some(false)

@test
def matches04(): Bool =
    use Option.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");    // 1+ digits
        Some(matches("aaa", p1))
    };
    step() == Some(false)

@test
def matches05(): Bool =
    use Option.flatMap;
    use Text/Regex.{matches};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(matches("1234a", p1))
    };
    step() == Some(false)

/////////////////////////////////////////////////////////////////////////////
// contains                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def contains01(): Bool =
    use Option.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(contains("", p1))
    };
    step() == Some(false)

@test
def contains02(): Bool =
    use Option.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(contains("123", p1))
    };
    step() == Some(true)

@test
def contains03(): Bool =
    use Option.flatMap;
    use Text/Regex.{contains};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(contains("123abc", p1))
    };
    step() == Some(true)

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def startsWith01(): Bool =
    use Option.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(startsWith("", p1))
    };
    step() == Some(false)

@test
def startsWith02(): Bool =
    use Option.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(startsWith("123", p1))
    };
    step() == Some(true)

@test
def startsWith03(): Bool =
    use Option.flatMap;
    use Text/Regex.{startsWith};
    let step = () -> {
        let* p1 = Text/Regex/Pattern.compile("\d+");
        Some(startsWith("123abc", p1))
    };
    step() == Some(true)

/////////////////////////////////////////////////////////////////////////////
// evalMatches                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def evalMatches01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalMatches};
    let eval = Text/Regex/MatchEvaluator.getCaptureNamed("num");
    let step = () -> {
        let* p1 = Option.toOk(Text/Regex/Pattern.compile("(?<num>\d+)"), "Pattern.compile");
        evalMatches("123", p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// evalContains                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def evalContains01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalContains};
    let step = () -> {
        let* p1     = Option.toOk(Text/Regex/Pattern.compile("(?<num>\d+)"), "Pattern.compile");
        let eval    = Text/Regex/MatchEvaluator.getCaptureNamed("num");
        evalContains("aa 123 bb", p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// evalStartsWith                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def evalStartsWith01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{evalStartsWith};
    let step = () -> {
        let* p1     = Option.toOk(Text/Regex/Pattern.compile("(?<num>\d+)"), "Pattern.compile");
        let eval    = Text/Regex/MatchEvaluator.getCaptureNamed("num");
        evalStartsWith("123 bb", p1, eval)
    };
    step() == Ok("123")

/////////////////////////////////////////////////////////////////////////////
// results                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def results01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{results};
    use Text/Regex/MatchEvaluator.{content};
    let step = () -> {
        let* p1     = Option.toOk(Text/Regex/Pattern.compile("\p{Alpha}+"), "Pattern.compile");
        let strm    = results("aa bb cc", p1, content());
        Ok(strm)
    };
    Result.map(Data/Stream.toList, step()) == Ok("aa" :: "bb" :: "cc" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// contentResults                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def contentResults01(): Bool & Impure =
    use Option.flatMap;
    use Text/Regex.{contentResults};
    let step = () -> {
        let* p1     = Text/Regex/Pattern.compile("\p{Alpha}+");
        let strm    = contentResults("aa bb cc", p1);
        Some(strm)
    };
    Option.map(Data/Stream.toList, step()) == Some("aa" :: "bb" :: "cc" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// rangeResults                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def rangeResults01(): Bool & Impure =
    use Result.flatMap;
    use Text/Regex.{rangeResults};
    let step = () -> {
        let* p1     = Option.toOk(Text/Regex/Pattern.compile("\p{Alpha}+"), "Pattern.compile");
        let strm    = rangeResults("aa bb cc", p1);
        Ok(strm)
    };
    Result.map(Data/Stream.toList, step()) == Ok((0,2) :: (3,5) :: (6,8) :: Nil)
}