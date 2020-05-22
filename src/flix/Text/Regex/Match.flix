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

use System.Error.{IllegalStateException, NoSuchElementException};

use Text/Regex.{Match};
use Text/Regex.Match.{Match};

use Text/Regex.Pattern;
use Text/Regex.Pattern.{Pattern};
use Text/Regex.MatchEvaluator;
use Text/Regex.MatchEvaluator.{MatchEvaluator};

/// This is a simple "steppable" matcher like .Net `Regex.Match`.

namespace Text/Regex {
    
    pub opaque type Match = { primMatcher: Text/Regex/Internal.PrimMatcher, success: Ref[Bool] }

}

namespace Text/Regex/Match {

    pub def findMatch(pattern: Pattern, input: String): Match & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, find};
        let Pattern(p1) = pattern;
        let m1 = newPrimMatcher(p1, input);
        let sc = m1.find();
        Match({ primMatcher = m1, success = ref sc})

    /// Fails with NoSuchElementException of no match found.
    pub def tryFindMatch(pattern: Pattern, input: String): Result[Match, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, find};
        let Pattern(p1) = pattern;
        let m1 = newPrimMatcher(p1, input);
        let sc = m1.find();
        if (sc)
            Ok(Match({ primMatcher = m1, success = ref sc}))
        else
            Err(NoSuchElementException)

    pub def next(x: Match): Unit & Impure = 
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, find};
        let Match(m1) = x;
        let matcher = m1.primMatcher;
        let scref = m1.success;
        let sc = matcher.find();
        scref := sc;
        ()

    pub def success(x: Match): Bool & Impure = 
        let Match(m1) = x;
        let scref = m1.success;
        deref scref
    
    pub def content(x: Match): Result[String, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{group};
        let Match(m1) = x;
        let matcher = m1.primMatcher;
        let scref = m1.success;
        if (deref scref == true)
            matcher.group()
        else 
            Err(IllegalStateException)

    
    pub def evaluate(x: Match, eval: MatchEvaluator[a]): Result[a, System.Error] & Impure =
        let Match(m1) = x;
        let matcher = m1.primMatcher;
        let scref = m1.success;
        if (deref scref == true) {
            let MatchEvaluator(fn) = eval;
            matcher.fn()
        } else 
            Err(IllegalStateException)

}        



