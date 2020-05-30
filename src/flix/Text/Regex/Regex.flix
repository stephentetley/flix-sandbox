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
use System.Error.{IllegalStateException};
use Text/Regex.Pattern;
use Text/Regex.Pattern.Pattern;
use Text/Regex.MatchEvaluator;
use Text/Regex.MatchEvaluator.{MatchEvaluator};
use Text/Regex/Internal.PrimMatcher;

namespace Text/Regex {

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    /// Note - use `isSubmatch` to search for a substring.
    ///
    pub def isMatch(patt: Pattern, input: String): Bool =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, isMatch};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        m1.isMatch() as & Pure

    ///
    /// Returns `true` if the string `s` is matched by the regular expression `pattern` at any point.
    ///
    pub def isSubmatch(patt: Pattern, input: String): Bool =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, isSubmatch};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        m1.isSubmatch() as & Pure

    /// Whole input matches...
    pub def evaluateMatch(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, matches};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        if (m1.matches() == true) {
            let MatchEvaluator(fn) = eval;
            m1.fn()
        } else
            Err(IllegalStateException)

    /// Input matches at least once (left to right)...
    pub def evaluateSubmatch(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, isSubmatch};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        if (m1.isSubmatch() == true) {
            let MatchEvaluator(fn) = eval;
            m1.fn()
        } else
            Err(IllegalStateException)


    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(patt: Pattern, s: String): Array[String] & Impure =
        import java.util.regex.Pattern.split(##java.lang.CharSequence);
        let Pattern(p1) = patt;
        let cs1 = s as ##java.lang.CharSequence;
        p1.split(cs1)


    ///
    /// Returns `input` with every match of the pattern `patt` replaced by the string `rep`.
    ///
    pub def replace(patt: Pattern, rep: String, input: String): String =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceAll};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        m1.replaceAll(rep) as & Pure

    ///
    /// Returns `input` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirstMatch(patt: Pattern, rep: String, input: String): String =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceFirst};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        m1.replaceFirst(rep) as & Pure

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def replaceWith(patt: Pattern, eval: MatchEvaluator[String], input: String): Result[String, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceAllWith, group};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        let MatchEvaluator(rewrite) = eval;
        m1.replaceAllWith(rewrite)

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def replaceWithIndex(patt: Pattern, eval: Int32 -> MatchEvaluator[String], input: String): Result[String, System.Error] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceAllWithIndex, group};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        m1.replaceAllWithIndex(ix -> 
            let MatchEvaluator(rewrite) = eval(ix);
            rewrite
        )

}