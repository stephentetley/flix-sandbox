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

namespace Text/Regex {

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.Pattern;
    use Text/Regex.MatchEvaluator;
    use Text/Regex.MatchEvaluator.{MatchEvaluator};
    use Text/Regex/Internal.PrimMatcher;

    ///
    /// Returns `true` if the entire string `input` is matched by the pattern `patt`.
    ///
    /// Note - use `contains` to search for a substring.
    ///
    pub def matches(input: String, patt: Pattern): Bool =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, matches};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        matches(m1) as & Pure

    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `patt` at any point.
    ///
    pub def contains(input: String, patt: Pattern): Bool =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, find};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        find(m1) as & Pure

    ///
    /// Returns `true` if the string `input` starts the regular expression pattern `patt`.
    ///
    pub def startsWith(input: String, patt: Pattern): Bool =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, lookingAt};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        lookingAt(m1) as & Pure

    /// Design note - if a function uses a MatchEvaluator it should return `Result[a, String]` rather than `Option[a]`
    /// as MatchEvaluator has user supplied error messages.
    def evalWithTest(test: PrimMatcher ~> Bool, input: String, patt: Pattern, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1)
        } else
            Err("evalWithTest")


    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalMatches(input: String, patt: Pattern, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{matches};
        evalWithTest(matches, input, patt, eval)


    /// Input matches at least once (left to right)...
    ///
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalContains(input: String, patt: Pattern, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{find};
        evalWithTest(find, input, patt, eval)

    /// Input matches at least once (left to right)...
    pub def evalStartsWith(input: String, patt: Pattern, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{lookingAt};
        evalWithTest(lookingAt, input, patt, eval)

    def boundedWithTest(test: PrimMatcher ~> Bool, input: String, patt: Pattern, start: Int32, end: Int32): Option[String] & Impure =
        use Option.flatMap;
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, region, group};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        let* _ = region(m1, start, end);
        if (test(m1))
            group(m1)
        else
            None

    /// TODO - should this be pure (yes), return a Bool (?)...
    /// Need to investigate why they were written this way, before changing them.
    /// Match input within region, match must start at start of region...
    pub def boundedMatches(input: String, patt: Pattern, start: Int32, end: Int32): Option[String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{matches};
        boundedWithTest(matches, input, patt, start, end)

    /// Match input within region, match must start at start of region...
    pub def boundedContains(input: String, patt: Pattern, start: Int32, end: Int32): Option[String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{find};
        boundedWithTest(find, input, patt, start, end)


    /// Match input within region, match must start at start of region...
    pub def boundedStartsWith(input: String, patt: Pattern, start: Int32, end: Int32): Option[String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{lookingAt};
        boundedWithTest(lookingAt, input, patt, start, end)



    /// Whole input matches within region...
    def evalBoundedWithTest(test: PrimMatcher ~> Bool, input: String, patt: Pattern, start: Int32, end: Int32, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Result.flatMap;
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, region};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input);
        let* _ = Option.toOk(region(m1, start, end), "evalBoundedWithTest - region");
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1)
        } else
            Err("evalBoundedWithTest - test")

    /// Match input within region, match must start at start of region...
    pub def evalBoundedMatches(input: String, patt: Pattern, start: Int32, end: Int32, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{matches};
        evalBoundedWithTest(matches, input, patt, start, end, eval)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedContains(input: String, patt: Pattern, start: Int32, end: Int32, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{find};
        evalBoundedWithTest(find, input, patt, start, end, eval)


    /// Match input within region, match must start at start of region...
    pub def evalBoundedStartsWith(input: String, patt: Pattern, start: Int32, end: Int32, eval: MatchEvaluator[a]): Result[a, String] & Impure =
        use Text/Regex/Internal/PrimMatcher.{lookingAt};
        evalBoundedWithTest(lookingAt, input, patt, start, end, eval)


    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(input: String, patt: Pattern): Array[String] & Impure =
        import java.util.regex.Pattern.split(##java.lang.CharSequence);
        let Pattern(p1) = patt;
        let cs1 = input as ##java.lang.CharSequence;
        split(p1, cs1)

    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def boundedSplit(input: String, patt: Pattern, start: Int32, end: Int32): Array[String] & Impure =
        let input1 = String.slice(input, start, end);
        split(input1, patt)


    ///
    /// Stream with a Iterator...
    ///
    pub def results(input: String, patt: Pattern, eval: MatchEvaluator[a]): Iterator[Result[a, String]] & Impure =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, find};
        let Pattern(p1) = patt;
        let MatchEvaluator(fn1) = eval;
        let m1 = newPrimMatcher(p1, input);
        let iterDone = () -> find(m1);
        let iterNext = () -> {
            try {
                fn1(m1)
            } catch {
                case _: ##java.lang.Exception => Err("contentsAllIter[next]")
            }
        };
        Iterator(iterDone, iterNext)



    ///
    /// Stream...
    ///
    pub def contentResults(input: String, patt: Pattern): Iterator[Result[String, String]] & Impure =
        use Text/Regex/MatchEvaluator.{content};
        results(input, patt, content())

    ///
    /// Stream...
    ///
    pub def rangeResults(input: String, patt: Pattern): Iterator[Result[(Int32, Int32), String]] & Impure =
        use Text/Regex/MatchEvaluator.{getRange};
        results(input, patt, getRange())

    ///
    /// Returns `input` with every match of the pattern `patt` replaced by the string `rep`.
    ///
    pub def replace(input: String, patt: Pattern, rep: String): String =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceAll};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        replaceAll(m1, rep) as & Pure

    ///
    /// Returns `input` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirstMatch(input: String, patt: Pattern, rep: String): String =
        use Text/Regex/Internal/PrimMatcher.{newPrimMatcher, replaceFirst};
        let Pattern(p1) = patt;
        let m1 = newPrimMatcher(p1, input) as & Pure;
        replaceFirst(m1, rep) as & Pure

    ///
    /// Rewrite the string `input` by applying the function `f` to the
    /// substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWith(input: String, patt: Pattern, eval: MatchEvaluator[String]): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAll!};
        execReplacer(evalRewriteAll!(eval), patt, input)

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWithIndex(input: String, patt: Pattern, eval: Int32 -> MatchEvaluator[String]): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAllWithIndex!};
        execReplacer(evalRewriteAllWithIndex!(eval), patt, input)


}