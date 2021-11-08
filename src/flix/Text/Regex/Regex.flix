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
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input) as & Pure;
        Text/Regex/Internal/Matcher.matches(m1) as & Pure

    ///
    /// Returns `true` if the string `input` is matched by the regular expression pattern `patt` at any point.
    ///
    pub def contains(input: String, patt: Pattern): Bool =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input) as & Pure;
        Text/Regex/Internal/Matcher.find(m1) as & Pure

    ///
    /// Returns `true` if the string `input` starts the regular expression pattern `patt`.
    ///
    pub def startsWith(input: String, patt: Pattern): Bool =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input) as & Pure;
        Text/Regex/Internal/Matcher.lookingAt(m1) as & Pure

    /// Design note - if a function uses a MatchEvaluator it should return `Result[a, String]` rather than `Option[a]`
    /// as MatchEvaluator has user supplied error messages.
    def evalWithTest(test: ##java.util.regex.Matcher ~> Bool,
                        patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1)
        } else
            Err("evalWithTest - no match")

    /// Design note - if a function uses a MatchEvaluator it should return `Result[a, String]` rather than `Option[a]`
    /// as MatchEvaluator has user supplied error messages.
    /// However "no match" is not always a error, this version uses an inner option to signal "match" / "not matched".
    def evalWithTestOption(test: ##java.util.regex.Matcher ~> Bool,
                            patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1) |> Result.map(Some)
        } else
            Ok(None)


    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalMatches(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalWithTest(Text/Regex/Internal/Matcher.matches, patt, eval, input)

    /// Whole input matches...
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    /// Returns `Ok(None)` on no match
    pub def evalMatchesOption(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalWithTestOption(Text/Regex/Internal/Matcher.matches, patt, eval, input)


    /// Input matches at least once (left to right)...
    ///
    /// The MatchEvaluator `eval` is potentially impure (e.g. it might create an array),
    /// so this function is impure.
    ///
    pub def evalContains(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalWithTest(Text/Regex/Internal/Matcher.find, patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def evalContainsOption(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalWithTestOption(Text/Regex/Internal/Matcher.find, patt, eval, input)

    /// Input matches at least once (left to right)...
    pub def evalStartsWith(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalWithTest(Text/Regex/Internal/Matcher.lookingAt, patt, eval, input)

    /// Returns `Ok(None)` on no match
    pub def evalStartsWithOption(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalWithTestOption(Text/Regex/Internal/Matcher.lookingAt, patt, eval, input)

    def boundedWithTest(test: ##java.util.regex.Matcher ~> Bool, start: Int32, end: Int32, patt: Pattern, input: String): Option[String] & Impure =
        use Option.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        let* _ = Result.toOption(Text/Regex/Internal/Matcher.region(m1, start, end));
        if (test(m1))
            Result.toOption(Text/Regex/Internal/Matcher.group(m1))
        else
            None

    /// TODO - should this be pure? (yes), return a Bool (?)...
    /// Need to investigate why they were written this way, before changing them.
    /// Match input within region, match must start at start of region...
    pub def boundedMatches(start: Int32, end: Int32, patt: Pattern, input: String): Option[String] & Impure =
        use Text/Regex/Internal/Matcher.{matches};
        boundedWithTest(matches, start, end, patt, input)

    /// Match input within region, match must start at start of region...
    pub def boundedContains(start: Int32, end: Int32, patt: Pattern, input: String): Option[String] & Impure =
        use Text/Regex/Internal/Matcher.{find};
        boundedWithTest(find, start, end, patt, input)


    /// Match input within region, match must start at start of region...
    pub def boundedStartsWith(start: Int32, end: Int32, patt: Pattern, input: String): Option[String] & Impure =
        use Text/Regex/Internal/Matcher.{lookingAt};
        boundedWithTest(lookingAt, start, end, patt, input)



    /// Whole input matches within region...
    def evalBoundedWithTest(test: ##java.util.regex.Matcher ~> Bool, start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        let* _ = Text/Regex/Internal/Matcher.region(m1, start, end);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1)
        } else
            Err("evalBoundedWithTest - no match")

    /// Whole input matches within region...
    /// Returns `Ok(None)` on no match
    def evalBoundedWithTestOption(test: ##java.util.regex.Matcher ~> Bool,
                                    start: Int32,
                                    end: Int32,
                                    patt: Pattern,
                                    eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        use Result.flatMap;
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        let* _ = Text/Regex/Internal/Matcher.region(m1, start, end);
        if (test(m1)) {
            let MatchEvaluator(fn) = eval;
            fn(m1) |> Result.map(Some)
        } else
            Ok(None)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedMatches(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalBoundedWithTest(Text/Regex/Internal/Matcher.matches, start, end, patt, eval, input)

    pub def evalBoundedMatchesOption(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalBoundedWithTestOption(Text/Regex/Internal/Matcher.matches, start, end, patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedContains(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalBoundedWithTest(Text/Regex/Internal/Matcher.find, start, end, patt, eval, input)

    pub def evalBoundedContainsOption(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalBoundedWithTestOption(Text/Regex/Internal/Matcher.find, start, end, patt, eval, input)

    /// Match input within region, match must start at start of region...
    pub def evalBoundedStartsWith(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[a, String] & Impure =
        evalBoundedWithTest(Text/Regex/Internal/Matcher.lookingAt, start, end, patt, eval, input)

    pub def evalBoundedStartsWithOption(start: Int32, end: Int32, patt: Pattern, eval: MatchEvaluator[a], input: String): Result[Option[a], String] & Impure =
        evalBoundedWithTestOption(Text/Regex/Internal/Matcher.lookingAt, start, end, patt, eval, input)


    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def split(patt: Pattern, input: String): Array[String] & Impure =
        import java.util.regex.Pattern.split(##java.lang.CharSequence);
        let Pattern(p1) = patt;
        let cs1 = input as ##java.lang.CharSequence;
        split(p1, cs1)

    ///
    /// Splits the string `s` around matches of the pattern `p`.
    ///
    pub def boundedSplit(start: Int32, end: Int32, patt: Pattern, input: String): Array[String] & Impure =
        let input1 = String.slice(start, end, input);
        split(patt, input1)


    ///
    /// TODO: use a "Stream" when stdlib has an implementation finalized
    ///
    pub def results(patt: Pattern, eval: MatchEvaluator[a], input: String): Result[List[a], String] & Impure =
        let Pattern(patt1) = patt;
        let MatchEvaluator(eval1) = eval;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input);
        let iter = _ -> match Text/Regex/Internal/Matcher.find(m1) {
            case true  => eval1(m1) |> Result.map(Some)
            case false => Ok(None)
        };
        List.unfoldWithOkIter(iter)

    ///
    /// Stream...
    ///
    pub def contentResults(patt: Pattern, input: String): Result[List[String], String] & Impure =
        use Text/Regex/MatchEvaluator.{content};
        results(patt, content(), input)

    ///
    /// Stream...
    ///
    pub def rangeResults(patt: Pattern, input: String): Result[List[(Int32, Int32)], String] & Impure =
        use Text/Regex/MatchEvaluator.{getRange};
        results(patt, getRange(), input)

    ///
    /// Returns `input` with every match of the pattern `patt` replaced by the string `rep`.
    ///
    pub def replace(patt: Pattern, rep: String, input: String): String =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input) as & Pure;
        Text/Regex/Internal/Matcher.replaceAll(m1, rep) as & Pure

    ///
    /// Returns `input` with the first match of the regular expression `patt` replaced by the string `rep`.
    ///
    pub def replaceFirstMatch(patt: Pattern, rep: String, input: String): String =
        let Pattern(patt1) = patt;
        let m1 = Text/Regex/Internal/Matcher.new(patt1, input) as & Pure;
        Text/Regex/Internal/Matcher.replaceFirst(m1, rep) as & Pure

    ///
    /// Rewrite the string `input` by applying the function `f` to the
    /// substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWith(patt: Pattern, eval: MatchEvaluator[String], input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAll!};
        execReplacer(evalRewriteAll!(eval), patt, input)

    ///
    /// Rewrite the string `input` by apply the function `f` to the substring produced by each match of the pattern `patt`.
    ///
    pub def rewriteWithIndex(patt: Pattern, eval: Int32 -> MatchEvaluator[String], input: String): Result[String, String] & Impure =
        use Text/Regex/Replacer.{execReplacer, evalRewriteAllWithIndex!};
        execReplacer(evalRewriteAllWithIndex!(eval), patt, input)

    /// Regex should provide Regex version of common String functions...

    ///
    /// Count the occurences of `patt` in string `s`.
    ///
    pub def countMatches(patt: Pattern, s: String): Int32 =
        Text/Regex/Matcher.runMatcher(countMatcher(0), patt, s) as & Pure
            |> flip(Result.getWithDefault, 0)

    def countMatcher(n: Int32): Matcher[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find, point};
        let* first = find();
        if (first) countMatcher(n+1) else point(n)


    ///
    /// Return the index of the first occurence of `patt` in `s` from the left.
    ///
    /// If `patt` is not present in `s` return None.
    ///
    /// If `patt` is the empty string return None.
    ///
    pub def indexOfLeft(patt: Pattern, s: String): Option[Int32] = indexOfLeftHelper(patt, s) as & Pure

    def indexOfLeftHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find, start, throwError};
        let m1 = {
            let* bfound = find();
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    pub def indexOfRight(patt: Pattern, s: String): Option[Int32] = indexOfRightHelper(patt, s) as & Pure

    def indexOfRightHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findLast, start, throwError};
        let m1 = {
            let* bfound = findLast();
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption


    pub def indexOfEndLeft(patt: Pattern, s: String): Option[Int32] = indexOfEndLeftHelper(patt, s) as & Pure

    def indexOfEndLeftHelper(patt: Pattern, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, find, end, throwError};
        let m1 = {
            let* bfound = find();
            if (bfound) end() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption


    ///
    /// This is `indexOfLeft` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def indexOfLeftWithOffset(patt: Pattern, offset: Int32, s: String): Option[Int32] =
        indexOfLeftWithOffsetHelper(patt, offset, s) as & Pure

    def indexOfLeftWithOffsetHelper(patt: Pattern, offset: Int32, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findFrom, start, throwError};
        let m1 = {
            let* bfound = findFrom(offset);
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// This is `indexOfRight` with a start offset.
    ///
    /// Returns `None` if `patt` does not match.
    ///
    pub def indexOfRightWithOffset(patt: Pattern, offset: Int32, s: String): Option[Int32] =
        indexOfRightWithOffsetHelper(patt, offset, s) as & Pure

    def indexOfRightWithOffsetHelper(patt: Pattern, offset: Int32, s: String): Option[Int32] & Impure =
        use Text/Regex/Matcher.{flatMap, findLastFrom, start, throwError};
        let m1 = {
            let* bfound = findLastFrom(offset);
            if (bfound) start() else throwError("none")
        };
        Text/Regex/Matcher.runMatcher(m1, patt, s) |> Result.toOption

    ///
    /// Find the first instance of `patt` in string `s`, return a pair of the
    /// prefix of string `s` up to `sub` and the rest of string `s` including `sub`.
    ///
    pub def breakOnLeft(patt: Pattern, s: String): (String, String) =
        match indexOfLeft(patt, s) {
            case None => (s, "")
            case Some(i) => (String.sliceLeft(i, s), String.sliceRight(i, s))
        }

    pub def breakAfterLeft(patt: Pattern, s: String): (String, String) =
        match indexOfEndLeft(patt, s) {
            case None => (s, "")
            case Some(i) => (String.sliceLeft(i, s), String.sliceRight(i, s))
        }

}
