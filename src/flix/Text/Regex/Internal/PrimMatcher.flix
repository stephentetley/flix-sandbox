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

use System.Error.{IllegalStateException, IllegalArgumentException, IndexOutOfBoundsException};

use Regex/Internal.{PrimMatcher, PrimMatchEvaluator};
use Regex/Internal.PrimMatcher.{PrimMatcher};

namespace Regex/Internal {

    ///
    /// Represents a mutable, matching engine for a Pattern.
    ///
    pub opaque type PrimMatcher = ##java.util.regex.Matcher

    ///
    /// Represents a match `evaluator` a function that generates a value from a match (typically a replacement String).
    ///
    type alias PrimMatchEvaluator[a] = PrimMatcher -> Result[a, System.Error] & Impure

}

namespace Regex/Internal/PrimMatcher {

    ///
    /// Create a matcher for Pattern `patt` on the source String `input`.
    ///
    pub def newPrimMatcher(patt: ##java.util.regex.Pattern, input: String): PrimMatcher & Impure =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence);
        let cs1 = input as ##java.lang.CharSequence;
        PrimMatcher(patt.matcher(cs1))

    pub def isMatch(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.matches();
        let PrimMatcher(x1) = x;
        x1.matches()

    pub def isSubmatch(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.find();
        let PrimMatcher(x1) = x;
        x1.find()

    /// No exception.
    pub def replaceAll(x: PrimMatcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceAll(String);
        let PrimMatcher(x1) = x;
        x1.replaceAll(rep)

    /// No exception.
    pub def replaceFirst(x: PrimMatcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceFirst(String);
        let PrimMatcher(x1) = x;
        x1.replaceFirst(rep)

    pub def find(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.find();
        let PrimMatcher(x1) = x;
        x1.find()

    pub def start(x: PrimMatcher): Result[Int32, System.Error] & Impure =
        import java.util.regex.Matcher.start();
        try {
            let PrimMatcher(x1) = x;
            Ok(x1.start())
        } catch {
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
        }

    pub def end(x: PrimMatcher): Result[Int32, System.Error] & Impure =
        import java.util.regex.Matcher.end();
        try {
            let PrimMatcher(x1) = x;
            Ok(x1.end())
        } catch {
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
        }

    ///
    pub def group(x: PrimMatcher): Result[String, System.Error] & Impure =
        import java.util.regex.Matcher.group();
        try { 
            let PrimMatcher(m1) = x;
            Ok(m1.group())
        } catch {
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
        }


    /// 
    /// Returns the number of capture groups in the pattern used by this PrimMatcher.
    ///
    /// This is not the number of groups matched, which should be all groups in a pattern
    /// or none.
    ///
    pub def groupCount(x: PrimMatcher): Int32 & Impure =
        import java.util.regex.Matcher.groupCount();
        let PrimMatcher(m1) = x;
        m1.groupCount()

    pub def groupElementByIndex(x: PrimMatcher, ix: Int32): Result[String, System.Error] & Impure =
        import java.util.regex.Matcher.group(Int32);
        try {
            let PrimMatcher(m1) = x;
            Ok(m1.group(ix))
        } catch {
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
            case _: ##java.lang.IndexOutOfBoundsException => Err(IndexOutOfBoundsException)
        }

    pub def groupElementByName(x: PrimMatcher, label: String): Result[String, System.Error] & Impure =
        import java.util.regex.Matcher.group(String);
        try {
            let PrimMatcher(m1) = x;
            Ok(m1.group(label))
        } catch {
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
            case _: ##java.lang.IllegalArgumentException => Err(IllegalArgumentException)
        }

    /// 
    pub def appendReplacement!(x: PrimMatcher, sb: StringBuilder, rep: String): Result[Unit, System.Error] & Impure =
        import java.util.regex.Matcher.appendReplacement(##java.lang.StringBuilder, String);    
        try {
            let PrimMatcher(x1) = x;
            let StringBuilder(sb1) = sb;
            let _ = x1.appendReplacement(sb1, rep);
            Ok()
        } catch { 
            case _: ##java.lang.IllegalStateException => Err(IllegalStateException) 
            case _: ##java.lang.IndexOutOfBoundsException => Err(IndexOutOfBoundsException)
            case _: ##java.lang.IllegalArgumentException => Err(IllegalArgumentException)
        }

    /// No exceptions.
    pub def appendTail!(x: PrimMatcher, sb: StringBuilder): Unit & Impure =
        import java.util.regex.Matcher.appendTail(##java.lang.StringBuilder);
        let PrimMatcher(x1) = x;
        let StringBuilder(sb1) = sb;
        x1.appendTail(sb1);
        ()

    /// We cannot wrap Java's `replaceAll​(Function<MatchResult,​String> replacer)` because we can't 
    /// marshal functions to Java, s owe remake it.
    pub def replaceAllWith(x: PrimMatcher, eval: PrimMatchEvaluator[String]): Result[String, System.Error] & Impure =
        use Result.flatMap;
        let sb = StringBuilder.new();
        let* _ = replaceAllWithHelper(x, eval, sb);
        Ok(StringBuilder.toString(sb))

    pub def replaceAllWithHelper(x: PrimMatcher, eval: PrimMatchEvaluator[String], sb: StringBuilder): Result[Unit, System.Error] & Impure =
        use Result.flatMap;
        if (x.find() == false) {
            let _ = x.appendTail!(sb);
            Ok()
        } else {
            let* rep = eval(x);
            let* _ = x.appendReplacement!(sb, rep);
            replaceAllWithHelper(x, eval, sb)
        }

    pub def replaceAllWithIndex(x: PrimMatcher, eval: Int32 -> PrimMatchEvaluator[String]): Result[String, System.Error] & Impure =
        use Result.flatMap;
        let sb = StringBuilder.new();
        let* _ = replaceAllWithIndexHelper(x, eval, sb, 0);
        Ok(StringBuilder.toString(sb))

    pub def replaceAllWithIndexHelper(x: PrimMatcher, eval: Int32 -> PrimMatchEvaluator[String], sb: StringBuilder, ix: Int32): Result[Unit, System.Error] & Impure =
        use Result.flatMap;
        if (x.find() == false) {
            let _ = x.appendTail!(sb);
            Ok()
        } else {
            let* rep = eval(ix,x);
            let* _ = x.appendReplacement!(sb, rep);
            replaceAllWithIndexHelper(x, eval, sb, ix+1)
        }



}