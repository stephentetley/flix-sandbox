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



namespace Text/Regex/Internal {


    ///
    /// Represents a mutable, matching engine for a Pattern.
    ///
    pub opaque type PrimMatcher = ##java.util.regex.Matcher

    ///
    /// Represents a match `evaluator` a function that generates a value from a match (typically a replacement String).
    ///
    type alias PrimMatchEvaluator[a] = ##java.util.regex.Matcher ~> Result[a, String]

}

namespace Text/Regex/Internal/PrimMatcher {


    use Text/Regex/Internal.{PrimMatcher, PrimMatchEvaluator};
    use Text/Regex/Internal.PrimMatcher.{PrimMatcher};

    ///
    /// Create a matcher for Pattern `patt` on the source String `input`.
    ///
    pub def newPrimMatcher(patt: ##java.util.regex.Pattern, input: String): PrimMatcher & Impure =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence);
        let cs1 = input as ##java.lang.CharSequence;
        PrimMatcher(matcher(patt, cs1))

    pub def matches(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.matches();
        let PrimMatcher(x1) = x;
        matches(x1)

    pub def lookingAt(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.lookingAt();
        let PrimMatcher(x1) = x;
        lookingAt(x1)

    pub def find(x: PrimMatcher): Bool & Impure =
        import java.util.regex.Matcher.find();
        let PrimMatcher(x1) = x;
        find(x1)

    pub def findFrom(x: PrimMatcher, start: Int32): Bool & Impure =
        import java.util.regex.Matcher.find(Int32);
        let PrimMatcher(x1) = x;
        find(x1, start)


    /// No exception.
    pub def replaceAll(x: PrimMatcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceAll(String);
        let PrimMatcher(x1) = x;
        replaceAll(x1, rep)

    /// No exception.
    pub def replaceFirst(x: PrimMatcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceFirst(String);
        let PrimMatcher(x1) = x;
        replaceFirst(x1, rep)

    pub def reset(x: PrimMatcher): Unit & Impure =
        import java.util.regex.Matcher.reset();
        let PrimMatcher(x1) = x;
        reset(x1);
        ()

    pub def resetInput(x: PrimMatcher, input: String): Unit & Impure =
        import java.util.regex.Matcher.reset(##java.lang.CharSequence);
        let PrimMatcher(x1) = x;
        let cs = input as ##java.lang.CharSequence;
        reset(x1, cs);
        ()

    pub def usePattern(x: PrimMatcher, patt: ##java.util.regex.Pattern): Unit & Impure =
        import java.util.regex.Matcher.usePattern(##java.util.regex.Pattern);
        let PrimMatcher(x1) = x;
        usePattern(x1, patt);
        ()

    pub def region(x: PrimMatcher, start: Int32, end: Int32): Option[Unit] & Impure =
        import java.util.regex.Matcher.region(Int32, Int32);
        try {
            let PrimMatcher(x1) = x;
            region(x1, start, end);
            Some()
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def start(x: PrimMatcher): Option[Int32] & Impure =
        import java.util.regex.Matcher.start();
        try {
            let PrimMatcher(x1) = x;
            Some(start(x1))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def end(x: PrimMatcher): Option[Int32] & Impure =
        import java.util.regex.Matcher.end();
        try {
            let PrimMatcher(x1) = x;
            Some(end(x1))
        } catch {
            case _: ##java.lang.Exception => None
        }

    ///
    pub def group(x: PrimMatcher): Option[String] & Impure =
        import java.util.regex.Matcher.group();
        try { 
            let PrimMatcher(x1) = x;
            Some(group(x1))
        } catch {
            case _: ##java.lang.Exception => None
        }


    /// 
    /// Returns the number of capture groups in the pattern used by this PrimMatcher.
    ///
    /// This is not the number of groups matched, which should be all groups in a pattern
    /// or none.
    ///
    pub def groupCount(x: PrimMatcher): Int32 & Impure =
        import java.util.regex.Matcher.groupCount();
        let PrimMatcher(x1) = x;
        groupCount(x1)

    /// This follows the Java convention where index 0 matches everything.
    pub def groupElementAt(x: PrimMatcher, ix: Int32): Option[String] & Impure =
        import java.util.regex.Matcher.group(Int32);
        try {
            let PrimMatcher(x1) = x;
            Some(group(x1, ix))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def groupElementNamed(x: PrimMatcher, label: String): Option[String] & Impure =
        import java.util.regex.Matcher.group(String);
        try {
            let PrimMatcher(x1) = x;
            Some(group(x1, label))
        } catch {
            case _: ##java.lang.Exception => None
        }

    /// This follows the Java convention where index 0 matches everything.
    pub def groupStartAt(x: PrimMatcher, ix: Int32): Option[Int32] & Impure =
        import java.util.regex.Matcher.start(Int32);
        try {
            let PrimMatcher(x1) = x;
            Some(start(x1, ix))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def groupStartNamed(x: PrimMatcher, label: String): Option[Int32] & Impure =
        import java.util.regex.Matcher.start(String);
        try {
            let PrimMatcher(x1) = x;
            Some(start(x1, label))
        } catch {
            case _: ##java.lang.Exception => None
        }

    /// This follows the Java convention where index 0 matches everything.
    pub def groupEndAt(x: PrimMatcher, ix: Int32): Option[Int32] & Impure =
        import java.util.regex.Matcher.end(Int32);
        try {
            let PrimMatcher(x1) = x;
            Some(end(x1, ix))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def groupEndNamed(x: PrimMatcher, label: String): Option[Int32] & Impure =
        import java.util.regex.Matcher.end(String);
        try {
            let PrimMatcher(x1) = x;
            Some(end(x1, label))
        } catch {
            case _: ##java.lang.Exception => None
        }

    /// 
    pub def appendReplacement(x: PrimMatcher, sb: StringBuilder, rep: String): Option[Unit] & Impure =
        import java.util.regex.Matcher.appendReplacement(##java.lang.StringBuilder, String);    
        try {
            let PrimMatcher(x1) = x;
            let StringBuilder(sb1) = sb;
            let _ = appendReplacement(x1, sb1, rep);
            Some()
        } catch { 
            case _: ##java.lang.Exception => None
        }

    /// No exceptions.
    pub def appendTail(x: PrimMatcher, sb: StringBuilder): Unit & Impure =
        import java.util.regex.Matcher.appendTail(##java.lang.StringBuilder);
        let PrimMatcher(x1) = x;
        let StringBuilder(sb1) = sb;
        x1.appendTail(sb1);
        ()




}