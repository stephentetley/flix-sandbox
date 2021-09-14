/*
 * Copyright 2021 Stephen Tetley
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

/// To replace Internal/PrimMatcher...

namespace Text/Regex/Internal/Matcher {


    ///
    /// Create a matcher for Pattern `patt` on the source String `input`.
    ///
    pub def new(patt: ##java.util.regex.Pattern, input: String): ##java.util.regex.Matcher & Impure =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence);
        let cs1 = input as ##java.lang.CharSequence;
        matcher(patt, cs1)

    pub def matches(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.matches();
        matches(x)

    pub def lookingAt(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.lookingAt();
        lookingAt(x)

    pub def find(x: ##java.util.regex.Matcher): Bool & Impure =
        import java.util.regex.Matcher.find();
        find(x)

    pub def findFrom(x: ##java.util.regex.Matcher, start: Int32): Result[Bool, String] & Impure =
        import java.util.regex.Matcher.find(Int32);
        try {
            Ok(find(x, start))
        } catch {
            case _: ##java.lang.Exception => Err("findFrom")
        }

    pub def replaceAll(x: ##java.util.regex.Matcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceAll(String);
        replaceAll(x, rep)

    pub def replaceFirst(x: ##java.util.regex.Matcher, rep: String): String & Impure =
        import java.util.regex.Matcher.replaceFirst(String);
        replaceFirst(x, rep)

    /// Note Java's reset imperatively resets `x` and returns it.
    /// This is idiomatically functional, so we imperatively reset `x` and return Unit.
    pub def reset(x: ##java.util.regex.Matcher): Unit & Impure =
        import java.util.regex.Matcher.reset();
        reset(x);
        ()

    pub def resetInput(x: ##java.util.regex.Matcher, input: String): Unit & Impure =
        import java.util.regex.Matcher.reset(##java.lang.CharSequence);
        let cs = input as ##java.lang.CharSequence;
        reset(x, cs);
        ()

    pub def usePattern(x: ##java.util.regex.Matcher, patt: ##java.util.regex.Pattern): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.usePattern(##java.util.regex.Pattern);
        try {
            usePattern(x, patt);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("usePattern")
        }

    pub def region(x: ##java.util.regex.Matcher, start: Int32, end: Int32): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.region(Int32, Int32);
        try {
            region(x, start, end);
            Ok()
        } catch {
            case _: ##java.lang.Exception => Err("region")
        }

    pub def start(x: ##java.util.regex.Matcher): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start();
        try {
            Ok(start(x))
        } catch {
            case _: ##java.lang.Exception => Err("start")
        }

    pub def end(x: ##java.util.regex.Matcher): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end();
        try {
            Ok(end(x))
        } catch {
            case _: ##java.lang.Exception => Err("end")
        }

    ///
    pub def group(x: ##java.util.regex.Matcher): Result[String, String] & Impure =
        import java.util.regex.Matcher.group();
        try {
            Ok(group(x))
        } catch {
            case _: ##java.lang.Exception => Err("group")
        }


    /// 
    /// Returns the number of capture groups in the pattern used by this PrimMatcher.
    ///
    /// This is not the number of groups matched, which should be all groups in a pattern
    /// or none.
    ///
    pub def groupCount(x: ##java.util.regex.Matcher): Int32 & Impure =
        import java.util.regex.Matcher.groupCount();
        groupCount(x)

    /// This follows the Java convention where index 0 matches everything.
    pub def groupWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[String, String] & Impure =
        import java.util.regex.Matcher.group(Int32);
        try {
            Ok(group(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("groupWithIndex - ${ix}")
        }

    pub def groupWithLabel(x: ##java.util.regex.Matcher, label: String): Result[String, String] & Impure =
        import java.util.regex.Matcher.group(String);
        try {
            Ok(group(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("groupWithLabel - '${label}'")
        }

    /// This follows the Java convention where index 0 matches everything.
    pub def startWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start(Int32);
        try {
            Ok(start(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("startWithIndex")
        }

    pub def startWithLabel(x: ##java.util.regex.Matcher, label: String): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.start(String);
        try {
            Ok(start(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("startWithLabel")
        }

    /// This follows the Java convention where index 0 matches everything.
    pub def endWithIndex(x: ##java.util.regex.Matcher, ix: Int32): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end(Int32);
        try {
            Ok(end(x, ix))
        } catch {
            case _: ##java.lang.Exception => Err("endWithIndex")
        }

    pub def endWithLabel(x: ##java.util.regex.Matcher, label: String): Result[Int32, String] & Impure =
        import java.util.regex.Matcher.end(String);
        try {
            Ok(end(x, label))
        } catch {
            case _: ##java.lang.Exception => Err("endWithLabel")
        }

    /// 
    pub def appendReplacement(x: ##java.util.regex.Matcher, sb: StringBuilder, rep: String): Result[Unit, String] & Impure =
        import java.util.regex.Matcher.appendReplacement(##java.lang.StringBuilder, String);    
        try {
            let StringBuilder(sb1) = sb;
            let _ = appendReplacement(x, sb1, rep);
            Ok()
        } catch { 
            case _: ##java.lang.Exception => Err("appendReplacement")
        }


    pub def appendTail(x: ##java.util.regex.Matcher, sb: StringBuilder): Unit & Impure =
        import java.util.regex.Matcher.appendTail(##java.lang.StringBuilder);
        let StringBuilder(sb1) = sb;
        appendTail(x, sb1);
        ()

}
