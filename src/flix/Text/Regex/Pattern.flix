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

    ///
    /// Represents a compiled Pattern.
    ///
    pub opaque type Pattern = ##java.util.regex.Pattern

    instance ToString[Pattern] {
        ///
        /// Return the string representation of the Pattern `patt`.
        ///
        pub def toString(patt: Pattern): String =
            import java.util.regex.Pattern.toString();
            let Pattern(p1) = patt;
            toString(p1) as & Pure
    }

    instance FromString[Pattern] {
        ///
        /// Return the string representation of the Pattern `patt`.
        ///
        pub def fromString(regex: String): Option[Pattern] =
            import java.util.regex.Pattern:compile(String);
            try {
                Some(Pattern(compile(regex))) as & Pure
            } catch {
                case _: ##java.util.regex.PatternSyntaxException => None
            }
    }

}

namespace Text/Regex/Pattern {

    use Text/Regex.Pattern;
    use Text/Regex.Pattern.{Pattern};

    pub enum Flag {
        case CanonEq,
        case CaseInsenstive,
        case Comments,
        case Dotall,
        case Literal,
        case Multiline,
        case UnicodeCase,
        case UnicodeCharacterClass,
        case UnixLines
    }

    instance ToString[Flag] {
        pub def toString(x: Flag): String = match x {
            case CanonEq => "CanonEq"
            case CaseInsenstive => "CaseInsenstive"
            case Comments => "Comments"
            case Dotall => "Dotall"
            case Literal => "Literal"
            case Multiline => "Multiline"
            case UnicodeCase => "UnicodeCase"
            case UnicodeCharacterClass => "UnicodeCharacterClass"
            case UnixLines => "UnixLines"
        }
    }

    instance Eq[Flag] {
        pub def eq(x: Flag, y: Flag): Bool = match (x, y) {
            case (CanonEq, CanonEq) => true
            case (CaseInsenstive, CaseInsenstive) => true
            case (Comments, Comments) => true
            case (Dotall, Dotall) => true
            case (Literal, Literal) => true
            case (Multiline, Multiline) => true
            case (UnicodeCase, UnicodeCase) => true
            case (UnicodeCharacterClass, UnicodeCharacterClass) => true
            case (UnixLines, UnixLines) => true
            case (_, _) => false
        }
    }

    def toInt(flag: Flag): Int32 =
        import get java.util.regex.Pattern:CANON_EQ as get_CANON_EQ;
        import get java.util.regex.Pattern:CASE_INSENSITIVE as get_CASE_INSENSITIVE;
        import get java.util.regex.Pattern:COMMENTS as get_COMMENTS;
        import get java.util.regex.Pattern:DOTALL as get_DOTALL;
        import get java.util.regex.Pattern:LITERAL as get_LITERAL;
        import get java.util.regex.Pattern:MULTILINE as get_MULTILINE;
        import get java.util.regex.Pattern:UNICODE_CASE as get_UNICODE_CASE;
        import get java.util.regex.Pattern:UNICODE_CHARACTER_CLASS as get_UNICODE_CHARACTER_CLASS;
        import get java.util.regex.Pattern:UNIX_LINES as get_UNIX_LINES;
        match flag {
            case CanonEq                => get_CANON_EQ()
            case CaseInsenstive         => get_CASE_INSENSITIVE()
            case Comments               => get_COMMENTS()
            case Dotall                 => get_DOTALL()
            case Literal                => get_LITERAL()
            case Multiline              => get_MULTILINE()
            case UnicodeCase            => get_UNICODE_CASE()
            case UnicodeCharacterClass  => get_UNICODE_CHARACTER_CLASS()
            case UnixLines              => get_UNIX_LINES()
        } as & Pure

    def sumFlags(flags: List[Flag]): Int32 =
        List.foldLeft((ac,e) -> ac + toInt(e), 0, flags)

    def listFlags(x: Int32): List[Flag] =
        let check = y -> {
            let y1 = toInt(y);
            x &&& y1 == y1
        };
        List.filter(check, CanonEq :: CaseInsenstive :: Comments :: Dotall :: Literal ::
                            Multiline :: UnicodeCase :: UnicodeCharacterClass :: UnixLines :: Nil)

    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compile(regex: String): Result[Pattern, String] =
        import java.util.regex.Pattern:compile(String);
        try {
            Ok(Pattern(compile(regex))) as & Pure
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => Err("Text/Regex/Pattern.compile failed - '${regex}'")
        }



    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compileWithFlags(regex: String, flags: List[Flag]): Result[Pattern, String] =
        import java.util.regex.Pattern:compile(String, Int32);
        try {
            Ok(Pattern(compile(regex, sumFlags(flags)))) as & Pure
        } catch {
            case _: ##java.lang.Exception => Err("Text/Regex/Pattern.compileWithFlags failed - '${regex}', ${flags}")
        }

    ///
    /// Irrefutabily compile the regular expression `regex` into a Pattern.
    /// If compilation fails the pattern will be substituted by the "unmatchable" pattern.
    ///
    /// Use this only in circumstances where you know the `regex` is a valid pattern
    /// and you need to avoid wrapping it in an `Ok`.
    ///
    pub def irrefutablePattern(regex: String): Pattern =
        import java.util.regex.Pattern:compile(String);
        try {
            Pattern(compile(regex)) as & Pure
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => unmatchable()
        }

    ///
    /// Return the unmatchable pattern - a pattern that will not match any input.
    ///
    pub def unmatchable(): Pattern =
        import java.util.regex.Pattern:compile(String);
        try {
            Pattern(compile("^\\b$")) as & Pure
        } catch {
            case _: ##java.util.regex.PatternSyntaxException => unreachable!()
        }


    pub def quote(s: String): String =
        import java.util.regex.Pattern:quote(String);
        quote(s) as & Pure




    ///
    /// Return the regular expression used to build this pattern.
    ///
    pub def getPattern(patt: Pattern): String =
        import java.util.regex.Pattern.pattern();
        let Pattern(p1) = patt;
        pattern(p1) as & Pure

    ///
    /// Return the flags used to build this pattern.
    ///
    pub def getFlags(patt: Pattern): List[Flag] =
        import java.util.regex.Pattern.flags();
        let Pattern(p1) = patt;
        let i = flags(p1) as & Pure;
        listFlags(i)

}
