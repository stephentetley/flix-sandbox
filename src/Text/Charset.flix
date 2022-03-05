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


namespace Text  {
    ///
    /// Represents a Charset.
    ///
    pub opaque type Charset = ##java.nio.charset.Charset

    instance ToString[Charset] {
        pub def toString(cs: Charset): String =
            import java.nio.charset.Charset.toString(): String & Pure;
            let Charset(cs1) = cs;
            toString(cs1)
    }

    instance FromString[Charset] {
        pub def fromString(x: String): Option[Charset] =
            Text/Charset.forName(x)
    }

    instance Eq[Charset] {
        pub def eq(x: Charset, y: Charset): Bool = Text/Charset.equals(x, y)
    }

    instance Hash[Charset] {
        pub def hash(x: Charset): Int32 = Text/Charset.hashCode(x)
    }

    instance Order[Charset] {
        pub def compare(x: Charset, y: Charset): Comparison =
            Text/Charset.compareTo(x, y) |> Comparison.fromInt32
    }

    instance Boxable[Charset]

}

namespace Text/Charset {

    use Text.Charset;
    use Text.Charset.{Charset};

    pub def forName(name: String): Option[Charset] =
        import static java.nio.charset.Charset.forName(String): ##java.nio.charset.Charset & Pure;
        try {
            Some(Charset(forName(name)))
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def defaultCharset(): Charset & Impure =
        import static java.nio.charset.Charset.defaultCharset(): ##java.nio.charset.Charset & Impure;
        Charset(defaultCharset())


    pub def isSupported(name: String): Bool =
        import static java.nio.charset.Charset.isSupported(String): Bool & Pure;
        try {
            isSupported(name)
        } catch {
            case _: ##java.lang.Exception => false
        }

    pub def iso_8859_1(): Charset =
        import static get java.nio.charset.StandardCharsets.ISO_8859_1: ##java.nio.charset.Charset & Pure as get_iso_8859_1;
        Charset(get_iso_8859_1())

    pub def us_ascii(): Charset =
        import static get java.nio.charset.StandardCharsets.US_ASCII: ##java.nio.charset.Charset & Pure as get_us_ascii;
        Charset(get_us_ascii())

    pub def utf_16(): Charset =
        import static get java.nio.charset.StandardCharsets.UTF_16: ##java.nio.charset.Charset & Pure as get_utf_16;
        Charset(get_utf_16())

    pub def utf_16be(): Charset =
        import static get java.nio.charset.StandardCharsets.UTF_16BE: ##java.nio.charset.Charset & Pure as get_utf_16be;
        Charset(get_utf_16be())

    pub def utf_16le(): Charset =
        import static get java.nio.charset.StandardCharsets.UTF_16LE: ##java.nio.charset.Charset & Pure as get_utf_16le;
        Charset(get_utf_16le())

    pub def utf_8(): Charset =
        import static get java.nio.charset.StandardCharsets.UTF_8: ##java.nio.charset.Charset & Pure as get_utf_8;
        Charset(get_utf_8())

    pub def equals(x: Charset, y: Charset): Bool =
        import java.nio.charset.Charset.equals(##java.lang.Object): Bool & Pure;
        let Charset(x1) = x;
        let Charset(y1) = y;
        equals(x1, y1 as ##java.lang.Object)

    pub def hashCode(cs: Charset): Int32 =
        import java.nio.charset.Charset.hashCode(): Int32 & Pure;
        let Charset(cs1) = cs;
        hashCode(cs1)

    pub def compareTo(x: Charset, y: Charset): Int32 =
        import java.nio.charset.Charset.compareTo(##java.nio.charset.Charset): Int32 & Pure;
        let Charset(x1) = x;
        let Charset(y1) = y;
        compareTo(x1, y1)

    pub def name(cs: Charset): String =
        import java.nio.charset.Charset.name(): String & Pure;
        let Charset(cs1) = cs;
        name(cs1)

    pub def displayName(cs: Charset): String =
        import java.nio.charset.Charset.displayName(): String & Pure;
        let Charset(cs1) = cs;
        displayName(cs1)

    pub def displayNameWithLocale(cs: Charset, locale: I18n.Locale): String =
        use I18n.Locale.{Locale};
        import java.nio.charset.Charset.displayName(##java.util.Locale): String & Pure;
        let Charset(cs1) = cs;
        let Locale(locale1) = locale;
        displayName(cs1, locale1)

    pub def isRegistered(cs: Charset): Bool =
        import java.nio.charset.Charset.isRegistered(): Bool & Pure;
        let Charset(cs1) = cs;
        isRegistered(cs1)

    pub def contains(x: Charset, y: Charset): Bool =
        import java.nio.charset.Charset.contains(##java.nio.charset.Charset): Bool & Pure;
        let Charset(x1) = x;
        let Charset(y1) = y;
        contains(x1, y1)

}
