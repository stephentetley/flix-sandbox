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
            import java.nio.charset.Charset.toString() as toS;
            let Charset(cs1) = cs;
            toS(cs1) as & Pure
    }

    instance FromString[Charset] {
        pub def fromString(x: String): Option[Charset] =
            Text/Charset.forName(x)
    }

    instance Eq[Charset] {
        pub def eq(x: Charset, y: Charset): Bool = Text/Charset.equals(x, y)
    }

    instance Hash[Charset] {
        pub def hash(x: Charset): Int = Text/Charset.hashCode(x)
    }

    instance Order[Charset] {
        pub def compare(x: Charset, y: Charset): Comparison =
            Text/Charset.compareTo(x,y) |> Comparison.fromInt
    }
}

namespace Text/Charset {

    use Text.Charset;
    use Text.Charset.{Charset};

    pub def forName(name: String): Option[Charset] =
        import java.nio.charset.Charset:forName(String);
        try {
            Some(Charset(forName(name))) as & Pure
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def defaultCharset(): Charset & Impure =
        import java.nio.charset.Charset:defaultCharset();
        Charset(defaultCharset())


    pub def isSupported(name: String): Bool =
        import java.nio.charset.Charset:isSupported(String);
        try {
            isSupported(name) as & Pure
        } catch {
            case _: ##java.lang.Exception => false
        }

    pub def iso_8859_1(): Charset = 
        import get java.nio.charset.StandardCharsets:ISO_8859_1 as get_iso_8859_1;
        Charset(get_iso_8859_1()) as & Pure

    pub def us_ascii(): Charset = 
        import get java.nio.charset.StandardCharsets:US_ASCII as get_us_ascii;
        Charset(get_us_ascii()) as & Pure

    pub def utf_16(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16 as get_utf_16;
        Charset(get_utf_16()) as & Pure

    pub def utf_16be(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16BE as get_utf_16be;
        Charset(get_utf_16be()) as & Pure

    pub def utf_16le(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16LE as get_utf_16le;
        Charset(get_utf_16le()) as & Pure

    pub def utf_8(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_8 as get_utf_8;
        Charset(get_utf_8()) as & Pure

    pub def equals(x: Charset, y: Charset): Bool =
        import java.nio.charset.Charset.equals(##java.lang.Object);
        let Charset(x1) = x;
        let Charset(y1) = y;
        equals(x1, y1 as ##java.lang.Object) as & Pure

    pub def hashCode(cs: Charset): Int32 =
        import java.nio.charset.Charset.hashCode();
        let Charset(cs1) = cs;
        hashCode(cs1) as & Pure

    pub def compareTo(x: Charset, y: Charset): Int32 =
        import java.nio.charset.Charset.compareTo(##java.nio.charset.Charset);
        let Charset(x1) = x;
        let Charset(y1) = y;
        compareTo(x1, y1) as & Pure

    pub def name(cs: Charset): String =
        import java.nio.charset.Charset.name();
        let Charset(cs1) = cs;
        name(cs1) as & Pure

    pub def displayName(cs: Charset): String =
        import java.nio.charset.Charset.displayName();
        let Charset(cs1) = cs;
        displayName(cs1) as & Pure

    pub def displayNameWithLocale(cs: Charset, locale: I18n.Locale): String =
        use I18n.Locale.{Locale};
        import java.nio.charset.Charset.displayName(##java.util.Locale);
        let Charset(cs1) = cs;
        let Locale(locale1) = locale;
        displayName(cs1, locale1) as & Pure

    pub def isRegistered(cs: Charset): Bool =
        import java.nio.charset.Charset.isRegistered();
        let Charset(cs1) = cs;
        isRegistered(cs1) as & Pure

}
