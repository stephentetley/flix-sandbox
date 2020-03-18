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

///
/// Represents a Charset.
///
opaque type Charset = ##java.nio.charset.Charset

namespace Charset {

    pub def iso_8859_1(): Charset = 
        import get java.nio.charset.StandardCharsets:ISO_8859_1 as get_iso_8859_1;
        Charset(get_iso_8859_1() as & Pure)

    pub def us_ascii(): Charset = 
        import get java.nio.charset.StandardCharsets:US_ASCII as get_us_ascii;
        Charset(get_us_ascii() as & Pure)

    pub def utf_16(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16 as get_utf_16;
        Charset(get_utf_16() as & Pure)

    pub def utf_16be(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16BE as get_utf_16be;
        Charset(get_utf_16be() as & Pure)

    pub def utf_16le(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_16LE as get_utf_16le;
        Charset(get_utf_16le() as & Pure)

    pub def utf_8(): Charset = 
        import get java.nio.charset.StandardCharsets:UTF_8 as get_utf_8;
        Charset(get_utf_8() as & Pure)

    pub def getRepr(cs: Charset): ##java.nio.charset.Charset = 
        let Charset(cs1) = cs;
        cs1

    pub def toString(cs: Charset): String = 
        import java.nio.charset.Charset.toString() as toStr;
        let Charset(cs1) = cs;
        toStr(cs1) as & Pure
        

}