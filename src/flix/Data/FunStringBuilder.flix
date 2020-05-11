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
/// Represents a functional StringBuilder.
///
pub enum FunStringBuilder {
    case Empty,
    case Text(String),
    case Char(Char),
    case Join(FunStringBuilder, FunStringBuilder)
}

namespace FunStringBuilder {


    ///
    /// Returns an empty FunStringBuilder.
    ///    
    pub def new(): FunStringBuilder = Empty

    ///
    /// Returns `sb2` appended to `sb1`.
    ///    
    pub def append(sb1: FunStringBuilder, sb2: FunStringBuilder): FunStringBuilder = match (sb1, sb2) {
        case (Empty, _) => sb2
        case (_, Empty) => sb1
        case (_, _) => Join(sb1,sb2)
    }

    ///
    /// Returns `sb` with the string `s` appended.
    ///    
    pub def appendString(sb: FunStringBuilder, s: String): FunStringBuilder = append(sb, Text(s))

    ///
    /// Returns `sb` with the char `c` appended.
    ///    
    pub def appendChar(sb: FunStringBuilder, c: Char): FunStringBuilder = append(sb, Char(c))


    ///
    /// Returns `sb` with the Int8 `i` appended.
    ///
    pub def appendInt8(sb: FunStringBuilder, i: Int8): FunStringBuilder =
        sb.appendString(Int8.toString(i))
        
    ///
    /// Returns `sb` with the Int16 `i` appended.
    ///
    pub def appendInt16(sb: FunStringBuilder, i: Int16): FunStringBuilder =
        sb.appendString(Int16.toString(i))
    
    ///
    /// Returns `sb` with the Int32 `i` appended.
    ///
    pub def appendInt32(sb: FunStringBuilder, i: Int32): FunStringBuilder =
        sb.appendString(Int32.toString(i))

    ///
    /// Returns `sb` with the Int64 `i` appended.
    ///
    pub def appendInt64(sb: FunStringBuilder, i: Int64): FunStringBuilder =
        sb.appendString(Int64.toString(i))

    ///
    /// Returns `sb` with the BigInt `i` appended.
    ///
    pub def appendBigInt(sb: FunStringBuilder, i: BigInt): FunStringBuilder =
        sb.appendString(BigInt.toString(i))

    ///
    /// Returns `sb` with the Float32 `d` appended.
    ///
    pub def appendFloat32(sb: FunStringBuilder, d: Float32): FunStringBuilder =
        sb.appendString(Float32.toString(d))

    ///
    /// Returns `sb` with the Float64 `d` appended.
    ///
    pub def appendFloat64(sb: FunStringBuilder, d: Float64): FunStringBuilder =
        sb.appendString(Float64.toString(d))

    ///
    /// Returns `sb` with the system line separator append.
    ///
    pub def appendLineSeparator(sb: FunStringBuilder): FunStringBuilder =
        sb.appendString(String.lineSeparator())

    ///
    /// Returns `sb` with the string `s` appended followed by the system line separator.
    ///
    pub def appendLine(sb: FunStringBuilder, s: String): FunStringBuilder =
        sb.appendString(s).appendLineSeparator()

    ///
    /// Returns `sb` with the string produced by `f(x)` appended.
    ///
    pub def appendLineWith(sb: FunStringBuilder, f: a -> String, x: a): FunStringBuilder =
        sb.appendString(f(x)).appendLineSeparator()

    ///
    /// Returns `sb` with each string in the array `a` appended.
    ///
    pub def appendLines(sb: FunStringBuilder, a: Array[String]): FunStringBuilder & Impure =
        Array.foldLeft(appendLine, sb, a)

    ///
    /// Returns `sb` with the strings produced by `f(x)` for each x in array `a` appended.
    ///
    pub def appendLinesWith(sb: FunStringBuilder, f: a -> String, a: Array[a]): FunStringBuilder & Impure =
       Array.foldLeft((b,x) -> appendLineWith(b, f, x), sb, a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate(sb: FunStringBuilder, sep: String, a: Array[String]): FunStringBuilder & Impure =
        let step = (ac, s) -> {
            let (sb1, i) = ac;
            if (i == 0)
                (sb1.appendString(s), i + 1)
            else
                (sb1.appendString(sep).appendString(s), i + 1)
        };
        Array.foldLeft(step, (sb, 0), a) |> fst

    ///
    /// Return `sb` converted  to a string.
    ///
    pub def toString(sb: FunStringBuilder): String = toStringHelper(sb) as & Pure
    
    def toStringHelper(sb: FunStringBuilder): String & Impure =
        let x = StringBuilder.new();
        toStringHelperBody(x, sb);
        StringBuilder.toString(x)

    def toStringHelperBody(x: StringBuilder, src: FunStringBuilder): Unit & Impure = match src {
        case Empty => ()
        case Text(s) => StringBuilder.appendString!(x, s)
        case Char(c) => StringBuilder.appendChar!(x, c)
        case Join(src1, src2) => {
            toStringHelperBody(x, src1);
            toStringHelperBody(x, src2)
        }
    }


}
