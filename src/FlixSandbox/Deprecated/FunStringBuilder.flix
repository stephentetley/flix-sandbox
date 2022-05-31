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


namespace FlixSandbox/Deprecated {

    // TODO use stdlib Chain

    ///
    /// Represents a functional StringBuilder.
    ///
    pub enum FunStringBuilder {
        case Empty,
        case Text(String),
        case Char(Char),
        case Join(FunStringBuilder, FunStringBuilder)
    }
}

namespace FlixSandbox/Deprecated/FunStringBuilder {

    use FlixSandbox/Deprecated.FunStringBuilder;
    use FlixSandbox/Deprecated.FunStringBuilder.{Empty, Text, Char, Join};

    ///
    /// Returns an empty FunStringBuilder.
    ///
    pub def funStringBuilder(): FunStringBuilder = Empty

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
    pub def appendString(s: String, sb: FunStringBuilder): FunStringBuilder = append(Text(s), sb)

    ///
    /// Returns `sb` with the char `c` appended.
    ///
    pub def appendChar(c: Char, sb: FunStringBuilder): FunStringBuilder = append(Char(c), sb)


    ///
    /// Returns `sb` with the Int8 `i` appended.
    ///
    pub def appendInt8(i: Int8, sb: FunStringBuilder): FunStringBuilder =
        appendString(Int8.toString(i), sb)

    ///
    /// Returns `sb` with the Int16 `i` appended.
    ///
    pub def appendInt16(i: Int16, sb: FunStringBuilder): FunStringBuilder =
        appendString(Int16.toString(i), sb)

    ///
    /// Returns `sb` with the Int32 `i` appended.
    ///
    pub def appendInt32(i: Int32, sb: FunStringBuilder): FunStringBuilder =
        appendString(Int32.toString(i), sb)

    ///
    /// Returns `sb` with the Int64 `i` appended.
    ///
    pub def appendInt64(i: Int64, sb: FunStringBuilder): FunStringBuilder =
        appendString(Int64.toString(i), sb)

    ///
    /// Returns `sb` with the BigInt `i` appended.
    ///
    pub def appendBigInt(i: BigInt, sb: FunStringBuilder): FunStringBuilder =
        appendString(BigInt.toString(i), sb)

    ///
    /// Returns `sb` with the Float32 `d` appended.
    ///
    pub def appendFloat32(d: Float32, sb: FunStringBuilder): FunStringBuilder =
        appendString(Float32.toString(d), sb)

    ///
    /// Returns `sb` with the Float64 `d` appended.
    ///
    pub def appendFloat64(d: Float64, sb: FunStringBuilder): FunStringBuilder =
        appendString(Float64.toString(d), sb)

    ///
    /// Returns `sb` with the system line separator append.
    ///
    pub def appendLineSeparator(sb: FunStringBuilder): FunStringBuilder =
        appendString(String.lineSeparator(), sb)

    ///
    /// Returns `sb` with the string `s` appended followed by the system line separator.
    ///
    pub def appendLine(sb: FunStringBuilder, s: String): FunStringBuilder =
        let sb1 = appendString(s, sb);
        appendLineSeparator(sb1)

    ///
    /// Returns `sb` with the string produced by `f(x)` appended.
    ///
    pub def appendLineWith(f: a -> String, x: a, sb: FunStringBuilder): FunStringBuilder =
        let sb1 = appendString(f(x), sb);
        appendLineSeparator(sb1)

    ///
    /// Returns `sb` with each string in the array `a` appended.
    ///
    pub def appendLines(a: Array[String], sb: FunStringBuilder): FunStringBuilder & Impure =
        Array.foldLeft(appendLine, sb, a)

    ///
    /// Returns `sb` with the strings produced by `f(x)` for each x in array `a` appended.
    ///
    pub def appendLinesWith(f: a -> String, a: Array[a], sb: FunStringBuilder): FunStringBuilder & Impure =
       Array.foldLeft((sb1, x) -> appendLineWith(f, x, sb1), sb, a)

    ///
    /// Append the array of strings `a` separating each pair of string with `sep` to the StringBuilder `sb`.
    ///
    pub def intercalate(sep: String, a: Array[String], sb: FunStringBuilder): FunStringBuilder & Impure =
        let step = (ac, s) -> {
            let (sb1, i) = ac;
            if (i == 0)
                (appendString(s, sb1), i + 1)
            else
                (let sb2 = appendString(sep, sb1); appendString(s, sb2), i + 1)
        };
        Array.foldLeft(step, (sb, 0), a) |> fst

    ///
    /// Return `sb` converted  to a string.
    ///
    pub def toString(f: FunStringBuilder): String =
        region r { 
            let sb = new StringBuilder(r);
            toStringHelperBody(sb, f, k -> k as & r);
            StringBuilder.toString(sb)
        }

    def toStringHelperBody(sb: StringBuilder[r], src: FunStringBuilder, k: Unit -> Unit \ Write(r)): Unit \ Write(r) = match src {
        case Empty => k()
        case Text(s) => {StringBuilder.append!(s, sb); k()}
        case Char(c) => {StringBuilder.append!(c, sb); k()}
        case Join(src1, src2) => {
            toStringHelperBody(sb, src1, _ ->
                toStringHelperBody(sb, src2, _ ->
                    k()))
        }
    }


}
