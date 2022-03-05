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

namespace Text/NumberFormatter {

    ///
    /// Return a decimal representation of `x`.
    ///
    pub def formatFloat64(decimalPlaces: Int32, x: Float64): String =
        formatFloat64Helper(decimalPlaces, x) as & Pure

    ///
    /// Helper function for `formatFloat64`.
    ///
    pub def formatFloat64Helper(decimalPlaces: Int32, x: Float64): String & Impure =
        import new java.text.DecimalFormat(): ##java.text.DecimalFormat & Pure as newDecimalFormat;
        import java.text.DecimalFormat.setMaximumFractionDigits(Int32): Unit & Impure;
        import java.text.NumberFormat.format(Float64): String & Pure;
        let df = newDecimalFormat();
        setMaximumFractionDigits(df, decimalPlaces);
        let nf = df as ##java.text.NumberFormat;
        format(nf, x)

    ///
    /// Parse the decimal string `s` as a Float64, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `OK(x)`, a parse failure is indicated by `Err(_)`.
    ///
    /// The format of a decimal string is decided by the locale settings of the JVM.
    ///
    pub def parseFloat64(s: String): Result[Float64, String] = parseFloat64Helper(s)

    ///
    /// Helper function for `parseFloat64`.
    ///
    def parseFloat64Helper(s: String): Result[Float64, String] & Pure = try {
        import new java.text.DecimalFormat(): ##java.text.DecimalFormat & Pure as newDecimalFormat;
        import java.lang.String.strip(): String & Pure;
        import java.text.NumberFormat.parse(String): ##java.lang.Number & Pure;
        import java.lang.Number.doubleValue(): Float64 & Pure;
        let nf = newDecimalFormat() as ##java.text.NumberFormat;
        let n1 = parse(nf, strip(s));
        Ok(doubleValue(n1))
    } catch {
        case _: ##java.lang.NumberFormatException => Err("Float64.parseFloat64")
    }


    ///
    /// Format `x` with underscores delimiting every three digits.
    ///
    pub def formatInt64WithGrouping(x: Int64): String =
        formatInt64WithGroupingHelper(x) as & Pure

    ///
    /// Helper function for `formatInt64WithGrouping`.
    ///
    def formatInt64WithGroupingHelper(x: Int64): String & Impure =
        import new java.text.DecimalFormat(): ##java.text.DecimalFormat & Impure as newDecimalFormat;
        import java.text.DecimalFormat.setGroupingSize(Int32): Unit & Impure;
        import java.text.DecimalFormat.setDecimalFormatSymbols(##java.text.DecimalFormatSymbols): Unit & Impure;
        import new java.text.DecimalFormatSymbols(): ##java.text.DecimalFormatSymbols & Impure as newDecimalFormatSymbols;
        import java.text.DecimalFormatSymbols.setGroupingSeparator(Char): Unit & Impure;
        import java.text.NumberFormat.format(Int64): String & Pure;
        let format1 = newDecimalFormat();
        let symbols = newDecimalFormatSymbols();
        setGroupingSeparator(symbols, '_');
        setGroupingSize(format1, 3);
        setDecimalFormatSymbols(format1, symbols);
        let format2 = format1 as ##java.text.NumberFormat;
        format(format2, x)

}
