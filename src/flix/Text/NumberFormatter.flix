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
        import new java.text.DecimalFormat() as newDecimalFormat;
        import java.text.DecimalFormat.setMaximumFractionDigits(Int32);
        import java.text.NumberFormat.format(Float64);
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
    pub def parseFloat64(s: String): Result[Float64, String] = parseFloat64Helper(s) as & Pure

    ///
    /// Helper function for `parseFloat64`.
    ///
    def parseFloat64Helper(s: String): Result[Float64, String] & Impure = try {
        import new java.text.DecimalFormat() as newDecimalFormat;
        import java.lang.String.strip();
        import java.text.NumberFormat.parse(String);
        import java.lang.Number.doubleValue();
        let nf = newDecimalFormat() as ##java.text.NumberFormat;
        let n1 = parse(nf, strip(s));
        Ok(doubleValue(n1))
    } catch {
        case _: ##java.lang.NumberFormatException => Err("Float64.fromDecimalString")
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
        import new java.text.DecimalFormat() as newDecimalFormat;
        import java.text.DecimalFormat.setGroupingSize(Int32);
        import java.text.DecimalFormat.setDecimalFormatSymbols(##java.text.DecimalFormatSymbols);
        import new java.text.DecimalFormatSymbols() as newDecimalFormatSymbols;
        import java.text.DecimalFormatSymbols.setGroupingSeparator(Char);
        import java.text.NumberFormat.format(Int64);
        let format1 = newDecimalFormat();
        let symbols = newDecimalFormatSymbols();
        setGroupingSeparator(symbols, '_');
        setGroupingSize(format1, 3);
        setDecimalFormatSymbols(format1, symbols);
        let format2 = format1 as ##java.text.NumberFormat;
        format(format2, x)

}
