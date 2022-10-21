/*
 *  Copyright 2021 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

pub enum BigDecimal(##java.math.BigDecimal)

instance Eq[BigDecimal] {
    pub def eq(x: BigDecimal, y: BigDecimal): Bool =
        import java.math.BigDecimal.equals(##java.lang.Object): Bool \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        equals(x1, y1 as ##java.lang.Object)
}

instance Order[BigDecimal] {
    pub def compare(x: BigDecimal, y: BigDecimal): Comparison =
        import java.math.BigDecimal.compareTo(##java.math.BigDecimal): Int32 \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        compareTo(x1, y1) |> Comparison.fromInt32
}


instance ToString[BigDecimal] {
    pub def toString(x: BigDecimal): String =
        import java.math.BigDecimal.toString(): String \ {};
        let BigDecimal(x1) = x;
        toString(x1)
}

instance FromString[BigDecimal] {
    pub def fromString(x: String): Option[BigDecimal] =
        import java.lang.String.strip(): String \ {};
        import new java.math.BigDecimal(String): ##java.math.BigDecimal \ {} as parseBigDecimal;
        try {
            Some(BigDecimal(parseBigDecimal(strip(x))))
        } catch {
            case _: ##java.lang.NumberFormatException => None
        }
}

instance Add[BigDecimal] {
    pub def add(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.add(##java.math.BigDecimal): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(add(x1, y1))
}

instance Sub[BigDecimal] {
    pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.subtract(##java.math.BigDecimal): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(subtract(x1, y1))
}

instance Mul[BigDecimal] {
    pub def mul(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.multiply(##java.math.BigDecimal): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(multiply(x1, y1))
}

instance Div[BigDecimal] {
    pub def div(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.divide(##java.math.BigDecimal): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(divide(x1, y1))
}

instance Neg[BigDecimal] {
    pub def neg(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.negate(): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        BigDecimal(negate(x1))
}


namespace BigDecimal {
    

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigDecimal, y: BigDecimal): BigDecimal = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigDecimal, y: BigDecimal): BigDecimal = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.abs(): ##java.math.BigDecimal \ {};
        let BigDecimal(x1) = x;
        abs(x1) |> BigDecimal

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigDecimal, y: BigDecimal): BigDecimal = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigDecimal, y: BigDecimal): Int32 =
        import java.math.BigDecimal.compareTo(##java.math.BigDecimal): Int32 \ {};
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        compareTo(x1, y1)

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigDecimal): Int32 =
        import java.math.BigDecimal.signum(): Int32 \ {};
        let BigDecimal(x1) = x;
        signum(x1)


    ///
    /// Parse the string `s` as an BigInt, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigDecimal] = try {
        import java.lang.String.strip(): String \ {};
        import new java.math.BigDecimal(String): ##java.math.BigDecimal \ {} as parseBigDecimal;
        Some(s |> strip |> parseBigDecimal |> BigDecimal)
    } catch {
        case _: ##java.lang.NumberFormatException => None
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` is within the range
    /// of Int8, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    // @Time(1) @Space(1)
    pub def tryToInt8(x: BigDecimal): Option[Int8] =
        tryToInt32(x) |> Option.flatMap(Int32.tryToInt8)

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` is within the range
    /// of Int16, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    @Time(1) @Space(1)
    pub def tryToInt16(x: BigDecimal): Option[Int16] =
        tryToInt32(x) |> Option.flatMap(Int32.tryToInt16)

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` is within the range
    /// of Int32, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647).
    ///
    @Time(1) @Space(1)
    pub def tryToInt32(x: BigDecimal): Option[Int32] =
        import java.math.BigDecimal.intValue(): Int32 \ {};
        if (x < Int32X.toBigDecimal(Int32.minValue()) or x > Int32X.toBigDecimal(Int32.maxValue()))
            None
        else {
            let BigDecimal(x1) = x;
            Some(intValue(x1))
        }

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` is within the range
    /// of Int64, rounding `x` towards 0`.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807), or it is NaN or infinity.
    ///
    @Time(1) @Space(1)
    pub def tryToInt64(x: BigDecimal): Option[Int64] =
        import java.math.BigDecimal.longValue(): Int64 \ {};
        if (x < Int64X.toBigDecimal(Int64.minValue()) or x > Int64X.toBigDecimal(Int64.maxValue()))
            None
        else {
            let BigDecimal(x1) = x;
            Some(longValue(x1))
        }
        
    ///
    /// Convert `x` to a `BigInt`.
    ///
    @Time(1) @Space(1)
    pub def toBigInt(x: BigDecimal): BigInt =
        import java.math.BigDecimal.toBigInteger(): BigInt \ {};
        let BigDecimal(x1) = x;
        toBigInteger(x1)

    ///
    /// Returns `x` rounded up to a BigDecimal representing the nearest larger integer value.
    ///
    @Time(1) @Space(1)
    pub def ceil(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.setScale(Int32, ##java.math.RoundingMode): ##java.math.BigDecimal \ {};
        import static get java.math.RoundingMode.CEILING: ##java.math.RoundingMode \ {} as getCeiling;
        let BigDecimal(x1) = x;
        setScale(x1, 0, getCeiling()) |> BigDecimal

    ///
    /// Returns `x` rounded down to a BigDecimal representing the nearest smaller integer value.
    ///
    @Time(1) @Space(1)
    pub def floor(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.setScale(Int32, ##java.math.RoundingMode): ##java.math.BigDecimal \ {};
        import static get java.math.RoundingMode.FLOOR: ##java.math.RoundingMode \ {} as getFloor;
        let BigDecimal(x1) = x;
        setScale(x1, 0, getFloor()) |> BigDecimal

    ///
    /// Returns `x` rounded to a BigDecimal representing the nearest integer value.
    ///
    /// The rounding may be upwards or downwards. If the rounding up and rounding down are equally
    /// close, `x` will be rounded to an even value (i.e. `round(0.5ff64) == 0.0ff64`).
    ///
    @Time(1) @Space(1)
    pub def round(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.setScale(Int32, ##java.math.RoundingMode): ##java.math.BigDecimal \ {};
        import static get java.math.RoundingMode.HALF_EVEN: ##java.math.RoundingMode \ {} as getHalfEven;
        let BigDecimal(x1) = x;
        setScale(x1, 0, getHalfEven()) |> BigDecimal


}

namespace Int8X {

    pub def toBigDecimal(x: Int8): BigDecimal = 
        import new java.math.BigDecimal(Int32): ##java.math.BigDecimal \ {} as fromInt32;
        Int8.toInt32(x) |> fromInt32 |> BigDecimal
}

namespace Int16X {

    pub def toBigDecimal(x: Int16): BigDecimal = 
        import new java.math.BigDecimal(Int32): ##java.math.BigDecimal \ {} as fromInt32;
        Int16.toInt32(x) |> fromInt32 |> BigDecimal
}

namespace Int32X {

    pub def toBigDecimal(x: Int32): BigDecimal = 
        import new java.math.BigDecimal(Int32): ##java.math.BigDecimal \ {} as fromInt32;
        fromInt32(x) |> BigDecimal
}

namespace Int64X {

    pub def toBigDecimal(x: Int64): BigDecimal = 
        import new java.math.BigDecimal(Int64): ##java.math.BigDecimal \ {} as fromInt64;
        fromInt64(x) |> BigDecimal
}
