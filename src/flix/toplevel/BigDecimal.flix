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

pub opaque type BigDecimal = ##java.math.BigDecimal

instance ToString[BigDecimal] {
    pub def toString(x: BigDecimal): String =
        import java.math.BigDecimal.toString();
        let BigDecimal(x1) = x;
        toString(x1) as & Pure
}

instance FromString[BigDecimal] {
    pub def fromString(x: String): Option[BigDecimal] =
        import java.lang.String.strip();
        import new java.math.BigDecimal(String) as parseBigDecimal;
        try {
            Some(BigDecimal(parseBigDecimal(strip(x)))) as & Pure
        } catch {
            case _: ##java.lang.NumberFormatException => None
        }
}

instance Add[BigDecimal] {
    pub def add(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.add(##java.math.BigDecimal);
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(add(x1, y1)) as & Pure
}

instance Sub[BigDecimal] {
    pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.subtract(##java.math.BigDecimal);
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(subtract(x1, y1)) as & Pure
}

instance Mul[BigDecimal] {
    pub def mul(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.multiply(##java.math.BigDecimal);
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(multiply(x1, y1)) as & Pure
}

instance Div[BigDecimal] {
    pub def div(x: BigDecimal, y: BigDecimal): BigDecimal =
        import java.math.BigDecimal.divide(##java.math.BigDecimal);
        let BigDecimal(x1) = x;
        let BigDecimal(y1) = y;
        BigDecimal(divide(x1, y1)) as & Pure
}

instance Neg[BigDecimal] {
    pub def neg(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.negate();
        let BigDecimal(x1) = x;
        BigDecimal(negate(x1)) as & Pure
}

namespace BigDecimal {

    pub def one(): BigDecimal =
        import get java.math.BigDecimal:ONE as getONE;
        BigDecimal(getONE()) as & Pure

    pub def ten(): BigDecimal =
        import get java.math.BigDecimal:TEN as getTEN;
        BigDecimal(getTEN()) as & Pure

    pub def zero(): BigDecimal =
        import get java.math.BigDecimal:ZERO as getZERO;
        BigDecimal(getZERO()) as & Pure


    pub def abs(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.abs();
        let BigDecimal(x1) = x;
        BigDecimal(abs(x1)) as & Pure

}
