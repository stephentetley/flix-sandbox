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


namespace FlixSandbox/Doodle {
    
    pub enum BigDecimal(##java.math.BigDecimal)

    instance ToString[BigDecimal] {
        pub def toString(x: BigDecimal): String =
            import java.math.BigDecimal.toString(): String & Pure;
            let BigDecimal(x1) = x;
            toString(x1)
    }

    instance FromString[BigDecimal] {
        pub def fromString(x: String): Option[BigDecimal] =
            import java.lang.String.strip(): String & Pure;
            import new java.math.BigDecimal(String): ##java.math.BigDecimal & Pure as parseBigDecimal;
            try {
                Some(BigDecimal(parseBigDecimal(strip(x))))
            } catch {
                case _: ##java.lang.NumberFormatException => None
            }
    }

    instance Add[BigDecimal] {
        pub def add(x: BigDecimal, y: BigDecimal): BigDecimal =
            import java.math.BigDecimal.add(##java.math.BigDecimal): ##java.math.BigDecimal & Pure;
            let BigDecimal(x1) = x;
            let BigDecimal(y1) = y;
            BigDecimal(add(x1, y1))
    }

    instance Sub[BigDecimal] {
        pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal =
            import java.math.BigDecimal.subtract(##java.math.BigDecimal): ##java.math.BigDecimal & Pure;
            let BigDecimal(x1) = x;
            let BigDecimal(y1) = y;
            BigDecimal(subtract(x1, y1))
    }

    instance Mul[BigDecimal] {
        pub def mul(x: BigDecimal, y: BigDecimal): BigDecimal =
            import java.math.BigDecimal.multiply(##java.math.BigDecimal): ##java.math.BigDecimal & Pure;
            let BigDecimal(x1) = x;
            let BigDecimal(y1) = y;
            BigDecimal(multiply(x1, y1))
    }

    instance Div[BigDecimal] {
        pub def div(x: BigDecimal, y: BigDecimal): BigDecimal =
            import java.math.BigDecimal.divide(##java.math.BigDecimal): ##java.math.BigDecimal & Pure;
            let BigDecimal(x1) = x;
            let BigDecimal(y1) = y;
            BigDecimal(divide(x1, y1))
    }

    instance Neg[BigDecimal] {
        pub def neg(x: BigDecimal): BigDecimal =
            import java.math.BigDecimal.negate(): ##java.math.BigDecimal & Pure;
            let BigDecimal(x1) = x;
            BigDecimal(negate(x1))
    }
}

namespace FlixSandbox/Doodle/BigDecimal {

    use FlixSandbox/Doodle.BigDecimal;
    use FlixSandbox/Doodle.BigDecimal.{BigDecimal};

    pub def one(): BigDecimal =
        import static get java.math.BigDecimal.ONE: ##java.math.BigDecimal & Pure as getONE;
        BigDecimal(getONE())

    pub def ten(): BigDecimal =
        import static get java.math.BigDecimal.TEN: ##java.math.BigDecimal & Pure as getTEN;
        BigDecimal(getTEN())

    pub def zero(): BigDecimal =
        import static get java.math.BigDecimal.ZERO: ##java.math.BigDecimal & Pure as getZERO;
        BigDecimal(getZERO())


    pub def abs(x: BigDecimal): BigDecimal =
        import java.math.BigDecimal.abs(): ##java.math.BigDecimal & Pure;
        let BigDecimal(x1) = x;
        BigDecimal(abs(x1))

}
