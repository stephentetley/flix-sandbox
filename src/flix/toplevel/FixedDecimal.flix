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

pub opaque type FixedDecimal[a] = (a, BigInt)

pub lawless class HasResolution[a] {
    pub def resolution(x: FixedDecimal[a]): BigInt
    pub def zero(): FixedDecimal[a]
}

pub opaque type E3 = Unit

instance HasResolution[E3] {
    pub def resolution(_: FixedDecimal[E3]): BigInt = 1000ii
    pub def zero(): FixedDecimal[E3] = FixedDecimal((E3(), 0ii))
}

instance ToString[FixedDecimal[a]] with HasResolution[a] {
    pub def toString(x: FixedDecimal[a]): String =
        use Option.{flatMap};
        let FixedDecimal((_, x1)) = x;
        let res = HasResolution.resolution(x);
        let step = () -> {
            let* d1 = BigInt.tryToFloat64(x1);
            let* r1 = BigInt.tryToFloat64(res);
            Some(ToString.toString(d1 / r1))
        };
        Option.getWithDefault(step(), "Error - ToString[FixedDecimal] failed")
}

instance FromString[FixedDecimal[a]] with HasResolution[a] {
    pub def fromString(x: String): Option[FixedDecimal[a]] =
        use Option.{flatMap};
        let* d1 = FromString.fromString(x);
        let zero1 = HasResolution.zero();
        let FixedDecimal((res, _)) = zero1;
        let* resolution = BigInt.tryToFloat64(HasResolution.resolution(zero1));
        let* ans = Float64.tryToBigInt(d1 * resolution);        // round here...
        Some(FixedDecimal((res, ans)))
}

instance Add[FixedDecimal[a]] with HasResolution[a] {
    pub def add(x: FixedDecimal[a], y: FixedDecimal[a]): FixedDecimal[a] =
        let FixedDecimal((res, x1)) = x;
        let FixedDecimal((_, y1)) = y;
        FixedDecimal((res, x1 + y1))
}

namespace FixedDecimal {

    pub def zero(): FixedDecimal[a] with HasResolution[a] =
        HasResolution.zero()

    pub def ofInt32(x: Int32): FixedDecimal[a] with HasResolution[a] =
        let zero1 = HasResolution.zero();
        let FixedDecimal((res, _)) = zero1;
        let resolution = HasResolution.resolution(zero1);
        let x1 = Int32.toBigInt(x) * resolution;
        FixedDecimal((res, x1))

    pub def abs(x: FixedDecimal[a]): FixedDecimal[a] with HasResolution[a] =
        let FixedDecimal((res, x1)) = x;
        FixedDecimal((res, BigInt.abs(x1)))

    pub def internal(x: FixedDecimal[a]): (BigInt, BigInt) with HasResolution[a] =
        let FixedDecimal((_, x1)) = x;
        let res = HasResolution.resolution(x);
        (res, x1)


}
