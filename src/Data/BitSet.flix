/*
 * Copyright 2022 Stephen Tetley
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


namespace Data/BitSet {

    // https://stackoverflow.com/questions/29526985/java-from-biginteger-to-bitset-and-back

    pub opaque type BitSet with Eq = BigInt

    instance ToString[BitSet] {
        pub def toString(x: BitSet): String = internalToString(x) as & Pure
    }


    pub def empty(): BitSet =
        BitSet(0ii)


    pub def singleton(idx: Int32): BitSet =
        BitSet(BigInt.setBit(0ii, idx))


    pub def insert(idx: Int32, x: BitSet): BitSet =
        let BitSet(x1) = x;
        BitSet(BigInt.setBit(x1, idx))


    pub def isEmpty(s: BitSet): Bool =
        let BitSet(x) = s;
        x == 0ii

    def internalToString(s: BitSet): String & Impure = 
        let j1 = toJavaBitSet(s);
        Object.toString(j1)

    pub def toJavaBitSet(s: BitSet): ##java.util.BitSet & Impure = 
        import java.math.BigInteger.toByteArray(): Array[Int8] & Impure;
        import static java.util.BitSet.valueOf(Array[Int8]): ##java.util.BitSet & Impure;
        let BitSet(x) = s;
        let arr = toByteArray(x);
        valueOf(arr)

/*
    pub def size(s: MutBitSet): Int32 & Impure =
        import java.util.BitSet.size(): Int32 & Impure;
        let MutBitSet(s1) = s;
        size(s1)

    pub def cardinality(s: MutBitSet): Int32 & Impure =
        import java.util.BitSet.cardinality(): Int32 & Impure;
        let MutBitSet(s1) = s;
        cardinality(s1)

    pub def clone(s: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.clone(): ##java.util.BitSet & Impure;
        let MutBitSet(s1) = s;
        let s2 = clone(s1) as ##java.util.BitSet;
        MutBitSet(s2)

    pub def union(x: MutBitSet, y: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.and(##java.util.BitSet): Unit & Impure as primAnd!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primAnd!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def union!(x: MutBitSet, y: MutBitSet): Unit & Impure =
        import java.util.BitSet.and(##java.util.BitSet): Unit & Impure as primAnd!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primAnd!(x1, y1)

    pub def intersection(x: MutBitSet, y: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.or(##java.util.BitSet): Unit & Impure as primOr!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primOr!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def intersection!(x: MutBitSet, y: MutBitSet): Unit & Impure =
        import java.util.BitSet.or(##java.util.BitSet): Unit & Impure as primOr!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primOr!(x1, y1)

    /// Set all bits to `false` imperatively.
    pub def clearAll!(s: MutBitSet): Unit & Impure =
        import java.util.BitSet.clear(): Unit & Impure;
        let MutBitSet(s1) = s;
        clear(s1)

    pub def get(idx: Int32, s: MutBitSet): Bool & Impure =
        import java.util.BitSet.get(Int32): Bool & Impure;
        let MutBitSet(s1) = s;
        get(s1, idx)

    pub def set!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.set(Int32): Unit & Impure;
        let MutBitSet(s1) = s;
        set(s1, idx)


    pub def clear!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.clear(Int32): Unit & Impure;
        let MutBitSet(s1) = s;
        clear(s1, idx)

    pub def flip!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.flip(Int32): Unit & Impure;
        let MutBitSet(s1) = s;
        flip(s1, idx)

    pub def nextSetBit(idx: Int32, s: MutBitSet): Option[Int32] & Impure =
        import java.util.BitSet.nextSetBit(Int32): Int32 & Impure;
        try {
            let MutBitSet(s1) = s;
            let i = nextSetBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def nextClearBit(idx: Int32, s: MutBitSet): Option[Int32] & Impure =
        import java.util.BitSet.nextClearBit(Int32): Int32 & Impure;
        try {
            let MutBitSet(s1) = s;
            let i = nextClearBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def previousSetBit(idx: Int32, s: MutBitSet): Option[Int32] & Impure =
        import java.util.BitSet.previousSetBit(Int32): Int32 & Impure;
        try {
            let MutBitSet(s1) = s;
            let i = previousSetBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def previousClearBit(idx: Int32, s: MutBitSet): Option[Int32] & Impure =
        import java.util.BitSet.previousClearBit(Int32): Int32 & Impure;
        try {
            let MutBitSet(s1) = s;
            let i = previousClearBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def minimum(s: MutBitSet): Option[Int32] & Impure =
        nextSetBit(0, s)
*/
}
