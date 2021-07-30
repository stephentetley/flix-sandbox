/*
 * Copyright 2021 Stephen Tetley
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


namespace Data {

    pub opaque type MutBitSet = ##java.util.BitSet

    instance ToString[MutBitSet] {
        pub def toString(x: MutBitSet): String =
            import java.util.BitSet.toString();
            let MutBitSet(x1) = x;
            toString(x1) as & Pure
    }

    instance Eq[MutBitSet] {
        pub def eq(x: MutBitSet, y: MutBitSet): Bool =
            import java.util.BitSet.equals(##java.lang.Object);
            let MutBitSet(x1) = x;
            let MutBitSet(y1) = y;
            equals(x1, y1 as ##java.lang.Object) as & Pure
    }

}

namespace Data/MutBitSet {

    use Data.MutBitSet;
    use Data.MutBitSet.{MutBitSet};

    pub def empty(): MutBitSet & Impure =
        import new java.util.BitSet() as newBitSet;
        MutBitSet(newBitSet())

    pub def singleton(idx: Int32): MutBitSet & Impure =
        let x = empty();
        set!(idx, x);
        x

    pub def isEmpty(s: MutBitSet): Bool & Impure =
        import java.util.BitSet.isEmpty();
        let MutBitSet(s1) = s;
        isEmpty(s1)

    pub def size(s: MutBitSet): Int32 & Impure =
        import java.util.BitSet.size();
        let MutBitSet(s1) = s;
        size(s1)

    pub def cardinality(s: MutBitSet): Int32 & Impure =
        import java.util.BitSet.cardinality();
        let MutBitSet(s1) = s;
        cardinality(s1)

    pub def clone(s: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.clone();
        let MutBitSet(s1) = s;
        let s2 = clone(s1) as ##java.util.BitSet;
        MutBitSet(s2)

    pub def union(x: MutBitSet, y: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.and(##java.util.BitSet) as primAnd!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primAnd!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def union!(x: MutBitSet, y: MutBitSet): Unit & Impure =
        import java.util.BitSet.and(##java.util.BitSet) as primAnd!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primAnd!(x1, y1)

    pub def intersection(x: MutBitSet, y: MutBitSet): MutBitSet & Impure =
        import java.util.BitSet.or(##java.util.BitSet) as primOr!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primOr!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def intersection!(x: MutBitSet, y: MutBitSet): Unit & Impure =
        import java.util.BitSet.or(##java.util.BitSet) as primOr!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primOr!(x1, y1)

    /// Set all bits to `false` imperatively.
    pub def clearAll!(s: MutBitSet): Unit & Impure =
        import java.util.BitSet.clear();
        let MutBitSet(s1) = s;
        clear(s1)

    pub def get(idx: Int32, s: MutBitSet): Bool & Impure =
        import java.util.BitSet.get(Int32);
        let MutBitSet(s1) = s;
        get(s1, idx)

    pub def set!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.set(Int32);
        let MutBitSet(s1) = s;
        set(s1, idx)


    pub def clear!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.clear(Int32);
        let MutBitSet(s1) = s;
        clear(s1, idx)

    pub def flip!(idx: Int32, s: MutBitSet): Unit & Impure =
        import java.util.BitSet.flip(Int32);
        let MutBitSet(s1) = s;
        flip(s1, idx)

    pub def nextSetBit(idx: Int32, s: MutBitSet): Option[Int32] & Impure =
        import java.util.BitSet.nextSetBit(Int32);
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
        import java.util.BitSet.nextClearBit(Int32);
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
        import java.util.BitSet.previousSetBit(Int32);
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
        import java.util.BitSet.previousClearBit(Int32);
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

}
