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


namespace FlixSandbox/Doodle {

    pub enum MutBitSet[_: Region](##java.util.BitSet)

    /// instance ToString[MutBitSet] {
    ///     pub def toString(x: MutBitSet[r]): String =
    ///         import java.util.BitSet.toString(): String & Pure;
    ///         let MutBitSet(x1) = x;
    ///         toString(x1)
    /// }

    /// instance Eq[MutBitSet] {
    ///     pub def eq(x: MutBitSet, y: MutBitSet[r]): Bool =
    ///         import java.util.BitSet.equals(##java.lang.Object): Bool & Pure;
    ///         let MutBitSet(x1) = x;
    ///         let MutBitSet(y1) = y;
    ///         equals(x1, y1 as ##java.lang.Object)
    /// }

}

namespace FlixSandbox/Doodle/MutBitSet {

    use FlixSandbox/Doodle.MutBitSet;
    use FlixSandbox/Doodle.MutBitSet.{MutBitSet};

    pub def empty(_: Region[r]): MutBitSet[r] \ Write(r) =
        import new java.util.BitSet(): ##java.util.BitSet & r as newBitSet;
        MutBitSet(newBitSet())

    pub def singleton(idx: Int32, r: Region[r]): MutBitSet[r] \ Write(r) =
        let x = empty(r);
        set!(idx, x);
        x

    pub def isEmpty(s: MutBitSet[r]): Bool \ Read(r) =
        import java.util.BitSet.isEmpty(): Bool & r;
        let MutBitSet(s1) = s;
        isEmpty(s1)

    pub def size(s: MutBitSet[r]): Int32 \ Read(r) =
        import java.util.BitSet.size(): Int32 & r;
        let MutBitSet(s1) = s;
        size(s1)

    pub def cardinality(s: MutBitSet[r]): Int32 \ Read(r) =
        import java.util.BitSet.cardinality(): Int32 & r;
        let MutBitSet(s1) = s;
        cardinality(s1)

    pub def clone(s: MutBitSet[r1]): MutBitSet[r2] \ {Read(r1), Write(r2) }=
        import java.util.BitSet.clone(): ##java.util.BitSet & r1 and r2;
        let MutBitSet(s1) = s;
        let s2 = clone(s1) as ##java.util.BitSet;
        MutBitSet(s2)

    pub def union(x: MutBitSet[r], y: MutBitSet[r]): MutBitSet[r] \ Write(r) =
        import java.util.BitSet.and(##java.util.BitSet): Unit & r as primAnd!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primAnd!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def union!(x: MutBitSet[r], y: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.and(##java.util.BitSet): Unit & r as primAnd!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primAnd!(x1, y1)

    pub def intersection(x: MutBitSet[r], y: MutBitSet[r]): MutBitSet[r] \ Write(r) =
        import java.util.BitSet.or(##java.util.BitSet): Unit & r as primOr!;
        let MutBitSet(x1) = clone(x);
        let MutBitSet(y1) = y;
        primOr!(x1, y1);       /// Mutates x1
        MutBitSet(x1)

    /// Mutates x
    pub def intersection!(x: MutBitSet[r], y: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.or(##java.util.BitSet): Unit & r as primOr!;
        let MutBitSet(x1) = x;
        let MutBitSet(y1) = y;
        primOr!(x1, y1)

    /// Set all bits to `false` imperatively.
    pub def clearAll!(s: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.clear(): Unit & r;
        let MutBitSet(s1) = s;
        clear(s1)

    pub def get(idx: Int32, s: MutBitSet[r]): Bool \ Read(r) =
        import java.util.BitSet.get(Int32): Bool & r;
        let MutBitSet(s1) = s;
        get(s1, idx)

    pub def set!(idx: Int32, s: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.set(Int32): Unit & r;
        let MutBitSet(s1) = s;
        set(s1, idx)


    pub def clear!(idx: Int32, s: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.clear(Int32): Unit & r;
        let MutBitSet(s1) = s;
        clear(s1, idx)

    pub def flip!(idx: Int32, s: MutBitSet[r]): Unit \ Write(r) =
        import java.util.BitSet.flip(Int32): Unit & r;
        let MutBitSet(s1) = s;
        flip(s1, idx)

    pub def nextSetBit(idx: Int32, s: MutBitSet[r]): Option[Int32] \ Read(r) =
        import java.util.BitSet.nextSetBit(Int32): Int32 & r;
        try {
            let MutBitSet(s1) = s;
            let i = nextSetBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def nextClearBit(idx: Int32, s: MutBitSet[r]): Option[Int32] \ Read(r) =
        import java.util.BitSet.nextClearBit(Int32): Int32 & r;
        try {
            let MutBitSet(s1) = s;
            let i = nextClearBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def previousSetBit(idx: Int32, s: MutBitSet[r]): Option[Int32] \ Read(r) =
        import java.util.BitSet.previousSetBit(Int32): Int32 & r;
        try {
            let MutBitSet(s1) = s;
            let i = previousSetBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def previousClearBit(idx: Int32, s: MutBitSet[r]): Option[Int32] \ Read(r) =
        import java.util.BitSet.previousClearBit(Int32): Int32 & r;
        try {
            let MutBitSet(s1) = s;
            let i = previousClearBit(s1, idx);
            if (i < 0)
                None
            else Some(i)
        } catch {
            case _: ##java.lang.Exception => None
        }

    pub def minimum(s: MutBitSet[r]): Option[Int32] \ Read(r) =
        nextSetBit(0, s)

}
