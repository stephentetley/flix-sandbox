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

    pub def minimum(s: MutBitSet): Option[Int32] & Impure =
        nextSetBit(0, s)

}
