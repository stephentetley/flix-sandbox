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


namespace FlixSandbox/BitSet {

    // https://stackoverflow.com/questions/29526985/java-from-biginteger-to-bitset-and-back


    // Only use non-negative ints!

    pub enum BitSet(BigInt) with Eq

    instance ToString[BitSet] {
        pub def toString(s: BitSet): String = ("BitSet#" + internalToString(s)) as & Pure
    }

    instance Hash[BitSet] {
        pub def hash(s: BitSet): Int32 = 
            import java.util.BitSet.hashCode(): Int32 & Impure;
            (toJavaBitSet(s) |> hashCode) as & Pure
    }

    instance LowerBound[BitSet] {
        pub def minValue(): BitSet = empty()
    }

    instance PartialOrder[BitSet] {
        pub def lessEqual(s1: BitSet, s2: BitSet): Bool = isSubsetOf(subset = s1, s2)
    }

    instance JoinLattice[BitSet] {
        pub def leastUpperBound(s1: BitSet, s2: BitSet): BitSet = s1 `union` s2
    }

    instance MeetLattice[BitSet] {
        pub def greatestLowerBound(s1: BitSet, s2: BitSet): BitSet = s1 `intersection` s2
    }

    instance SemiGroup[BitSet] {
        pub def combine(s1: BitSet, s2: BitSet): BitSet = union(s1, s2)
    }

    instance CommutativeSemiGroup[BitSet]

    instance Monoid[BitSet] {
        pub def empty(): BitSet = empty()
    }

    instance CommutativeMonoid[BitSet]

    pub def empty(): BitSet =
        BitSet(0ii)


    pub def singleton(i: Int32): BitSet =
        BitSet(BigInt.setBit(0ii, i))

    pub def range(b: Int32, e: Int32): BitSet =
        def loop(i, s) = {
            if (i >= e)
                s
            else
                loop(i + 1, insert(i, s))
        };
        loop(b, empty())


    pub def insert(i: Int32, s: BitSet): BitSet =
        let BitSet(s1) = s;
        BitSet(BigInt.setBit(s1, i))

    pub def remove(i: Int32, s: BitSet): BitSet =
        let BitSet(s1) = s;
        BitSet(BigInt.clearBit(s1, i))


    pub def isEmpty(s: BitSet): Bool =
        let BitSet(s1) = s;
        s1 == 0ii

    pub def memberOf(i: Int32, s: BitSet): Bool = 
        let BitSet(s1) = s;
        (s1 &&& BigInt.setBit(0ii, i)) != 0ii


    pub def isSubsetOf(subset: {subset :: BitSet}, s: BitSet): Bool = 
        let BitSet(s1) = s;
        let BitSet(subset1) = subset.subset;
        (s1 ||| subset1) == s1

    pub def isProperSubsetOf(subset: {subset :: BitSet}, s: BitSet): Bool = 
        isSubsetOf(subset, s) and subset.subset != s


    def toJavaBitSet(s: BitSet): ##java.util.BitSet & Impure = 
        import java.math.BigInteger.toByteArray(): Array[Int8] & Impure;
        import static java.util.BitSet.valueOf(Array[Int8]): ##java.util.BitSet & Impure;
        let BitSet(x) = s;
        let arr = toByteArray(x);
        let arr1 = Array.reverse(arr);
        valueOf(arr1)

    def toJavaIntStream(s: BitSet): ##java.util.stream.IntStream & Impure = 
        import java.util.BitSet.stream(): ##java.util.stream.IntStream & Impure;
        toJavaBitSet(s) |> stream


    def internalToString(s: BitSet): String & Impure = 
        let j1 = toJavaBitSet(s);
        Object.toString(j1)


    pub def size(s: BitSet): Int64 =
        import java.util.stream.IntStream.count(): Int64 & Impure;
        (toJavaIntStream(s) |> count) as & Pure

    pub def cardinality(s: BitSet): Int32 =
        import java.util.BitSet.cardinality(): Int32 & Impure;
        (toJavaBitSet(s) |> cardinality) as & Pure



    pub def union(x: BitSet, y: BitSet): BitSet =
        let BitSet(x1) = x;
        let BitSet(y1) = y;
        BitSet(x1 ||| y1)

    pub def intersection(x: BitSet, y: BitSet): BitSet =
        let BitSet(x1) = x;
        let BitSet(y1) = y;
        BitSet(x1 &&& y1)

    pub def difference(x: BitSet, y: BitSet): BitSet =
        let BitSet(x1) = x;
        let BitSet(y1) = y;
        BitSet(x1 &&& BitwiseNot.not(y1))


    pub def toArray(s: BitSet): Array[Int32] & Impure = 
        import java.util.stream.IntStream.toArray(): Array[Int32] & Impure;
        toJavaIntStream(s) |> toArray

    pub def toList(s: BitSet): List[Int32] = 
        foldLeft((k, i) -> ks -> k(i :: ks), ks -> ks, s) |> f -> f(Nil)

    pub def toIterator(s: BitSet): Iterator[Int32] & Impure = 
        let istrm = toJavaIntStream(s);
        FlixSandbox/Marshal.unmarshalIntStream(istrm)

    pub def toSet(s: BitSet): Set[Int32] = 
        foldLeft((ac, i) -> Set.insert(i, ac), Set.empty(), s)


    pub def filter(f: Int32 -> Bool & ef, s: BitSet): BitSet & ef = 
        foldLeft((ac, i) -> if (f(i)) insert(i, ac) else ac, empty(), s)        

    pub def map(f: Int32 -> Int32 & ef, s: BitSet): BitSet & ef = 
        foldLeft((ac, i) -> insert(f(i), ac), empty(), s)

    pub def foldLeft(f: (ac, Int32) -> ac & ef, start: ac, s: BitSet): ac & ef = 
        let step = () -> {
            let iter = toIterator(s);
            Iterator.foldLeft((ac, i) -> f(ac, i), start, iter)
        };
        step() as & ef

}
