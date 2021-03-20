/*
 * Copyright 2020
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

///
/// The Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports fast append and "snoc" - appending elements at the tail
/// of the list.
///
enum Chain[t] {
    case Empty,
    case One(t),
    case Chain(Chain[t], Chain[t])
}

instance ToString[Chain[a]] with [a : ToString] {
    pub def toString(xs: Chain[a]): String =
        let fn = () -> {
            let sb = StringBuilder.new();
            StringBuilder.appendString!(sb, "Chain#{");
            Chain.foldLeft((ac, x) -> {
                if (ac < 1)
                    StringBuilder.appendString!(sb, "${x}")
                else
                    StringBuilder.appendString!(sb, ", ${x}");
                ac + 1}, 0, xs);
            StringBuilder.appendString!(sb, "}");
            StringBuilder.toString(sb)
        };
        fn() as & Pure
}

instance Eq[Chain[a]] with[a : Eq] {
    pub def eq(xs: Chain[a], ys: Chain[a]): Bool = Chain.equals(xs, ys)
}

instance Monoid[Chain[a]] {
    pub def empty(): Chain[a] = Chain.empty()
    pub def combine(xs: Chain[a], ys: Chain[a]): Chain[a] = Chain.append(xs, ys)
}

enum ViewLeft[t] {
    case NoneLeft
    case SomeLeft(t, Chain[t])
}

enum ViewRight[t] {
    case NoneRight
    case SomeRight(Chain[t], t)
}

namespace Chain {

    pub def empty(): Chain[a] = Empty

    pub def singleton(x: a): Chain[a] = One(x)

    pub def cons(x: a, xs: Chain[a]): Chain[a] = match xs {
        case Empty => One(x)
        case _ => Chain(One(x), xs)
    }

    pub def snoc(xs: Chain[a], x: a): Chain[a] = match xs {
        case Empty => One(x)
        case _ => Chain(xs, One(x))
    }


    ///
    /// Returns true if and only if `xs` is the empty chain, i.e. `Empty`.
    ///
    pub def isEmpty(xs: Chain[a]): Bool = match xs {
        case Empty => true
        case _ => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def head(xs: Chain[a]): Option[a] = match viewLeft(xs) {
        case SomeLeft(x, _) => Some(x)
        case _ => None
    }


    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def last(xs: Chain[a]): Option[a] = match viewRight(xs) {
        case SomeRight(_, x) => Some(x)
        case _ => None
    }

    ///
    /// Returns the length of `xs`.
    ///
    pub def length(xs: Chain[a]): Int32 = foldRight((_, ac) -> ac + 1, 0, xs)

    ///
    /// Returns `ys` appended to `xs`.
    ///
    pub def append(xs: Chain[a], ys: Chain[a]): Chain[a] = match (xs, ys) {
        case (Empty, _) => ys
        case (_, Empty) => xs
        case (_, _) => Chain(xs, ys)
    }

    ///
    /// Deconstruct a Chain from the left end.
    ///
    pub def viewLeft(xs: Chain[a]): ViewLeft[a] = viewLeftHelper(xs, Empty, x -> x)

    ///
    /// Helper function for `viewLeft`.
    ///
    /// Precondition: `xs` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewLeftHelper(xs: Chain[a], ac: Chain[a], k: ViewLeft[a] -> ViewLeft[a]): ViewLeft[a] =
        match xs {
            case Empty => k(NoneLeft)
            case One(x) => k(SomeLeft(x, ac))
            case Chain(ls, rs) => viewLeftHelper(ls, append(rs, ac),  k)
        }

    ///
    /// Deconstruct a Chain from the right end.
    ///
    pub def viewRight(xs: Chain[a]): ViewRight[a] = viewRightHelper(xs, Empty, x -> x)

    ///
    /// Helper function for `viewRight`.
    ///
    /// Precondition: `xs` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewRightHelper(xs: Chain[a], ac: Chain[a], k: ViewRight[a] -> ViewRight[a]): ViewRight[a] = match xs {
        case Empty => k(NoneRight)
        case One(x) => k(SomeRight(ac, x))
        case Chain(ls, rs) => viewRightHelper(rs, append(ac, ls),  k)
    }

    ///
    /// Returns `true` if and only if `xs` contains the element `a`.
    ///
    pub def memberOf[a: Eq](a: a, xs: Chain[a]): Bool = memberOfHelper(a, viewLeft(xs))

    ///
    /// Helper function for `memberOf`.
    ///
    def memberOfHelper[a: Eq](a: a, vx: ViewLeft[a]): Bool = match vx {
        case SomeLeft(x, _) if x == a => true
        case SomeLeft(_, xs) => memberOfHelper(a, viewLeft(xs))
        case NoneLeft => false
    }

    ///
    /// Returns the position of `a` in `xs`.
    ///
    /// Returns `-1` if `a` does not exist in `xs`.
    ///
    pub def indexOf[a: Eq](a: a, xs: Chain[a]): Int32 = indexOfHelper(a, 0, viewLeft(xs))

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper[a: Eq](a: a, ac: Int32, vx: ViewLeft[a]): Int32 = match vx {
        case NoneLeft => -1
        case SomeLeft(x, xs) => if (x == a) ac else indexOfHelper(a, ac + 1, viewLeft(xs))
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, xs: Chain[a]): Option[a] = findLeft(f, xs)

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, xs: Chain[a]): Option[a] = match viewLeft(xs) {
        case NoneLeft => None
        case SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `xs` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, xs: Chain[a]): Option[a] = match viewRight(xs) {
        case NoneRight => None
        case SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Chain[Int32] = rangeHelper(b, e, Chain.empty())

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, ac: Chain[Int32]): Chain[Int32] =
        if (b >= e) ac else rangeHelper(b + 1, e, snoc(ac, b))

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): Chain[a] = repeatHelper(a, n, Empty)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(a: a, n: Int32, ac: Chain[a]): Chain[a] =
        if (n <= 0)
            ac
        else {
            let ac1 = cons(a, ac);
            repeatHelper(a, n-1, ac1)
        }

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & e, s: b, xs: Chain[a]): Chain[b] & e = scanLeft(f, s, xs)

    ///
    /// Accumulates the result of applying `f` to `xs` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b & e, s: b, xs: Chain[a]): Chain[b] & e = scanLeftHelper(f, s, xs, singleton(s))

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & e, s: b, xs: Chain[a], ac: Chain[b]): Chain[b] & e = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => {let a = f(s, x); scanLeftHelper(f, a, rs, snoc(ac, a))}
    }

    ///
    /// Accumulates the result of applying `f` to `xs` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b & e, s: b, xs: Chain[a]): Chain[b] & e = scanRightHelper(f, s, xs, singleton(s))

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanRightHelper(f: (a, b) -> b & e, s: b, xs: Chain[a], ac: Chain[b]): Chain[b] & e = match viewRight(xs) {
        case NoneRight => ac
        case SomeRight(rs, x) => {let a = f(x, s); scanRightHelper(f, a, rs, cons(a, ac))}
    }

    ///
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b, xs: Chain[a]): Chain[b] =
        mapHelper(f, xs, Empty)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b, xs: Chain[a], ac: Chain[b]): Chain[b] = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(a, rs) => mapHelper(f, rs, snoc(ac, f(a)))
    }

    ///
    /// Returns the result of applying `f` to every element in `xs` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & e, xs: Chain[a]): Chain[b] & e =
        mapWithIndexHelper(f, xs, 0, Empty)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & e, xs: Chain[a], i: Int32, ac: Chain[b]): Chain[b] & e =
        match viewLeft(xs) {
            case NoneLeft => ac
            case SomeLeft(x, rs) => {let a = f(x, i); mapWithIndexHelper(f, rs, i+1, snoc(ac, a))}
        }

    ///
    /// Returns the result of applying `f` to every element in `xs` and concatenating the results.
    ///
    pub def flatMap(f: a -> Chain[b] & e, xs: Chain[a]): Chain[b] & e = flatMapHelper(f, xs, Empty)

    ///
    /// Helper function for `flatMap`.
    ///
    pub def flatMapHelper(f: a -> Chain[b] & e, xs: Chain[a], ac: Chain[b]): Chain[b] & e = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => flatMapHelper(f, rs, append(ac, f(x)))
    }

    ///
    /// Returns the reverse of `xs`.
    ///
    pub def reverse(xs: Chain[a]): Chain[a] = reverseHelper(xs, Empty)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(xs: Chain[a], ac: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => reverseHelper(rs, cons(x, ac))
    }

    ///
    /// Returns `xs` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, xs: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => Empty
        case SomeLeft(x, rs) => intersperseHelper(a, rs, singleton(x))
    }

    ///
    /// Helper function for `reverse`.
    ///
    def intersperseHelper(sep: a, xs: Chain[a], ac: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => intersperseHelper(sep, rs, ac `snoc` sep `snoc` x)
    }

    ///
    /// Returns the number of elements in `xs` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, xs: Chain[a]): Int32 = countHelper(f, xs, 0)

    ///
    /// Helper function for `count`.
    ///
    def countHelper(f: a -> Bool, xs: Chain[a], ac: Int32): Int32 = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => if (f(x)) countHelper(f, rs, ac + 1) else countHelper(f, rs, ac)
    }

    ///
    /// Returns the concatenation of the elements in `xs`.
    ///
    pub def flatten(xs: Chain[Chain[a]]): Chain[a] =
        foldLeft(append, empty(), xs)

    ///
    /// Returns `true` if and only if at least one element in `xs` satisfies the predicate `f`.
    ///
    /// Returns `false` if `xs` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, xs: Chain[a]): Bool = match viewLeft(xs) {
        case NoneLeft => false
        case SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `xs` satisfy the predicate `f`.
    ///
    /// Returns `true` if `xs` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, xs: Chain[a]): Bool = match viewLeft(xs) {
        case NoneLeft => true
        case SomeLeft(x, rs) => if (not f(x)) false else forall(f, rs)
    }

    ///
    /// Returns a list of every element in `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool, xs: Chain[a]): Chain[a] = filterHelper(f, xs, Empty)

    ///
    /// Helper function for `filter`.
    ///
    def filterHelper(f: a -> Bool, xs: Chain[a], ac: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => if (f(x)) filterHelper(f, rs, snoc(ac, x)) else filterHelper(f, rs, ac)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, ac: b, xs: Chain[a]): b & ef = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => {
            let b = f(ac, x);
            foldLeft(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `xs` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, ac: b, xs: Chain[a]): b & ef = match viewRight(xs) {
        case NoneRight => ac
        case SomeRight(rs, x) => {
            let b = f(x, ac);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Returns `xs` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    pub def dropLeft(n: Int32, xs: Chain[a]): Chain[a] = if (n < 0) xs else match (n, viewLeft(xs)) {
        case (_, NoneLeft) => Empty
        case (0, _) => xs
        case (i, SomeLeft(_, rs)) => dropLeft(i - 1, rs)
    }

    ///
    /// Returns `xs` without the last `n` elements.
    ///
    /// Returns `Nil` if `n > length(xs)`.
    /// Returns `xs` if `n < 0`.
    ///
    pub def dropRight(n: Int32, xs: Chain[a]): Chain[a] = if (n < 0) xs else match (n, viewRight(xs)) {
        case (_, NoneRight) => Empty
        case (0, _) => xs
        case (i, SomeRight(rs, _)) => dropRight(i - 1, rs)
    }

    ///
    /// Returns `xs` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileLeft(f: a -> Bool, xs: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => Empty
        case SomeLeft(x, rs) => if (f(x)) dropWhileLeft(f, rs) else xs
    }

    ///
    /// Returns `xs` without the longest suffix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileRight(f: a -> Bool, xs: Chain[a]): Chain[a] = match viewRight(xs) {
        case NoneRight => Empty
        case SomeRight(rs, x) => if (f(x)) dropWhileRight(f, rs) else xs
    }

    ///
    /// Returns the first `n` elements of `xs`.
    ///
    /// Returns `xs` if `n > length(xs)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeLeft(n: Int32, xs: Chain[a]): Chain[a] = takeLeftHelper(n, xs, Empty)

    ///
    /// Helper for function for `takeLeft`.
    ///
    def takeLeftHelper(n: Int32, xs: Chain[a], ac: Chain[a]): Chain[a] =
        if (n < 0) Empty else match (n, viewLeft(xs)) {
            case (_, NoneLeft) => ac
            case (0, _) => ac
            case (i, SomeLeft(x, rs)) => takeLeftHelper(i-1, rs, snoc(ac, x))
        }

    ///
    /// Returns the last `n` elements of `xs`.
    ///
    /// Returns `xs` if `n > length(xs)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeRight(n: Int32, xs: Chain[a]): Chain[a] = takeRightHelper(n, xs, Empty)

    ///
    /// Helper for function for `takeRight`.
    ///
    def takeRightHelper(n: Int32, xs: Chain[a], ac: Chain[a]): Chain[a] =
        if (n < 0) Empty else match (n, viewRight(xs)) {
            case (_, NoneRight) => ac
            case (0, _) => ac
            case (i, SomeRight(rs, x)) => takeRightHelper(i-1, rs, cons(x, ac))
        }

    ///
    /// Returns the longest prefix of `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileLeft(f: a -> Bool, xs: Chain[a]): Chain[a] = takeWhileLeftHelper(f, xs, Empty)

    ///
    /// Helper for function for `takeWhileLeft`.
    ///
    def takeWhileLeftHelper(f: a -> Bool, xs: Chain[a], ac: Chain[a]): Chain[a] = match viewLeft(xs) {
        case NoneLeft => ac
        case SomeLeft(x, rs) => if (f(x)) takeWhileLeftHelper(f, rs, snoc(ac, x)) else ac
    }

    ///
    /// Returns the longest suffix of `xs` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileRight(f: a -> Bool, xs: Chain[a]): Chain[a] = takeWhileRightHelper(f, xs, Empty)

    ///
    /// Helper for function for `takeWhileRight`.
    ///
    def takeWhileRightHelper(f: a -> Bool, xs: Chain[a], ac: Chain[a]): Chain[a] = match viewRight(xs) {
        case NoneRight => ac
        case SomeRight(rs, x) => if (f(x)) takeWhileRightHelper(f, rs, cons(x, ac)) else ac
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `xs`.
    ///
    pub def filterMap(f: a -> Option[b] & e, xs: Chain[a]): Chain[b] & e =
        let step = (ac, x) -> match f(x) {
                                case None => ac
                                case Some(v) => snoc(ac, v)
        };
        foldLeft(step, empty(), xs)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `xs`.
    ///
    /// Returns `None` if every element of `xs` is `None`.
    ///
    pub def findMap(f: a -> Option[b] & e, xs: Chain[a]): Option[b] & e = match viewLeft(xs) {
        case NoneLeft => None
        case SomeLeft(x, rs) => match f(x) {
                            case None => findMap(f, rs)
                            case Some(v) => Some(v)
        }
    }


    ///
    /// Returns a chain where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zip(xs: Chain[a], ys: Chain[b]): Chain[(a,b)] = zipHelper(xs, ys, Chain.empty())

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(xs: Chain[a], ys: Chain[b], ac: Chain[(a,b)]): Chain[(a,b)] = match (viewLeft(xs), viewLeft(ys)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => zipHelper(rs, qs, snoc(ac, (x, y)))
        case _ => ac
    }

    ///
    /// Returns a chain where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `xs` and `b` is the element at index `i` in `ys`.
    ///
    /// If either `xs` or `ys` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zipWith(f: (a, b) -> c & e, xs: Chain[a], ys: Chain[b]): Chain[c] & e = zipWithHelper(f, xs, ys, Chain.empty())

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & e, xs: Chain[a], ys: Chain[b], ac: Chain[c]): Chain[c] & e = match (viewLeft(xs), viewLeft(ys)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => {
            let a = f(x, y);
            zipWithHelper(f, rs, qs, snoc(ac, a))
        }
        case _ => ac
    }

    ///
    /// Returns a pair of chains, the first containing all first components in `xs`
    /// and the second containing all second components in `xs`.
    ///
    pub def unzip(xs: Chain[(a, b)]): (Chain[a], Chain[b]) = unzipHelper(xs, Chain.empty(), Chain.empty())

    ///
    /// Helper function for `unzip`.
    ///
    def unzipHelper(xs: Chain[(a, b)], ac1: Chain[a], ac2: Chain[b]): (Chain[a], Chain[b]) = match viewLeft(xs) {
        case SomeLeft((x1, x2), rs) => unzipHelper(rs, snoc(ac1, x1), snoc(ac2, x2))
        case NoneLeft => (ac1, ac2)
    }

    ///
    /// Returns `xs` as a list.
    ///
    pub def toList(xs: Chain[a]): List[a] =
        foldRight((x,ac) -> x :: ac, Nil, xs)

    ///
    /// Returns the list `xs` as a chain.
    ///
    pub def fromList(xs: List[a]): Chain[a] =
        List.foldLeft(snoc, empty(), xs)

    ///
    /// Returns `xs` as a mutable list.
    ///
    pub def toMut(xs: Chain[a]): MutList[a] & Impure =
        MutList(ref toArray(xs), ref length(xs))

    ///
    /// Returns the list `xs` as a set.
    ///
    pub def toSet[a: Order](xs: Chain[a]): Set[a] = foldRight(Set.insert, Set.empty(), xs)


    ///
    /// Returns the association list `xs` as a map.
    ///
    /// If `xs` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap[a: Order, b](xs: Chain[(a, b)]): Map[a, b] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), xs)


    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: a ~> Unit, xs: Chain[a]): Unit & Impure = match viewLeft(xs) {
        case NoneLeft => ()
        case SomeLeft(x, rs) => f(x); foreach(f, rs)
    }

    ///
    /// Returns the chain `xs` as an array.
    ///
    pub def toArray(xs: Chain[a]) : Array[a] & Impure = match head(xs) {
        case None => []
        case Some(x) =>
            let a = Array.new(x, length(xs));
            let f = (i,b) -> { a[i] = b; i + 1 };
            foldLeft(f, 0, xs);
            a
        }

    ///
    /// Returns `true` if and only if `xs` and `ys` and equal.
    ///
    pub def equals[a: Eq](xs: Chain[a], ys: Chain[a]): Bool = equalsHelper(viewLeft(xs), viewLeft(ys))

    ///
    /// Helper function for `equals`.
    ///
    /// Note: Chains are considered equal if their (ordered) list of elements are equal.
    ///
    /// Because they may have different shapes due to construction we use a view to
    /// decide equality which imposes an order on the Chain.
    ///
    def equalsHelper[a: Eq](vx: ViewLeft[a], vy: ViewLeft[a]): Bool = match (vx, vy) {
        case (NoneLeft, NoneLeft) => true
        case (SomeLeft(x, xs), SomeLeft(y, ys)) =>
            if (x == y)
                equalsHelper(viewLeft(xs), viewLeft(ys))
            else
                false
        case (_, _) => false
    }

}