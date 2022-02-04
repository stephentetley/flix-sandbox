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

///
/// The NonEmptyChain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `NecOne` and `Nec` should not be used directly.
///
pub enum Nec[t] {
    case NecOne(t)
    case Nec(Nec[t], Nec[t])
}
/*
instance Eq[Nec[a]] with Eq[a] {
    pub def eq(c1: Nec[a], c2: Nec[a]): Bool = Nec.equals(c1, c2)
}

instance SemiGroup[Nec[a]] {
    pub def combine(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec.append(c1, c2)
}

instance Monoid[Nec[a]] with Monoid[a] {
    pub def empty(): Nec[a] = Nec.singleton(Monoid.empty())
}

instance Functor[Nec] {
    pub def map(f: a -> b & ef, c: Nec[a]): Nec[b] & ef = Nec.map(f, c)
}

instance Applicative[Nec] {
    pub def point(x: a) : Nec[a] = Nec.singleton(x)
    pub def ap(f: Nec[a -> b & ef], x: Nec[a]) : Nec[b] & ef = Nec.ap(f, x)
}

instance Monad[Nec] {
    pub def flatMap(f: a -> Nec[b] & ef, x : Nec[a]) : Nec[b] & ef = Nec.flatMap(f, x)
}

instance Foldable[Nec] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, c: Nec[a]): b & ef = Nec.foldLeft(f, s, c)
    pub def foldRight(f: (a, b) -> b & ef, s: b, c: Nec[a]): b & ef = Nec.foldRight(f, s, c)
}

instance Traversable[Nec] {
    pub def traverse(f: a -> m[b] & ef, t: Nec[a]): m[Nec[b]] & ef with Applicative[m] = Nec.traverse(f, t)
    pub override def sequence(t: Nec[m[a]]): m[Nec[a]] with Applicative[m] = Nec.sequence(t)
}

instance ToString[Nec[a]] with ToString[a] {
    pub def toString(c: Nec[a]): String =
        let fn = () -> {
            let sb = StringBuilder.new();
            StringBuilder.appendString!(sb, "Nec#{");
            Nec.foldLeft((acc, x) -> {
                if (acc < 1)
                    StringBuilder.appendString!(sb, "${x}")
                else
                    StringBuilder.appendString!(sb, ", ${x}");
                acc + 1}, 0, c);
            StringBuilder.appendString!(sb, "}");
            StringBuilder.toString(sb)
        };
        fn() as & Pure
}
*/

namespace Nec {

    ///
    /// A datatype for pattern matching on a Nec (traversing left-to-right).
    ///
    pub enum NecViewLeft[a] with Eq {
        case OneLeft(a)
        case NecSomeLeft(a, Nec[a])
    }

    ///
    /// A datatype for pattern matching on a Nec (traversing right-to-left).
    ///
    pub enum ViewRight[a] with Eq {
        case OneRight(a)
        case NecSomeRight(Nec[a], a)
    }

    ///
    /// Return the singleton Nec with element `x`.
    ///
    pub def singleton(x: a): Nec[a] = NecOne(x)

    ///
    /// Apply every function from `f` to every argument from `x` and return a Nec with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Nec[a -> b & ef], x: Nec[a]) : Nec[b] & ef =
        apHelper(viewLeft(f), x, empty())

    ///
    /// Helper function for `ap`.
    ///
    def apHelper(f: ViewLeft[a -> b & ef], c: Nec[a], acc: Nec[b]): Nec[b] & ef = match f {
        case NoneLeft         => acc
        case SomeLeft(f1, rs) => apHelper(viewLeft(rs), c, acc `append` map(f1, c))
    }

    ///
    /// Returns true if and only if `c` is a single element Nec.
    ///
    pub def isSingleton(c: Nec[a]): Bool = match c {
        case NecOne(_) => true
        case _         => false
    }

    ///
    /// Add element `x` to the left end of Nec `c`.
    ///
    pub def cons(x: a, c: Nec[a]): Nec[a] = match c {
        case _     => Nec(Single(x), c)
    }

    ///
    /// Add element `x` to the right end of Nec `c`.
    ///
    pub def snoc(c: Nec[a], x: a): Nec[a] = match c {
        case Empty => Single(x)
        case _     => Nec(c, One(x))
    }

    ///
    /// Returns the first element of `c`.
    ///
    pub def head(c: Nec[a]): a = match viewLeft(c) {
        case NecSomeLeft(x, _) => x
        case OneLeft(x)        => x
    }

    ///
    /// Returns the last element of `c`.
    ///
    pub def last(c: Nec[a]): a = match viewRight(c) {
        case NecSomeRight(_, x) => x
        case OneRight(x)        => x
    }

    ///
    /// Returns the sub-Nec of `c` without the last element.
    /// Returns `None` if the Nec `c` has a single element.
    ///
    pub def init(c: Nec[a]): Option[Nec[a]] = match viewRight(c) {
        case NecSomeRight(rs, _) => Some(rs)
        case _                   => None
    }

    ///
    /// Returns the length of `c`.
    ///
    pub def length(c: Nec[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns a new Nec formed by appending the Necs `c1` and `c2`.
    ///
    pub def append(c1: Nec[a], c2: Nec[a]): Nec[a] = match (c1, c2) {
        case (Empty, c) => c
        case (c, Empty) => c
        case _          => Nec(c1, c2)
    }

    ///
    /// Deconstruct a Nec from left-to-right.
    ///
    /// Returns `ViewLeft(x, rs)` if the Nec is non-empty, where `x` is the leftmost
    /// element of the Nec `c`, and `rs` is the rest of the Nec.
    ///
    /// Returns `NoneLeft` if the Nec is empty.
    ///
    pub def viewLeft(c: Nec[a]): ViewLeft[a] = viewLeftHelper(c, Empty, x -> x)

    ///
    /// Helper function for `viewLeft`.
    ///
    /// This traverses the Nec from left-to-right, because the Nec is represented
    /// by a binary tree, the function may have recursively descend the tree to find
    /// the leftmost node.
    ///
    /// Precondition: `c` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewLeftHelper(c: Nec[a], acc: Nec[a], k: ViewLeft[a] -> ViewLeft[a]): ViewLeft[a] = match c {
        case Empty       => k(NoneLeft)
        case One(x)      => k(SomeLeft(x, acc))
        case Nec(l, r) => viewLeftHelper(l, append(r, acc),  k)
    }

    ///
    /// Deconstruct a Nec from right-to-left.
    ///
    /// Returns `ViewRight(rs, x)` if the Nec is non-empty, where `x` is the rightmost
    /// element of the Nec `c``, and `rs` is the front of the Nec.
    ///
    /// Returns `NoneRight` if the Nec is empty.
    ///
    pub def viewRight(c: Nec[a]): ViewRight[a] = viewRightHelper(c, Empty, x -> x)

    ///
    /// Helper function for `viewRight`.
    ///
    /// This traverses the Nec from right-to-left, because the Nec is represented
    /// by a binary tree, the function may have recursively descend the tree to find
    /// the rightmost node.
    ///
    /// Precondition: `c` has been properly formed with no internal `Empty` nodes.
    /// This means that `Empty` can only be encountered on the first match and it
    /// can ignore the accumulator which is also `Empty` the first time this is called.
    ///
    def viewRightHelper(c: Nec[a], acc: Nec[a], k: ViewRight[a] -> ViewRight[a]): ViewRight[a] = match c {
        case Empty       => k(NoneRight)
        case One(x)      => k(SomeRight(acc, x))
        case Nec(l, r) => viewRightHelper(r, append(acc, l),  k)
    }

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Nec[a]): Bool with Eq[a] = memberOfHelper(a, viewLeft(c))

    ///
    /// Helper function for `memberOf`.
    ///
    def memberOfHelper(a: a, v: ViewLeft[a]): Bool with Eq[a] = match v {
        case NoneLeft                 => false
        case SomeLeft(x, _) if x == a => true
        case SomeLeft(_, c)           => memberOfHelper(a, viewLeft(c))
    }

    ///
    /// Returns the position of `a` in `c`.
    ///
    /// Returns `-1` if `a` does not exist in `c`.
    ///
    pub def indexOf(a: a, c: Nec[a]): Int32 with Eq[a] = indexOfHelper(a, 0, viewLeft(c))

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, acc: Int32, v: ViewLeft[a]): Int32 with Eq[a] = match v {
        case NoneLeft       => -1
        case SomeLeft(x, c) => if (x == a) acc else indexOfHelper(a, acc + 1, viewLeft(c))
    }

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, c: Nec[a]): Option[a] = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, c: Nec[a]): Option[a] = match viewLeft(c) {
        case OneLeft(x)         => f(x)
        case NecSomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, c: Nec[a]): Option[a] = match viewRight(c) {
        case OneRight(x)         => f(x)
        case NecSomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Nec[Int32] = rangeHelper(b, e, Nec.empty())

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, acc: Nec[Int32]): Nec[Int32] =
        if (b >= e) acc else rangeHelper(b + 1, e, snoc(acc, b))

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(n: Int32, a: a): Nec[a] = repeatHelper(n, a, Empty)

    ///
    /// Helper function for `repeat`.
    ///
    def repeatHelper(n: Int32, a: a, acc: Nec[a]): Nec[a] =
        if (n <= 0)
            acc
        else {
            let ac1 = cons(a, acc);
            repeatHelper(n-1, a, ac1)
        }

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b & ef, s: b, c: Nec[a]): Nec[b] & ef = scanLeft(f, s, c)

    ///
    /// Accumulates the result of applying `f` to `c` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b & ef, s: b, c: Nec[a]): Nec[b] & ef = scanLeftHelper(f, s, c, singleton(s))

    ///
    /// Helper function for `scanLeft`.
    ///
    def scanLeftHelper(f: (b, a) -> b & ef, s: b, c: Nec[a], acc: Nec[b]): Nec[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {let a = f(s, x); scanLeftHelper(f, a, rs, snoc(acc, a))}
    }

    ///
    /// Accumulates the result of applying `f` to `c` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b & ef, s: b, c: Nec[a]): Nec[b] & ef = scanRightHelper(f, s, c, singleton(s))

    ///
    /// Helper function for `scanRight`.
    ///
    def scanRightHelper(f: (a, b) -> b & ef, s: b, c: Nec[a], acc: Nec[b]): Nec[b] & ef = match viewRight(c) {
        case NoneRight        => acc
        case SomeRight(rs, x) => {let a = f(x, s); scanRightHelper(f, a, rs, cons(a, acc))}
    }

    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b & ef, c: Nec[a]): Nec[b] & ef =
        mapHelper(f, c, Empty)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b & ef, c: Nec[a], acc: Nec[b]): Nec[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(a, rs) => mapHelper(f, rs, snoc(acc, f(a)))
    }

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (a, Int32) -> b & ef, c: Nec[a]): Nec[b] & ef =
        mapWithIndexHelper(f, c, 0, Empty)

    ///
    /// Helper function for `mapWithIndex`.
    ///
    def mapWithIndexHelper(f: (a, Int32) -> b & ef, c: Nec[a], i: Int32, acc: Nec[b]): Nec[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {let a = f(x, i); mapWithIndexHelper(f, rs, i+1, snoc(acc, a))}
    }

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Nec[b] & ef, c: Nec[a]): Nec[b] & ef = flatMapHelper(f, c, Empty)

    ///
    /// Helper function for `flatMap`.
    ///
    def flatMapHelper(f: a -> Nec[b] & ef, c: Nec[a], acc: Nec[b]): Nec[b] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => flatMapHelper(f, rs, append(acc, f(x)))
    }

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Nec[a]): Nec[a] = reverseHelper(c, Empty)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(c: Nec[a], acc: Nec[a]): Nec[a] = match viewLeft(c) {
        case OneLeft(x)         => cons(x, acc)
        case NecSomeLeft(x, rs) => reverseHelper(rs, cons(x, acc))
    }

/*
    ///
    /// Returns `c` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, c: Nec[a]): Nec[a] = match viewLeft(c) {
        case NoneLeft        => Empty
        case SomeLeft(x, rs) => intersperseHelper(a, rs, singleton(x))
    }

    ///
    /// Helper function for `reverse`.
    ///
    def intersperseHelper(sep: a, c: Nec[a], acc: Nec[a]): Nec[a] = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => intersperseHelper(sep, rs, acc `snoc` sep `snoc` x)
    }
*/
    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: a -> Bool, c: Nec[a]): Int32 = countHelper(f, c, 0)

    ///
    /// Helper function for `count`.
    ///
    def countHelper(f: a -> Bool, c: Nec[a], acc: Int32): Int32 = match viewLeft(c) {
        case OneLeft(_)         => acc + 1
        case NecSomeLeft(x, rs) => if (f(x)) countHelper(f, rs, acc + 1) else countHelper(f, rs, acc)
    }
/*
    ///
    /// Returns the sum of all elements in the Nec `c`.
    ///
    pub def sum(c: Nec[Int32]): Int32 =
        Foldable.sum(c)

    ///
    /// Returns the sum of all elements in the Nec `c` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 & ef, c: Nec[a]): Int32 & ef =
        Foldable.sumWith(f, c)

    ///
    /// Returns the product of all elements in the list `l`.
    ///
    pub def product(c: Nec[Int32]): Int32 =
        Foldable.product(c)

    ///
    /// Returns the product of all elements in the list `l` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 & ef, c: Nec[a]): Int32 & ef =
        Foldable.productWith(f, c)

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Nec[Nec[a]]): Nec[a] =
        foldLeft(append, empty(), c)

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: a -> Bool, c: Nec[a]): Bool = match viewLeft(c) {
        case NoneLeft        => false
        case SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: a -> Bool, c: Nec[a]): Bool = match viewLeft(c) {
        case NoneLeft        => true
        case SomeLeft(x, rs) => if (not f(x)) false else forall(f, rs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool & ef, c: Nec[a]): Nec[a] & ef = filterHelper(f, c, Empty)

    ///
    /// Helper function for `filter`.
    ///
    def filterHelper(f: a -> Bool & ef, c: Nec[a], acc: Nec[a]): Nec[a] & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => if (f(x)) filterHelper(f, rs, snoc(acc, x)) else filterHelper(f, rs, acc)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b & ef, acc: b, c: Nec[a]): b & ef = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => {
            let b = f(acc, x);
            foldLeft(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b & ef, acc: b, c: Nec[a]): b & ef = match viewRight(c) {
        case NoneRight        => acc
        case SomeRight(rs, x) => {
            let b = f(x, acc);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Returns `c` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropLeft(n: Int32, c: Nec[a]): Nec[a] =
        if (n < 0)
            c
        else match (n, viewLeft(c)) {
            case (_, NoneLeft)        => Empty
            case (0, _)               => c
            case (i, SomeLeft(_, rs)) => dropLeft(i - 1, rs)
        }

    ///
    /// Returns `c` without the last `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropRight(n: Int32, c: Nec[a]): Nec[a] =
        if (n < 0)
            c
        else match (n, viewRight(c)) {
            case (_, NoneRight)        => Empty
            case (0, _)                => c
            case (i, SomeRight(rs, _)) => dropRight(i - 1, rs)
        }

    ///
    /// Returns `c` without the longest prefix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileLeft(f: a -> Bool, c: Nec[a]): Nec[a] = match viewLeft(c) {
        case SomeLeft(x, rs) if (f(x)) => dropWhileLeft(f, rs)
        case _                         => c
    }

    ///
    /// Returns `c` without the longest suffix that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def dropWhileRight(f: a -> Bool, c: Nec[a]): Nec[a] = match viewRight(c) {
        case SomeRight(rs, x) if (f(x)) => dropWhileRight(f, rs)
        case _                          => c
    }

    ///
    /// Returns the first `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeLeft(n: Int32, c: Nec[a]): Nec[a] =
        if (n < 0)
            Empty
        else
            takeLeftHelper(n, c, Empty)

    ///
    /// Helper for function for `takeLeft`.
    ///
    /// Precondition n >= 0
    ///
    def takeLeftHelper(n: Int32, c: Nec[a], acc: Nec[a]): Nec[a] = match (n, viewLeft(c)) {
        case (_, NoneLeft)        => acc
        case (i, _) if i < 1      => acc
        case (i, SomeLeft(x, rs)) => takeLeftHelper(i-1, rs, snoc(acc, x))
    }

    ///
    /// Returns the last `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeRight(n: Int32, c: Nec[a]): Nec[a] =
        if (n < 0)
            Empty
        else
            takeRightHelper(n, c, Empty)

    ///
    /// Helper for function for `takeRight`.
    ///
    /// Precodition: n >= 0
    ///
    def takeRightHelper(n: Int32, c: Nec[a], acc: Nec[a]): Nec[a] = match (n, viewRight(c)) {
        case (_, NoneRight)        => acc
        case (i, _)  if i < 1      => acc
        case (i, SomeRight(rs, x)) => takeRightHelper(i-1, rs, cons(x, acc))
    }

    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileLeft(f: a -> Bool, c: Nec[a]): Nec[a] = takeWhileLeftHelper(f, c, Empty)

    ///
    /// Helper for function for `takeWhileLeft`.
    ///
    def takeWhileLeftHelper(f: a -> Bool, c: Nec[a], acc: Nec[a]): Nec[a] = match viewLeft(c) {
        case NoneLeft        => acc
        case SomeLeft(x, rs) => if (f(x)) takeWhileLeftHelper(f, rs, snoc(acc, x)) else acc
    }

    ///
    /// Returns the longest suffix of `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def takeWhileRight(f: a -> Bool, c: Nec[a]): Nec[a] = takeWhileRightHelper(f, c, Empty)

    ///
    /// Helper for function for `takeWhileRight`.
    ///
    def takeWhileRightHelper(f: a -> Bool, c: Nec[a], acc: Nec[a]): Nec[a] = match viewRight(c) {
        case SomeRight(rs, x) if (f(x)) => takeWhileRightHelper(f, rs, cons(x, acc))
        case _                          => acc
    }

    ///
    /// Collects the results of applying the partial function `f` to every element in `c`.
    ///
    pub def filterMap(f: a -> Option[b] & ef, c: Nec[a]): Nec[b] & ef =
        let step = (acc, x) -> match f(x) {
            case None    => acc
            case Some(v) => snoc(acc, v)
        };
        foldLeft(step, empty(), c)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if every element of `c` is `None`.
    ///
    pub def findMap(f: a -> Option[b] & ef, c: Nec[a]): Option[b] & ef = match viewLeft(c) {
        case NoneLeft        => None
        case SomeLeft(x, rs) => match f(x) {
            case None    => findMap(f, rs)
            case Some(v) => Some(v)
        }
    }


    ///
    /// Returns a Nec where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zip(c1: Nec[a], c2: Nec[b]): Nec[(a,b)] = zipHelper(c1, c2, Nec.empty())

    ///
    /// Helper function for `zip`.
    ///
    def zipHelper(c1: Nec[a], c2: Nec[b], acc: Nec[(a,b)]): Nec[(a,b)] = match (viewLeft(c1), viewLeft(c2)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => zipHelper(rs, qs, snoc(acc, (x, y)))
        case _                                  => acc
    }

    ///
    /// Returns a Nec where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zipWith(f: (a, b) -> c & ef, c1: Nec[a], c2: Nec[b]): Nec[c] & ef = zipWithHelper(f, c1, c2, Nec.empty())

    ///
    /// Helper function for `zipWith`.
    ///
    def zipWithHelper(f: (a, b) -> c & ef, c1: Nec[a], c2: Nec[b], acc: Nec[c]): Nec[c] & ef = match (viewLeft(c1), viewLeft(c2)) {
        case (SomeLeft(x, rs), SomeLeft(y, qs)) => {
            let a = f(x, y);
            zipWithHelper(f, rs, qs, snoc(acc, a))
        }
        case _                                  => acc
    }

    ///
    /// Returns a pair of Necs, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Nec[(a, b)]): (Nec[a], Nec[b]) = unzipHelper(c, Nec.empty(), Nec.empty())

    ///
    /// Helper function for `unzip`.
    ///
    def unzipHelper(c: Nec[(a, b)], ac1: Nec[a], ac2: Nec[b]): (Nec[a], Nec[b]) = match viewLeft(c) {
        case NoneLeft               => (ac1, ac2)
        case SomeLeft((x1, x2), rs) => unzipHelper(rs, snoc(ac1, x1), snoc(ac2, x2))
    }

    ///
    /// `mapAccumLeft` is a stateful version of `map`. The accumulating paramter `s` is updated at each
    /// step in a left-to-right traversal.
    ///
    pub def mapAccumLeft(f: (s, a) -> (s, b) & ef, start: s, c: Nec[a]): (s, Nec[b]) & ef =
        mapAccumLeftHelper(f, start, c, (s1, ks) -> (s1, ks))

    ///
    /// Helper function for `mapAccumLeft`.
    ///
    def mapAccumLeftHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Nec[a], k: (s, Nec[b]) -> (s, Nec[b])): (s, Nec[b]) & ef =
        match viewLeft(c) {
            case NoneLeft        => k(s1, empty())
            case SomeLeft(x, rs) => {
                let (s2, x1) = f(s1, x);
                mapAccumLeftHelper(f, s2, rs, (s3, ks) -> k(s3, cons(x1, ks)))
            }
        }

    ///
    /// `mapAccumRight` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a right-to-left traversal.
    ///
    pub def mapAccumRight(f: (s, a) -> (s, b) & ef, start: s, c: Nec[a]): (s, Nec[b]) & ef =
        mapAccumRightHelper(f, start, c, (s1, ks) -> (s1, ks))

    ///
    /// Helper function for `mapAccumRight`.
    ///
    def mapAccumRightHelper(f: (s, a) -> (s, b) & ef, s1: s, c: Nec[a], k: (s, Nec[b]) -> (s, Nec[b])): (s, Nec[b]) & ef =
        match viewRight(c) {
            case NoneRight        => k(s1, empty())
            case SomeRight(rs, x) => {
                let (s2, x1) = f(s1, x);
                mapAccumRightHelper(f, s2, rs, (s3, ks) -> k(s3, snoc(ks, x1)))
           }
        }

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def foreach(f: a -> Unit & ef, c: Nec[a]): Unit & ef = match viewLeft(c) {
        case NoneLeft        => ()
        case SomeLeft(x, rs) => f(x); foreach(f, rs)
    }

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Nec[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Returns `c` as a mutable list.
    ///
    pub def toMutList(c: Nec[a]): MutList[a] & Impure =
        MutList(ref toArray(c), ref length(c))

    ///
    /// Returns the list `c` as a set.
    ///
    pub def toSet(c: Nec[a]): Set[a] with Order[a] = foldRight(Set.insert, Set.empty(), c)


    ///
    /// Returns the Nec of pairs `c` that represents an association list as a map.
    ///
    /// If `c` contains multiple mappings with the same key, `toMap` does not
    /// make any guarantees about which mapping will be in the resulting map.
    ///
    pub def toMap(c: Nec[(a, b)]): Map[a, b] with Order[a] = foldRight((x, m) -> Map.insert(fst(x), snd(x), m), Map.empty(), c)


    ///
    /// Returns the Nec `c` as an array.
    ///
    pub def toArray(c: Nec[a]) : Array[a] & Impure = match head(c) {
        case None => []
        case Some(x) =>
            let arr = Array.new(x, length(c));
            let f = (i, b) -> { arr[i] = b; i + 1 };
            foldLeft(f, 0, c);
            arr
        }

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(c1: Nec[a], c2: Nec[a]): Bool with Eq[a] = equalsHelper(viewLeft(c1), viewLeft(c2))

    ///
    /// Helper function for `equals`.
    ///
    /// Note: Necs are considered equal if their (ordered) list of elements are equal.
    ///
    /// Because they may have different shapes due to construction we use a view to
    /// decide equality which imposes an order on the Nec.
    ///
    def equalsHelper(v1: ViewLeft[a], v2: ViewLeft[a]): Bool with Eq[a] = match (v1, v2) {
        case (NoneLeft, NoneLeft)                           => true
        case (SomeLeft(x, cx), SomeLeft(y, cy)) if (x == y) => equalsHelper(viewLeft(cx), viewLeft(cy))
        case _                                              => false
    }

    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to their `Order` instance.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sort(c: Nec[a]): Nec[a] with Order[a] =
        (c |> toArray |> Array.sort |> Array.toNec) as & Pure

    /// Sort Nec `c` so that elements are ordered from low to high according to the `Order` instance
    /// for the values obtained by applying `f` to each element.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortBy(f: a -> b, c: Nec[a]): Nec[a] with Order[b] =
        (c |> toArray |> Array.sortBy(f) |> Array.toNec) as & Pure

    ///
    /// Sort Nec `c` so that elements are ordered from low to high according to the comparison function `cmp`.
    ///
    /// The sort is not stable, i.e., equal elements may appear in a different order than in the input `c`.
    ///
    /// The sort implementation is a Quicksort.
    ///
    pub def sortWith(cmp: (a,a) -> Comparison, c: Nec[a]): Nec[a] =
        (c |> toArray |> Array.sortWith(cmp) |> Array.toNec) as & Pure

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative Nec" from an applicative Nec of the front of the Nec
    /// "snocing" a last element of one applictive action.
    ///
    def snocA(mxs: f[Nec[a]], mx: f[a]): f[Nec[a]] with Applicative[f] =
        (((xs, x) -> snoc(xs, x)) `Functor.map` mxs) `Applicative.ap` mx

    ///
    /// Returns the result of running all the actions in the Nec `c`.
    ///
    pub def sequence(c: Nec[m[a]]): m[Nec[a]] with Applicative[m] =
        def loop(v, acc) = match v {
            case NoneLeft        => acc
            case SomeLeft(x, rs) => loop(viewLeft(rs), snocA(acc, x))
        };
        loop(viewLeft(c), Applicative.point(empty()))

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// Nec `c`.
    ///
    pub def traverse(f: a -> m[b] & ef, c: Nec[a]): m[Nec[b]] & ef with Applicative[m] =
        def loop(v, acc) = match v {
            case NoneLeft        => acc as & ef
            case SomeLeft(x, rs) => loop(viewLeft(rs), snocA(acc, f(x)))
        };
        loop(viewLeft(c), Applicative.point(empty()))

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `c` with `sep` inserted between each element.
    ///
    pub def join(sep: String, c: Nec[a]): String with ToString[a] =
        Foldable.join(sep, c)

    ///
    /// Returns the concatenation of the string representation
    /// of each element in `c` according to `f` with `sep` inserted between each element.
    ///
    pub def joinWith(f: a -> String & ef, sep: String, c: Nec[a]): String =
        Foldable.joinWith(f, sep, c)
*/
}
