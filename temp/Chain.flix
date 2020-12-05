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
    pub def head(xs: Chain[a]): Option[a] = headHelper(xs, x -> x)

    ///
    /// Helper function for `head`.
    ///
    /// Precondition: `xs` has been properly formed with any concatenation using `append`.
    /// This means there will be no internal `Empty` nodes.
    ///
    def headHelper(xs: Chain[a], k: Option[a] -> Option[a]): Option[a] = match xs {
        case Empty => k(None)
        case One(x) => k(Some(x))
        case Chain(ls, _) => headHelper(ls, k)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `xs`.
    ///
    /// Returns `None` if `xs` is empty.
    ///
    pub def last(xs: Chain[a]): Option[a] = lastHelper(xs, x -> x)

    ///
    /// Helper function for `last`.
    ///
    def lastHelper(xs: Chain[a], k: Option[a] -> Option[a]): Option[a] = match xs {
        case Empty => k(None)
        case One(x) => k(Some(x))
        case Chain(_, rs) => lastHelper(rs, k)
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
    pub def memberOf(a: a, xs: Chain[a]): Bool = memberOfHelper(a, xs, x -> x)

    ///
    /// Helper function for `memberOf`.
    ///
    def memberOfHelper(a: a, xs: Chain[a], k: Bool -> Bool): Bool = match xs {
        case Empty => k(false)
        case One(x) => k(x == a)
        case Chain(ls, rs) => memberOfHelper(a, ls, ans -> if (ans) k(true) else memberOfHelper(a, rs, k))
    }

    ///
    /// Returns the position of `a` in `xs`.
    ///
    /// Returns `-1` if `a` does not exist in `xs`.
    ///
    pub def indexOf(a: a, xs: Chain[a]): Int32 = indexOfHelper(a, 0, viewLeft(xs), x -> x)

    ///
    /// Helper function for `indexOf`.
    ///
    def indexOfHelper(a: a, ac: Int32, vx: ViewLeft[a], k: Int32 -> Int32): Int32 = match vx {
        case NoneLeft => k(-1)
        case SomeLeft(x, xs) => if (x == a) k(ac) else indexOfHelper(a, ac + 1, viewLeft(xs), k)
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
    pub def range(b: Int32, e: Int32): Chain[Int32] = rangeHelper(b, e, xs -> xs)

    ///
    /// Helper function for `range`.
    ///
    def rangeHelper(b: Int32, e: Int32, k: Chain[Int32] -> Chain[Int32]): Chain[Int32] =
        if (b >= e) k(Empty) else rangeHelper(b + 1, e, xs -> k(cons(b,xs)))

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
    /// Returns the result of applying `f` to every element in `xs`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b, t: Chain[a]): Chain[b] =
        mapHelper(f, t, xs -> xs)

    ///
    /// Helper function for `map`.
    ///
    def mapHelper(f: a -> b, t: Chain[a], k: Chain[b] -> Chain[b]): Chain[b] = match t {
        case Empty => k(Empty)
        case One(a) => k(One(f(a)))
        case Chain(xs, ys) => mapHelper(f, xs, vs1 -> mapHelper(f, ys, vs2 -> k(Chain(vs1, vs2))))
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

    pub def toList(xs: Chain[a]): List[a] = toListHelper(xs, Nil, ks -> ks)

    ///
    /// Helper for `toList` in CPS.
    ///
    def toListHelper(xs: Chain[a], ac:List[a], k: List[a] -> List[a]): List[a] =
        match xs {
            case Empty => k(ac)
            case One(a) => k(a :: ac)
            case Chain(l, r) =>
                // Go Right first
                toListHelper(r, ac, ac1 ->
                    toListHelper(l, ac1, k))
        }

    pub def fromList(xs: List[a]): Chain[a] =
        List.foldRight(cons, empty(), xs)

    pub def concat(xss: List[Chain[a]]): Chain[a] =
        List.foldLeft(append, empty(), xss)

    ///
    /// Right-associative fold of a Chain.
    ///
    pub def foldRight(f: (a, b) -> b, ac: b, xs: Chain[a]): b =
        foldRightHelper(f, ac, xs, identity)

    ///
    /// Helper for `foldRight` in CPS.
    ///
    def foldRightHelper(f: (a, b) -> b, ac: b, xs: Chain[a], k: b -> b): b = match xs {
        case Empty => k(ac)
        case One(a) => k(f(a, ac))
        case Chain(x,y) =>
            foldRightHelper(f, ac, y, ac1 -> foldRightHelper(f, ac1, x, ac2 -> k(ac2)))
    }


    ///
    /// Left-associative fold of a Chain.
    ///
    pub def foldLeft(f: (b, a) -> b, ac: b, xs: Chain[a]): b =
        foldLeftHelper(f, ac, xs, identity)

    ///
    /// Helper for `foldLeft` in CPS.
    ///
    def foldLeftHelper(f: (b, a) -> b, ac: b, xs: Chain[a], k: b -> b): b = match xs {
        case Empty => k(ac)
        case One(a) => k(f(ac, a))
        case Chain(x,y) =>
            foldLeftHelper(f, ac, x, ac1 -> foldLeftHelper(f, ac1, y, ac2 -> k(ac2)))
    }


    ///
    /// Returns `true` if and only if `xs` and `ys` and equal.
    ///
    pub def __eq(xs: Chain[a], ys: Chain[a]): Bool = eqHelper(viewLeft(xs), viewLeft(ys), x -> x)

    ///
    /// Helper function for `__eq`.
    ///
    /// Note: Chains are considered equal if their (ordered) list of elements are equal.
    ///
    /// Because they may have different shapes due to construction we use a view to
    /// decide equality which imposes an order on the Chain.
    ///
    def eqHelper(vx: ViewLeft[a], vy: ViewLeft[a], k: Bool -> Bool): Bool = match (vx, vy) {
        case (NoneLeft, NoneLeft) => k(true)
        case (SomeLeft(x, xs), SomeLeft(y, ys)) =>
            if (x == y)
                eqHelper(viewLeft(xs), viewLeft(ys), k)
            else
                k(false)
        case (_, _) => k(false)
    }

}