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

namespace Chain {

    pub def empty(): Chain[a] = Empty

    pub def singleton(x: a): Chain[a] = One(x)


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

    pub def cons(x: a, xs: Chain[a]): Chain[a] = match xs {
        case Empty => One(x)
        case _ => Chain(One(x), xs)
    }

    pub def snoc(xs: Chain[a], x: a): Chain[a] = match xs {
        case Empty => One(x)
        case _ => Chain(xs, One(x))
    }

    pub def toList(a: Chain[a]): List[a] = toListHelper(a, Nil, identity)

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
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(a: a, n: Int32): Chain[a] = repeatHelper(a, n, Empty)

    def repeatHelper(a: a, n: Int32, ac: Chain[a]): Chain[a] =
        if (n <= 0)
            ac
        else {
            let ac1 = cons(a, ac);
            repeatHelper(a, n-1, ac1)
        }

    pub def map(f: a -> b, t: Chain[a]): Chain[b] =
        mapHelper(f, t, identity)

    ///
    /// Helper for `map` in CPS.
    ///
    def mapHelper(f: a -> b, t: Chain[a], k: Chain[b] -> Chain[b]): Chain[b] = match t {
        case Empty => k(Empty)
        case One(a) => k(One(f(a)))
        case Chain(xs, ys) => mapHelper(f, xs, vs1 -> mapHelper(f, ys, vs2 -> k(Chain(vs1, vs2))))
    }


}